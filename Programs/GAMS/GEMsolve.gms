* GEMsolve.gms


* Last modified by Dr Phil Bishop, 26/04/2016 (emi@ea.govt.nz)


$ontext
  This program continues sequentially from GEMdata. The GEMdata work file (GEMdata.g00) must be called at the invocation
  of GEMsolve. Note that GEMdata was invoked by restarting from the GEMdeclarations work file. Hence, GEMsolve embodies
  all that is declared and/or initialised within GEMdeclarations and GEMdata. GEMsolve may be followed either by another
  invocation of GEMdata/GEMsolve - i.e. if there are to be multiple run versions, or by runGEMreports.

  TODO:
  1. Make sure that each model type has the correct modelstat error condition driving the abort statements. The assumption is that
     DISP is solved as an RMIP so the solver reporting related to MIPs is skipped for DISP. Is this o.k.? Should we force, perhaps
     in EMI, DISP to never be an LP. As an RMIP, it's an LP anyway. Allowing LP as a solve type requires more modelstat error conditions.
  2. The 'abort if slacks are present' statement has been removed. Perhaps it should be reinstated? Add a warning if penalties
     are present?
  3. Code at very end of file relates to counting integer solutions from log file. Decide whether to keep this or ditch it. 
  4. Users should be aware that GEMdata may have changed the data associated with the following symbols in the input data:
       Sets: y and exist.
       Parameters: i_fixedOM, i_txCapacity, i_txCapacityPO and i_txEarlyComYr.
  5. ...

 Code sections:
  1. Take care of preliminaries.
  2. Set bounds, initial levels and, in some cases, fix variables to a specified level.
  3. Loop through all the solves
  4. Dump selected prepared input data into a GDX file and rename/relocate a couple of log files.
$offtext

* Track memory usage.
* Higher numbers are for more detailed information inside loops. Alternatively, on the command line, type: gams xxx profile=1
*option profile = 1 ;
*option profile = 2 ;
*option profile = 3 ;

option seed = 101 ;

* Turn the following stuff on/off as desired.
$offinline offeolcom
$inlinecom { } eolcom !
$offupper onempty
$offuelxref offuellist	
$offsymxref offsymlist



*===============================================================================================
* 1. Take care of preliminaries.

$include VOLLplant.inc
$include tempSets.inc

* Stamp header for current run/runVersion into GEMsolveReport.
putclose rep 'Run name:' @15 "%runName%" / 'Run version:' @15 "%runVersionName%" / 'Date/time:' @15 system.date, ' - ' system.time / ;

* Specify various .lst file options.
if(%limitOutput% = 1, option limcol = 0, limrow = 0, sysout = off, solprint = off ; ) ; 
*option solprint = on ;

* Specify the MIP/RMIP solver to use.
option MIP = %Solver%, RMIP = %Solver%, LP = %Solver% ;

* Create solver options file on the fly
$onecho > gurobi.opt
  threads     %threads%
$offecho
$onecho > cplex.opt
  threads     %threads%
  mipemphasis  2
  heurfreq     500
  rinsheur     500
  scaind       0
  mipstart     1
  repairtries  5
  lpmethod     2
$offecho
$onecho > xpress.opt
  threads     %threads%
  loadmipsol   0
  mipcleanup   0
  covercuts    1000
  cutdepth     50
  cutfreq      8
  cutstrategy  2
  gomcuts      100
  algorithm    barrier
$offecho

* Specify default solver options.
option reslim = 500, iterlim = 10000000 ;

* Specified various solve settings for each model type.
gem.optfile = 1 ;     gem.optca = 0 ;
gem.optcr = 0.00001 ; gem.reslim = 500 ;
gem.optcr = %optcr% ; gem.reslim = %CPUsecsGEM% ;

disp.reslim = 300 ;   disp.optfile = 0 ;

* Turn on the following to use the GAMSCHK routines (need to also comment out abort statements).
*option MIP=GAMSCHK ;

* Include a 'GR' investment schedule if one is specified correctly.
$if %GRscheduleRead%==0 $goto noGRschedule1
$if not exist "%GRscheduleFile%" $error Specified investment schedule does not exist
$if     exist "%GRscheduleFile%" $include "%GRscheduleFile%"
$label noGRschedule1



*===============================================================================================
* 2. Set bounds, initial levels and, in some cases, fix variables to a specified level.

* Fix CGEN to zero for all years less than cGenYr and fix BGEN to zero for all years greater than or equal to cGenYr.
CGEN.up(g,y) = 1 ;     CGEN.fx(g,y)$( yearNum(y) <  cGenYr ) = 0 ;
BGEN.up(g,y) = 1 ;     BGEN.fx(g,y)$( yearNum(y) >= cGenYr ) = 0 ;

* Restrict refurbishment cost to be zero in years prior to refurbishment.
REFURBCOST.fx(g,y)$( yearNum(y) < i_refurbDecisionYear(g) ) = 0 ;

* Restrict generation:
* Don't allow generation unless the unit is in validYrOperate (validYrOperate embodies the appropriate dates for existing, committed, and new units).
GEN.fx(g,y,t,lb,scen)$( not validYrOperate(g,y) ) = 0 ;
* Force generation from the must-run plant, i.e base load (convert MW capacity to GWh for each load block).
GEN.fx(g,y,t,lb,scen)$( ( exist(g) or commit(g) ) * i_baseload(g) * validYrOperate(g,y) ) =
  1e-3 * hoursPerBlock(t,lb) * i_nameplate(g) * maxCapFactPlant(g,t,lb) ;

* Place restrictions on VOLL plants:
VOLLGEN.up(s,y,t,lb,scen) = 1e-3 * hoursPerBlock(t,lb) * VOLLcap ;   ! Respect the capacity of VOLL plants
VOLLGEN.fx(s,y,t,lb,scen)$( ord(lb) <= noVOLLblks ) = 0 ;            ! Don't allow VOLL in user-specified top load blocks 

* Fix bounds on TX according to the largest capacity allowed in any state. Lower bound must be zero if transportation formulation is being used.
TX.lo(paths,y,t,lb,scen) = -smax(ps, i_txCapacity(paths,ps)) ;
TX.lo(paths,y,t,lb,scen)$(DCloadFlowOn = 0) = 0 ;
TX.up(paths,y,t,lb,scen) = +smax(ps, i_txCapacity(paths,ps)) ;

* Fix the reference bus angle to zero (only used in case of DC load flow formulation).
THETA.fx(slackBus(r),y,t,lb,scen) = 0 ;

* Fix various reserves variables to zero if they are not needed.
RESV.fx(g,rc,y,t,lb,scen)$(            ( not reservesOn ) or ( not reservesCapability(g,rc) ) ) = 0 ;
RESVVIOL.fx(rc,ild,y,t,lb,scen)$(        not reservesOn ) = 0 ;
RESVTRFR.fx(rc,ild,ild1,y,t,lb,scen)$( ( not reservesOn ) or singleReservesReqF(rc) ) = 0 ;
RESVREQINT.fx(rc,ild,y,t,lb,scen)$(      not reservesOn ) = 0 ;
NORESVTRFR.fx(ild,ild1,y,t,lb,scen)$(    not reservesOn ) = 0 ;

* Fix to zero the intra-island reserve variables.
RESVTRFR.fx(rc,ild,ild,y,t,lb,scen) = 0 ;
NORESVTRFR.fx(ild,ild,y,t,lb,scen)  = 0 ;

* Set the lower bound on the reserve requirement if there is an external requirement specified.
RESVREQINT.lo(rc,ild,y,t,lb,scen)$( i_reserveReqMW(y,ild,rc) > 0 ) = i_reserveReqMW(y,ild,rc) * hoursPerBlock(t,lb) ;

* Reserve contribution cannot exceed the specified capability during peak or other periods.
RESV.up(g,rc,y,t,lb,scen)$( reservesOn and reservesCapability(g,rc) ) = reservesCapability(g,rc) * hoursPerBlock(t,lb) ;

* Don't allow reserves from units prior to committed date or earliest allowable operation or if plant is retired.
RESV.fx(g,rc,y,t,lb,scen)$( not validYrOperate(g,y) ) = 0 ;



*===============================================================================================
* 3. Loop through all the solves

* The solve statement is inside a triple nested loop:
*   Outer loop: Experiments
*     Middle loop: Steps, i.e. timing, reopt, or dispatch
*       Inner loop: scenarioSets
* If more than one scenario is mapped to the current scenarioSet, then they're all solved simultaneously, i.e. in a single solve.

$set AddUpSlacks    "sum(y, ANNMWSLACK.l(y) + RENCAPSLACK.l(y) + HYDROSLACK.l(y) + MINUTILSLACK.l(y) + FUELSLACK.l(y) )"
$set AddUpPenalties "sum((y,sc), PEAK_NZ_PENALTY.l(y,sc) + PEAK_NI_PENALTY.l(y,sc) + NOWINDPEAK_NI_PENALTY.l(y,sc) )"

* First, loop through all the experiments
loop(experiments,

* Reset any variables that might have been fixed for an earlier reoptimisation or dispatch solve (reset fixes by initialising .lo and .up fields)
* Restrict the build variable (i.e. MW) to zero or i_nameplate under certain input assumptions:
  BUILD.lo(g,y) = 0 ;    BUILD.up(g,y) = i_nameplate(g) ;             ! Lower bound equals zero, upper bound equals i_nameplate.
  BUILD.fx(g,y)$( not validYrBuild(g,y) ) = 0 ;                       ! Don't allow capacity to be built in years outside the valid range of build years.
  BUILD.fx(g,y)$( commit(g) * validYrBuild(g,y) ) = i_nameplate(g) ;  ! For committed plant, fix the MW able to be built regardless of any other settings.

* Fix retired MW by year and generating plant to zero if not able to be endogenously retired.
  RETIRE.lo(g,y) = 0 ;   RETIRE.up(g,y) = inf ;
  RETIRE.fx(g,y)$( not endogenousRetireYrs(g,y) ) = 0 ;

* Fix the endogenous retirement binaries at zero for all cases where it's not required.
  BRET.lo(g,y) = 0 ;     BRET.up(g,y) = 1 ;
  BRET.fx(g,y)$( not endogenousRetireDecisnYrs(g,y) ) = 0 ;
  ISRETIRED.lo(g) = 0 ;  ISRETIRED.up(g) = 1 ;
  ISRETIRED.fx(g)$( not possibleToEndogRetire(g) ) = 0 ;

* Fix BUILD, RETIRE, BRET and ISRETIRED using the values from the specified 'GR' investment schedule.
$ if %GRscheduleRead%==0 $goto noGRschedule2
  BUILD.fx(g,y) = 0 ;    BUILD.fx(g,y)$fix_BUILD(g,y) = fix_BUILD(g,y) ;
  RETIRE.fx(g,y) = 0 ;   RETIRE.fx(g,y)$fix_RETIRE(g,y) = fix_RETIRE(g,y) ;
  BRET.fx(g,y) = 0 ;     BRET.fx(g,y)$fix_BRET(g,y) = fix_BRET(g,y) ;
  ISRETIRED.fx(g) = 0 ;  ISRETIRED.fx(g)$fix_ISRETIRED(g) = fix_ISRETIRED(g) ;
$ label noGRschedule2

* Impose upper bound of 1 on continuous 0-1 transmission-related variables.
  TXUPGRADE.lo(r,rr,ps,pss,y) = 0 ;  TXUPGRADE.up(validTransitions(paths,ps,pss),y) = 1 ;
  TXPROJVAR.lo(tupg,y) = 0 ;         TXPROJVAR.up(tupg,y) = 1 ;

* Force transmission upgrades to occur in the user-specified year (do this in either endogogenous or exogenous investment mode).
  loop((transitions(tupg,r,rr,ps,pss),y)$( txFixedComYr(transitions) = yearNum(y) ),
    TXUPGRADE.fx(r,rr,ps,pss,y) = 1 ;
  ) ;

* Fix all years prior to earliest year at zero for either exogenous or endogenous transmission investment.
  loop((transitions(tupg,r,rr,ps,pss),y)$( yearNum(y) < txEarlyComYr(transitions) ),
    TXUPGRADE.fx(r,rr,ps,pss,y) = 0 ;
  ) ;

* Fix transmission binaries to zero if they're not needed.
  BTX.lo(paths,ps,y) = 0 ;  BTX.up(paths,ps,y) = 1 ;
  BTX.fx(notAllowedStates,y) = 0 ;


* Second, loop through each of the steps: timing, reoptimisation, and dispatch.
  loop(steps,

*   If it's a reoptimisation solve, fix the build (generation and transmission) to be the same as for
*   the timing solve, but free up the movers.
    if(sameas(steps,'reopt'),
      BUILD.fx(g,y) = BUILD.l(g,y) ;

      TXPROJVAR.fx(tupg,y) = TXPROJVAR.l(tupg,y) ;
      TXUPGRADE.fx(validTransitions(paths,ps,pss),y) = TXUPGRADE.l(paths,ps,pss,y) ;
      BTX.fx(paths,ps,y) = BTX.l(paths,ps,y) ;

      loop((g,movers(k))$( (noExist(g) * mapg_k(g,k)) * (not moverExceptions(g)) ),
        BUILD.lo(g,y)$validYrBuild(g,y) = 0 ;
        BUILD.up(g,y)$validYrBuild(g,y) = i_nameplate(g) ;
      ) ;

*     Similarly, fix the retirements to be the same as for the timing solve.
      BRET.fx(g,y) = BRET.l(g,y) ; ISRETIRED.fx(g) = ISRETIRED.l(g) ; RETIRE.fx(g,y) = RETIRE.l(g,y) ;

*     If it's a dispatch solve, fix the timing decisions (of generation and transmission investment, and
*     generation retirement and refurbishment) from the timing/reoptimisation solve.
      else if(sameas(steps,'dispatch'),
        BUILD.fx(g,y) = BUILD.l(g,y) ;

        BRET.fx(g,y) = BRET.l(g,y) ;
        ISRETIRED.fx(g) = ISRETIRED.l(g) ;
        RETIRE.fx(g,y) = RETIRE.l(g,y) ;

        TXPROJVAR.fx(tupg,y) = TXPROJVAR.l(tupg,y) ;
        TXUPGRADE.fx(validTransitions(paths,ps,pss),y) = TXUPGRADE.l(paths,ps,pss,y) ;
        BTX.fx(paths,ps,y) = BTX.l(paths,ps,y) ;
      ) ;

*   End of step-type if
    ) ;

*   Third, loop over each scenarioSet for this step of the experiment.
    loop(allSolves(experiments,steps,scenSet),

*     Initialise the desired scenarios for this solve
      sc(scen) = no ;
      sc(scen)$mapScenarios(scenSet,scen) = yes ;

*     Select the appropriate scenario weight.
      scenarioWeight(sc) = 0 ;
      scenarioWeight(sc) = weightScenariosBySet(scenSet,sc) ;
      display 'Scenario and weight for this solve:', sc, scenarioWeight ;

*     Compute the hydro output values to use for the selected scenarios (NB: only works for hydroSeqTypes={same,sequential}).
      modelledHydroOutput(g,y,t,scen) = 0 ;
      loop(sc(scen),
        if(mapSC_hydroSeqTypes(scen,'same'),
          modelledHydroOutput(g,y,t,scen) = hydroOutputScalar *
            sum((mapv_g(v,g),mapm_t(m,t),hY)$(mapSC_hY(scen,hY)), historicalHydroOutput(v,hY,m)) / sum(mapSC_hY(scen,hYY), 1) ;
*         Capture the current mapping of historical hydro years to modelled years.
          mapHydroYearsToModelledYears(experiments,steps,scenSet,sc,y,hY)$( ord(hY) = sum(mapSC_hY(scen,hYY), 1) ) = yes ;
*         Assign hydro output to potential new plant linked to existing schedulable hydro plant.
          hydroOutputUpgrades(schedHydroUpg(gg),y,t,sc) = sum(mapSH_Upg(g,gg)$i_namePlate(g), modelledHydroOutput(g,y,t,sc) / i_namePlate(g) ) ;
          else
          loop(y,
            chooseHydroYears(hY) = no ;
            chooseHydroYears(hY)$(sum(hYY$(mapSC_hY(scen, hYY) and ord(hYY) + ord(y) - 1            = ord(hY)), 1)) = yes ;
            chooseHydroYears(hY)$(sum(hYY$(mapSC_hY(scen, hYY) and ord(hYY) + ord(y) - 1 - card(hY) = ord(hY)), 1)) = yes ;
            modelledHydroOutput(g,y,t,sc) =
              sum((mapv_g(v,g),mapm_t(m,t),chooseHydroYears), historicalHydroOutput(v,chooseHydroYears,m)) / sum(chooseHydroYears, 1) ;
*           Capture the current mapping of historical hydro years to modelled years. 
            mapHydroYearsToModelledYears(experiments,steps,scenSet,sc,y,chooseHydroYears) = yes ;
*           Assign hydro output to potential new plant linked to existing schedulable hydro plant.
            hydroOutputUpgrades(schedHydroUpg(gg),y,t,sc) = sum(mapSH_Upg(g,gg)$i_namePlate(g), modelledHydroOutput(g,y,t,sc) / i_namePlate(g) ) ;
          ) ;
        ) ;
      ) ;
      display 'Hydro output:', modelledHydroOutput, hydroOutputUpgrades ;

*     Collect modelledHydroOutput and hydroOutputUpgrades for posterity.
      allModelledHydroOutput(experiments,steps,scenSet,g,y,t,sc) = modelledHydroOutput(g,y,t,sc) ;
      allHydroOutputUpgrades(experiments,steps,scenSet,g,y,t,sc) = hydroOutputUpgrades(g,y,t,sc) ;

*     Solve either GEM or DISP, depending on what step we're at.
      if(sameas(steps,'dispatch'),
        Solve DISP using %DISPtype% minimizing TOTALCOST ;
        else
        Solve GEM using %GEMtype% minimizing TOTALCOST ;
      ) ;

*     Add up the value of slacks and penalties in current model.
      slacks = %AddUpSlacks% ;
      penalties = %AddUpPenalties% ;

*     Collect solver and model info for current model just solved, and figure out if entire GEMsolve job should be
*     aborted, or a warning issued - but hold off doing anything until solve report is created.
      counter = 0 ;
      if(sameas(steps,'dispatch'),
        genSecs = DISP.resGen ; numSecs = DISP.resUsd ; numIters = DISP.iterUsd ; solverStat = DISP.solveStat ; modelStat = DISP.modelStat ;
        solveReport(allSolves,'OptFile') = DISP.optfile ;      solveReport(allSolves,'DVars')  = DISP.numdvar ;
        solveReport(allSolves,'Vars')    = DISP.numvar ;       solveReport(allSolves,'DVars')  = DISP.numdvar ;
        solveReport(allSolves,'Eqns')    = DISP.numequ ;
        else
        genSecs = GEM.resGen ; numSecs = GEM.resUsd ; numIters = GEM.iterUsd ; solverStat = GEM.solveStat ; modelStat = GEM.modelStat ;
        solveReport(allSolves,'OptFile') = gem.optfile ;       solveReport(allSolves,'OptCr')  = gem.optcr ;
        solveReport(allSolves,'Vars')    = gem.numvar ;        solveReport(allSolves,'DVars')  = gem.numdvar ;
        solveReport(allSolves,'Eqns')    = gem.numequ ;        solveReport(allSolves,'GapAbs') = abs( gem.objest - gem.objval ) ;
        solveReport(allSolves,'Gap%')$gem.objval = 100 * abs( gem.objest - gem.objval ) / gem.objval ;
      ) ;

      solveReport(allSolves,'ObjFnValue') = TOTALCOST.l ;      solveReport(allSolves,'SCcosts') = sum(sc(scen), SCENARIO_COSTS.l(sc) ) ;
      solveReport(allSolves,'ModStat') = modelStat ;           solveReport(allSolves,'SolStat') = solverStat ;
      solveReport(allSolves,'genSecs') = genSecs ;             solveReport(allSolves,'Time')    = numSecs ;
      solveReport(allSolves,'Iter')    = numIters ;

      counter$( solverStat <> 1 and modelStat <> 1 and modelStat <> 8 ) = 1 ;

      if(slacks > 0,    solveReport(allSolves,'Slacks') = 1    else solveReport(allSolves,'Slacks') = -99 ) ;
      if(penalties > 0, solveReport(allSolves,'Penalties') = 1 else solveReport(allSolves,'Penalties') = -99 ) ;

      display 'solve report:', slacks, penalties, solveReport ;

*     Post a progress/status message to the console.
      putclose con // '   Model: ' experiments.tl '-' steps.tl '-' scenSet.tl ' from the run %runName% and the run version %runVersionName% has finished' /
                      '   Objective function value: ' TOTALCOST.l:<12:1 // ;

*     If job is about to be aborted or a warning issued, post an error message in GEMsolveReport.
      if(counter = 1,
        putclose rep / 'Model: ' experiments.tl '-' steps.tl '-' scenSet.tl ' finished with some sort of problem and the job is now going to abort.' /
                       'Examine GEMsolve.lst and/or GEMsolve.log to see what went wrong.' ;
      ) ;

      if(sameas(steps,'dispatch'),
        abort$( DISP.modelstat <> 1 and DISP.modelstat <> 8 ) "Problem encountered when solving DISP..." ;
        abort$( DISP.solvestat = 2 )   "The iteration limit is insufficient. Increase iterlim in GEMsolve.gms" ;
        display$( DISP.solvestat = 3 ) "Warning: The CPU seconds for DISP are insufficient. Increase disp.reslim in GEMsolve.gms" ;
        abort$( DISP.solvestat = 7 )   "The license file does not support your solver selections" ;
        else
        abort$( GEM.modelstat = 10 )   "GEM is infeasible - check out GEMsolve.log to see what you've done wrong in configuring a model that is infeasible" ;
        abort$( GEM.modelstat <> 1 and GEM.modelstat <> 8 ) "Problem encountered solving GEM..." ;
        abort$( GEM.solvestat = 2 )    "The iteration limit is insufficient. Increase iterlim in GEMsolve.gms" ;
        display$( GEM.solvestat = 3 )  "Warning: The CPU seconds for GEM are insufficient. Increase CPUsecsGEM in GEMssttings.inc" ;
        abort$( GEM.solvestat = 7 )    "The license file does not support your solver selections" ;
     ) ;

*     Write current model summary information to GEMsolveReport:
      put rep / 'Experiment: ' experiments.tl '.  Step: ' steps.tl '.  Scenario set: ' scenSet.tl '.' /
      '  Objective function value: ' @32 TOTALCOST.l:>12:1 / ;
$     if "%GEMtype%"=="RMIP" $goto skipThis
      if(not sameas(steps,'dispatch'),
        put '  Percent gap:'                @33 solveReport(allSolves,'Gap%'):12:2 /
            '  Absolute gap:'               @30 solveReport(allSolves,'GapAbs'):12:0 /
            '  Number of binary variables:' @30 solveReport(allSolves,'DVars'):12:0 /
      ) ;
$     label skipThis
      put
      '  Number of variables:'        @30 solveReport(allSolves,'Vars'):12:0 /
      '  Number of equations:'        @30 solveReport(allSolves,'Eqns'):12:0 /
      '  Number of iterations:'       @30 solveReport(allSolves,'Iter'):12:0 /
      '  Model generation seconds:'   @30 solveReport(allSolves,'genSecs'):12:0 /
      '  CPU seconds:'                @30 solveReport(allSolves,'Time'):12:0 /
      '  Number of iterations:'       @30 solveReport(allSolves,'Iter'):12:0 /
      '  Slacks present: '            @39 if(slacks > 0, put 'Yes' else put @40 'No' ) put /
      '  Penalties present: '         @39 if(penalties > 0, put 'Yes' else put @40 'No' ) put /
      '  MIP/RMIP:' @38 if(sameas(steps,'dispatch'), put "%DISPtype%" else put "%GEMtype%" ) ;
      if(%GRscheduleRead%=0, put / else put '  Fixed investment schedule:' @30 "%GRscheduleFile%" // ) ; 

*     Write a GAMS-readable file of variable levels for fixing variables in subsequent models (requires GRscheduleWrite = 1).
      if(GRscheduleWrite,
        dummy.lw = 0 ; put dummy ;
        put_utility 'ren' / "%OutPath%\%runName%\Processed files\GRschedule - %runVersionName%_" experiments.tl '_' steps.tl '_' scenSet.tl '.gms' ;
        if(not sameas(steps,'dispatch'),
          put "Parameter fix_BUILD(g,y)   'New capacity installed by generating plant and year, MW' /" ;
          loop((g,y)$BUILD.l(g,y), put /  "'" g.tl "'.'" y.tl "'" BUILD.l(g,y):15:8 ) put ' /;' // ;
          put "Parameter fix_RETIRE(g,y)  'Capacity endogenously retired by generating plant and year, MW' /" ;
          loop((g,y)$RETIRE.l(g,y), put / "'" g.tl "'.'" y.tl "'" RETIRE.l(g,y):15:8 ) put ' /;' // ;
          put "Parameter fix_BRET(g,y)    'Binary variable to identify endogenous retirement year for the eligble generation plant' /" ;
          loop((g,y)$BRET.l(g,y),   put / "'" g.tl "'.'" y.tl "'" BRET.l(g,y):15:8 ) put ' /;' // ;
          put "Parameter fix_ISRETIRED(g) 'Binary variable to identify if the plant has actually been endogenously retired (0 = not retired, 1 = retired)' /" ;
          loop(g$ISRETIRED.l(g),    put / "'" g.tl "'" ISRETIRED.l(g):15:8 ) put ' /;' // ;
*         put "Parameter fix_CGEN(g,y)    'Continuous variable to identify build year for new scalable generation plant - for plant in integerPlantBuild set (CGEN or BGEN = 0 in any year)' /" ;
*         loop((g,y)$CGEN.l(g,y),   put / "'" g.tl "'.'" y.tl "'" CGEN.l(g,y):15:8 ) put ' /;' // ;
*         put "Parameter fix_BGEN(g,y)    'Binary variable to identify build year for new generation plant' /" ;
*         loop((g,y)$BGEN.l(g,y),   put / "'" g.tl "'.'" y.tl "'" BGEN.l(g,y):15:8 ) put ' /;' // ;
*         put "Parameter fix_CAPACITY(g,y)  'Cumulative nameplate capacity at each generating plant in each year, MW' /" ;
*         loop((g,y)$CAPACITY.l(g,y), put / "'" g.tl "'.'" y.tl "'" CAPACITY.l(g,y):15:8 ) put ' /;' // ;
*         put "Parameter fix_BTX(r,rr,ps,y) 'Binary variable indicating the current state of a transmission path' /" ;
*         loop((r,rr,ps,y)$BTX.l(r,rr,ps,y), put / "'" r.tl "'.'" rr.tl "'.'" ps.tl "'.'" y.tl "'" BTX.l(r,rr,ps,y):15:8 ) put ' /;' // ;
        ) ;
      ) ;

*     Collect up solution values - by experiment, step and scenarioSet.
$     include CollectResults.inc

*   End of scenarioSet loop.
    ) ;

* End of steps loop.
  ) ;

* Before going around the 'Experiments' loop again, dump all output for the current experiment to a GDX file named after the experiment.
  put dummy ;
  put_utility 'gdxout' / '%OutPath%\%runName%\GDX\temp\AllOut\' experiments.tl ;
  execute_unload
* Free Variables
  s_TOTALCOST s_SCENARIO_COSTS s_TX s_THETA
* Binary Variables
  s_BGEN s_BRET s_ISRETIRED s_BTX s_NORESVTRFR
* Positive Variables
  s_REFURBCOST s_GENBLDCONT s_CGEN s_BUILD s_RETIRE s_CAPACITY s_TXCAPCHARGES s_GEN s_VOLLGEN s_PUMPEDGEN s_LOSS s_TXPROJVAR s_TXUPGRADE
* Reserve variables
  s_RESV s_RESVVIOL s_RESVTRFR s_RESVREQINT
* Non-free reserve variable
  s_RESVCOMPONENTS
* Penalty variables
  s_RENNRGPENALTY s_PEAK_NZ_PENALTY s_PEAK_NI_PENALTY s_NOWINDPEAK_NI_PENALTY
* Slack variables
  s_ANNMWSLACK s_RENCAPSLACK s_HYDROSLACK s_MINUTILSLACK s_FUELSLACK
* Equations (ignore the objective function)
  s_calc_scenarioCosts s_calc_nfreserves s_resv_capacity s_calc_refurbcost s_calc_txcapcharges
  s_bldgenonce s_buildcapint s_buildcapcont s_annnewmwcap s_endogpltretire s_endogretonce s_balance_capacity s_bal_supdem s_peak_nz s_peak_ni s_noWindPeak_ni
  s_limit_maxgen s_limit_mingen s_minutil s_limit_fueluse s_limit_nrg s_minreq_rennrg s_minreq_rencap s_limit_hydro s_limit_pumpgen1 s_limit_pumpgen2 s_limit_pumpgen3
  s_calcTxLossesMIP s_calcTxLossesRMIP s_tx_capacity s_tx_projectdef s_tx_onestate s_tx_upgrade s_tx_oneupgrade
  s_tx_dcflow s_tx_dcflow0 s_equatetxloss s_txGrpConstraint s_resvsinglereq1 s_genmaxresv1 s_resvtrfr1 s_resvtrfr2 s_resvtrfr3 s_resvrequnit s_resvreq2
  s_resvreqhvdc s_resvtrfr4 s_resvtrfrdef s_resvoffcap s_resvreqwind ;

* Repeat the output dump to a GDX file named after the experiment, but this time dump only the output required for reporting.
  put dummy ;
  put_utility 'gdxout' / '%OutPath%\%runName%\GDX\temp\RepOut\' experiments.tl ;
  execute_unload
  solveReport
* Variable levels
  s_TOTALCOST s_TX s_BTX s_REFURBCOST s_BUILD s_RETIRE s_CAPACITY s_TXCAPCHARGES s_GEN s_VOLLGEN s_LOSS s_TXPROJVAR s_TXUPGRADE
  s_RENNRGPENALTY s_PEAK_NZ_PENALTY s_PEAK_NI_PENALTY s_NOWINDPEAK_NI_PENALTY
  s_ANNMWSLACK s_RENCAPSLACK s_HYDROSLACK s_MINUTILSLACK s_FUELSLACK s_RESV s_RESVVIOL s_RESVCOMPONENTS
* Equation marginals (ignore the objective function)
  s_bal_supdem s_peak_nz s_peak_ni s_noWindPeak_ni s_limit_maxgen s_limit_mingen s_minutil s_limit_fueluse s_limit_Nrg
  s_minReq_RenNrg s_minReq_RenCap s_limit_hydro s_tx_capacity
  ;

* End of experiments loop.
) ;


* Place some white space in GEMsolveReport ahead of the next run version. Also, note whether any models in this run version contain slacks or penalties. 
if(sum(allSolves, solveReport(allSolves,'Penalties')) > 0, putclose rep / '+++ At least one of the above models contains penalty variables +++' ) ;
if(sum(allSolves, solveReport(allSolves,'Slacks')) > 0,    putclose rep / '+++ At least one of the above models contains slack variables +++' ) ;
putclose rep /// ;


* Merge the GDX files from each experiment into a single GDX - one for all output and once for the 'report only' output. Call the files 'allExperimentsXXX.gdx'.
execute 'gdxmerge "%OutPath%\%runName%\GDX\temp\AllOut\"*.gdx output="%OutPath%\%runName%\GDX\allExperimentsAllOutput - %runName%_%runVersionName%.gdx" big=100000'
execute 'gdxmerge "%OutPath%\%runName%\GDX\temp\RepOut\"*.gdx output="%OutPath%\%runName%\GDX\allExperimentsReportOutput - %runName%_%runVersionName%.gdx" big=100000'

* NB: The big parameter is used to specify a cutoff for symbols that will be written one at a time. Each symbol
* that exceeds the size will be processed by reading each gdx file and only process the data for that symbol. This
* can lead to reading the same gdx file many times, but it allows the merging of large data sets.



*===============================================================================================
* 4. Dump selected prepared input data into a GDX file and rename/relocate a couple of log files.
*    NB: input data is as imported from .gdx/.inc files or from intermediate steps in GEMdata.

Execute_Unload "%OutPath%\%runName%\Input data checks\Selected prepared input data - %runName%_%runVersionName%.gdx",
* Basic sets, subsets, and mapping sets.
  y t f fg k g o lb i r e ild ps tupg scen rc n tgc hY s
  techColor fuelColor fuelGrpColor
  mapg_k mapg_o mapg_e mapg_f mapf_fg maps_r mapg_r mapild_r mapAggR_r avgDispatchSteptoRepStep allAvgDispatchSolves allNotAvgDispatchSolves
  isIldEqReg firstPeriod firstYr lastYr allButFirstYr
  paths nwd swd interIsland pumpedHydroPlant wind gas diesel
  thermalFuel i_fuelQuantities renew schedHydroPlant trnch demandGen 
  allSolves weightScenariosBySet numExperiments numSteps numScenarioSets numScenarios
* Time, financial, capex and cost related sets and parameters
  yearNum taxRate discountRates PVfacG PVfacT PVfacsM PVfacsEY PVfacs capexLife annuityFacN annuityFacR TxAnnuityFacN TxAnnuityFacR
  capRecFac depTCrecFac txCapRecFac txDepTCrecFac i_capitalCost i_connectionCost ensembleFactor capexPlant refurbCapexPlant
  capCharge refurbCapCharge txCapCharge
  i_winterCapacityMargin i_SIACrisk i_fkSI i_fkNI i_HVDClossesAtMaxXfer i_largestGenerator i_P200ratioNZ i_P200ratioNI
  i_fixedOM i_HVDCshr i_HVDClevy totalFuelCost CO2taxByPlant srmc i_plantReservesCost
* Generation plant related sets and parameters
  exist noExist commit new neverBuild nigen sigen possibleToBuild validYrBuild linearPlantBuild integerPlantBuild validYrOperate
  exogMWretired possibleToEndogRetire possibleToRetire possibleToRefurbish continueAftaEndogRetire peakConPlant NWpeakConPlant
  endogenousRetireDecisnYrs endogenousRetireYrs movers i_nameplate i_heatRate initialCapacity maxCapFactPlant minCapFactPlant AnnualMWlimit
  i_minUtilisation i_maxNrgByFuel renNrgShrOn renCapShrOn niNWpeakCnstrntOn i_renewNrgShare i_renewCapShare i_fof
  i_distdGenRenew i_distdGenFossil i_pumpedHydroEffic i_PumpedHydroMonth i_UnitLargestProp MWtoBuild
* Load and peak
  hoursPerBlock AClossFactors scenarioNRGfactor i_NrgDemand NrgDemand ldcMW scenarioPeakLoadFactor peakLoadNZ peakLoadNI
* Transmission and grid
  DCloadFlowOn transitions validTransitions allowedStates upgradeableStates i_txCapacity i_txCapacityPO txCapitalCost
  lossSlopeMIP lossIntercept bigLoss bigM susceptanceYr BBincidence regLower validTGC i_txGrpConstraintsLHS i_txGrpConstraintsRHS
* Reserves
  reservesOn singleReservesReqF i_maxReservesTrnsfr i_reserveReqMW i_propWindCover windCoverPropn reservesCapability i_offlineReserve
* Non-free reserves
  lvl pNFresvCap pNFresvCost
* Hydro related sets and parameters
  hydroOutputScalar allModelledHydroOutput allHydroOutputUpgrades mapHydroYearsToModelledYears
* Penalties
  penaltyViolatePeakLoad penaltyViolateRenNrg penaltyViolateReserves
  ;

* Stamp run version name, description and colour scheme in runVersions.txt
putclose rvs "%runVersionName%" ' | ' "%runVersionDesc%" ' | ' "%runVersionRGB%" / ;

bat.ap = 0 ;
putclose bat
  'copy "GEMsolve.log" "%OutPath%\%runName%\Processed files\GEMsolveLog - %runName%_%runVersionName%.txt"' /
  'copy "Report.txt"   "%OutPath%\%runName%\GEMsolveReport - %runName%.txt"' / ;
execute 'temp.bat' ;




* End of file.



$stop

*  x. Create an awk script which, when executed, will produce a file containing the number of integer solutions per MIP model.
$if %GEMtype%=="rmip" $goto NoMIP
$onecho > f.awk
/Restarting execution/ {
  ++count2
  if (count1>0) {
    print "Model: " name "  Integer solutions: " count1
    count1 = 0
  }
}

/^\* / {
  ++count1
}

/^--- LOOPS / {
# Assumes there will be 3 nested loops
  name = $NF
  getline
  name = name "-" $NF
  getline
  name = name "-" $NF
}
$offecho
$label NoMIP
