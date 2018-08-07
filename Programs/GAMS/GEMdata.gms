* GEMdata.gms


* Last modified by Dr Phil Bishop, 01/06/2012 (imm@ea.govt.nz)


$ontext
  This program prepares the data for a single run version of GEM. Note that a GEM run version may comprise many experiments and
  scenarios. Note too that one or many run versions comprise a GEM run. GEMdata imports the input data from GDX files, undertakes
  some manipulations/transformations, and performs integrity checks. It finishes by writing out some input data summary tables.

  The GEMdata invocation requires GEMdata to be restarted from the GEMdeclarations work file (GEMdeclarations.g00). The files
  called GEMpathsAndFiles.inc, GEMsettings.inc and GEMstochastic.inc are included into GEMdata. The GEMdata work file is saved
  and used to start GEMsolve. GEMsolve is invoked immediately after GEMdata and from the same 'runGEM...' script.

  Notes:
  1. The override structure needs to be generalised to work with any number of input parameters - perhaps even all of them? Also,
     when this takes place, the override declarations need to be moved into GEMdeclarations.
  2. See "File rawData 'GEM input data'" The creation of this file needs to be completed. Symbols need to be group together in a
     sensible order. The generated output needs to be checked for reliability. Some symbols, such as i_inflexiblePlantFactor(g,lb),
     are not yet written out.
  3. Need to update and make current the display statement in code section 5.
  4. Code section (6)(d) - some info from GEMsettings and elsewhere(?) not yet written or its no longer relevant.
  5. Some transmission-related data is yet to be written into transmission input data summary file - see code section (6)(e). 
  6. ...

 Code sections:
  1. Take care of a few preliminaries.
  2. Load data that comes from the 3 input GDX files and/or the override files (also from GEMsettings for set y).
  3. Initialise sets and parameters.
     a) Time/date-related sets and parameters.
     b) Various mappings, subsets and counts.
     c) Financial parameters.
     d) Generation data.
     e) Transmission data.
     f) Reserve energy data.
     g) Non-free reserves
     h) Create set of VOLL plant - one per region.
  4. Prepare the scenario-dependent input data; key user-specified settings are obtained from GEMstochastic.inc.
  5. Ascertain which dispatch solves ought to be summed and averaged for reporting purposes...
  6. Display sets and parameters.
  7. Create input data summaries.
     a) Declare input data summary files.
     b) Do the calculations.
     c) Write miscellaneous configuration information required later by GEMreports.
     d) Write the run configuration summary.
     e) Write the transmission data summaries.
     f) Write the plant data summaries.
     g) Write the capex statistics.
     h) Write the load summaries.
     i) Include code to compute and write out LRMC of all non-existing plant.
$offtext



*===============================================================================================
* 1. Take care of a few preliminaries.

* Track memory usage.
* Higher numbers are for more detailed information inside loops. Alternatively, on the command line, type: gams xxx profile=1
*option profile = 1 ;
*option profile = 2 ;
*option profile = 3 ;

option seed = 101 ;
$include GEMpathsAndFiles.inc
$include GEMsettings.inc

* Turn the following on/off as desired.
$offupper onempty inlinecom { } eolcom !
$offuelxref offuellist	
$offsymxref offsymlist

* Create and execute a batch file to archive/save selected files.
File bat "A recyclable batch file" / "%ProgPath%temp.bat" / ; bat.lw = 0 ; bat.ap = 0 ;
putclose bat
  'copy "%DataPath%\%GEMinputGDX%"        "%OutPath%\%runName%\Archive\"' /
  'copy "%DataPath%\%GEMnetworkGDX%"      "%OutPath%\%runName%\Archive\"' /
  'copy "%DataPath%\%GEMdemandGDX%"       "%OutPath%\%runName%\Archive\"' /
  'copy "%ProgPath%\GEMpathsAndFiles.inc" "%OutPath%\%runName%\Archive\GEMpathsAndFiles - %runVersionName%.inc"' /
  'copy "%ProgPath%\GEMsettings.inc"      "%OutPath%\%runName%\Archive\GEMsettings - %runVersionName%.inc"' /
  'copy "%ProgPath%\GEMstochastic.inc"    "%OutPath%\%runName%\Archive\GEMstochastic - %runVersionName%.inc"' / ;
$ if %useOverrides%==0 $goto noOverrides1
  bat.ap = 1 ; putclose bat
  'copy "%DataPath%\%GEMoverrideGDX%"     "%OutPath%\%runName%\Archive\"' / ;
$ label noOverrides1
execute 'temp.bat' ;
bat.ap = 0 ;



*===============================================================================================
* 2. Load data that comes from the 3 input GDX files and/or the override files (also from GEMsettings for set y).
*    NB: Some symbols in the input GDX files are defined on years that may extend beyond %firstYear% and %lastYear%.
*        Hence, those symbols must be loaded without domain checking, i.e. $load c.f. $loaddc.

Set y  / %firstYear% * %lastYear% / ;

* Load 133 symbols from 3 input GDX files: 114 from GEMinputGDX, 18 from GEMnetworkGDX, and 1 from GEMdemandGDX.
$gdxin "%DataPath%\%GEMinputGDX%"
* Sets
$loaddc k f fg g o i e tgc t lb rc hY v
$loaddc mapf_k mapf_fg techColor fuelColor fuelGrpColor movers refurbish endogRetire cogen peaker hydroSched hydroPumped
$loaddc wind renew thermalTech demandGen randomiseCapex linearBuildTech coal lignite gas diesel
$loaddc mapGenPlant exist schedHydroUpg mapSH_Upg
$loaddc Haywards Benmore zoneCentroid islandCentroid
$loaddc mapm_t
$loaddc mapReservoirs
* Parameters 
$loaddc i_plantLife i_refurbishmentLife i_retireOffsetYrs i_linearBuildMW i_linearBuildYr i_depRate
$loaddc i_peakContribution i_NWpeakContribution i_capFacTech i_FOFmultiplier i_maxNrgByFuel i_emissionFactors
$load   i_fuelPrices i_fuelQuantities i_co2tax i_minUtilisation
$loaddc i_nameplate i_UnitLargestProp i_baseload i_offlineReserve i_FixComYr i_EarlyComYr i_ExogenousRetireYr i_refurbDecisionYear i_fof
$loaddc i_heatrate i_PumpedHydroMonth i_PumpedHydroEffic i_minHydroCapFact i_maxHydroCapFact i_fixedOM i_varOM i_varFuelCosts i_fixedFuelCosts
$loaddc i_capitalCost i_connectionCost i_refurbCapitalCost i_hydroPeakingFactor i_inflexiblePlantFactor i_plantReservesCap i_plantReservesCost
$loaddc i_PltCapFact i_HVDCshr
$load   i_renewNrgShare i_renewCapShare i_distdGenRenew i_distdGenFossil
$loaddc i_substnCoordinates i_zonalLocFacs
$load   i_HVDClevy
$load   i_firstDataYear i_lastDataYear i_HalfHrsPerBlk i_inflation
$loaddc i_ReserveSwitch i_ReserveAreas i_propWindCover i_ReservePenalty
$load   i_reserveReqMW i_winterCapacityMargin i_SIACrisk i_fkSI i_fkNI i_HVDClossesAtMaxXfer i_largestGenerator i_P200ratioNZ i_P200ratioNI
$load   i_firstHydroYear i_historicalHydroOutput

$gdxin "%DataPath%\%GEMnetworkGDX%"
* Sets
$loaddc r p ps tupg mapLocations regionCentroid txUpgradeTransitions mapArcNode
* Parameters 
$loaddc i_txCapacity i_txCapacityPO i_txResistance i_txReactance i_maxReservesTrnsfr
$loaddc i_txCapitalCost i_txEarlyComYr i_txFixedComYr i_txGrpConstraintsLHS i_txGrpConstraintsRHS

$gdxin "%DataPath%\%GEMdemandGDX%"
$load   i_NrgDemand

* Initialise set 'n' and record the number of loss tranches - %NumVertices% comes from GEMsettings.inc.
Set n 'Piecewise linear vertices' / n1 * n%NumVertices% / ;
numT = %NumVertices% - 1 ; ;

* Install data overrides.
** mds1, mds2 and mds5 override 3 params: i_fuelPrices, i_fuelQuantities and i_co2tax. mds4 overrides just 1 param: i_co2tax.
$if %useOverrides%==0 $goto noOverrides2

Parameters
  i_fuelPricesOvrd(f,y)           'Fuel prices by fuel type and year, $/GJ'
  i_fuelQuantitiesOvrd(f,y)       'Quantitative limit on availability of various fuels by year, PJ'
  i_co2taxOvrd(y)                 'CO2 tax by year, $/tonne CO2-equivalent'
  i_FixComYrOvrd(g)               'Fixed commissioning year for potentially new generation plant (includes plant fixed never to be built)'
  ;

$gdxin "%DataPath%\%GEMoverrideGDX%"
$load   i_fuelPricesOvrd i_fuelQuantitiesOvrd i_co2taxOvrd i_FixComYrOvrd
i_fuelPrices(f,y)$i_fuelPricesOvrd(f,y) = i_fuelPricesOvrd(f,y) ;              i_fuelPrices(f,y)$( i_fuelPrices(f,y) = eps ) = 0 ;
i_fuelQuantities(f,y)$i_fuelQuantitiesOvrd(f,y) = i_fuelQuantitiesOvrd(f,y) ;  i_fuelQuantities(f,y)$( i_fuelQuantities(f,y) = eps ) = 0 ;
i_co2tax(y)$i_co2taxOvrd(y) = i_co2taxOvrd(y) ;                                i_co2tax(y)$( i_co2tax(y) = eps ) = 0 ; 
i_FixComYr(g)$i_FixComYrOvrd(g) = i_FixComYrOvrd(g) ;                          i_FixComYr(g)$( i_FixComYr(g) = eps ) = 0 ; 
$label noOverrides2

* Create a CSV file of input data, including overrides - unadulterated and just as imported.
File rawData 'GEM input data' / "%OutPath%\%runName%\Input data checks\Raw GEM input data - %runName%_%runVersionName%.csv" / ;
rawData.pc = 5 ; rawData.pw = 999 ;
put rawData 'Data as imported into GEMdata.gms. Sourced from:' /
  '' "%DataPath%\%GEMinputGDX%" / '' "%DataPath%\%GEMnetworkGDX%" / '' "%DataPath%\%GEMdemandGDX%" / ;
put$(%useOverrides% <> 0) '' "%DataPath%\%GEMoverrideGDX%" / ;
put 'Sets and parameter symbols, and units, are more fully defined in "%DataPath%GEMdeclarations.gms"' //
 'Technology and fuel data' / 'k' 'f' 'fg' 'cogen' 'peaker' 'hydroSched' 'hydroPumped' 'wind' 'renew' 'thermalTech' 'demandGen' 'coal' 'lignite' 'gas' 'diesel'
 'i_depRate' 'i_plantLife' 'randomiseCapex' 'linearBuildTech' 'i_linearBuildMW' 'i_linearBuildYr' 'movers' 'refurbish' 'i_refurbishmentLife' 'endogRetire'
 'i_retireOffsetYrs' 'i_peakContribution' 'i_NWpeakContribution' 'i_capFacTech' 'i_maxNrgByFuel' 'i_emissionFactors' ;
loop((k,f,fg)$( mapf_k(f,k) * mapf_fg(f,fg) ),
  put / k.tl, f.tl, fg.tl, cogen(k), peaker(k), hydroSched(k), hydroPumped(k), wind(k), renew(k), thermalTech(k), demandGen(k), coal(f), lignite(f), gas(f), diesel(f)
  i_depRate(k), i_plantLife(k), randomiseCapex(k), linearBuildTech(k), i_linearBuildMW(k), i_linearBuildYr(k), movers(k), refurbish(k), i_refurbishmentLife(k), endogRetire(k)
  i_retireOffsetYrs(k), i_peakContribution(k), i_NWpeakContribution(k), i_capFacTech(k), i_maxNrgByFuel(f), i_emissionFactors(f) ;
) ;

put // 'Generation plant data' / 'g' 'k' 'f' 'i' 'o' 'exist' 'coal' 'lignite' 'gas' 'diesel' 'cogen' 'peaker' 'hydroSched' 'hydroPumped' 'wind' 'renew'
  'thermalTech' 'demandGen' 'schedHydroUpg' 'mapSH_Upg' 'i_nameplate' 'i_heatrate' 'i_fixedOM' 'i_varOM' 'i_varFuelCosts', 'i_fixedFuelCosts' 'i_hydroPeakingFactor'
  'i_HVDCshr' 'i_fof' 'i_capFacTech' 'i_peakContribution', 'i_NWpeakContribution' 'i_minHydroCapFact' 'i_maxHydroCapFact', 'i_PumpedHydroMonth' 'i_PumpedHydroEffic'
  'i_UnitLargestProp' 'i_baseload' 'i_offlineReserve' 'i_plantLife' 'i_capitalCost' 'i_connectionCost' 'i_FixComYr' 'i_EarlyComYr' 'i_ExogenousRetireYr'
  'refurbish' 'i_refurbishmentLife' 'i_refurbDecisionYear' 'i_refurbcapitalcost' 'i_retireOffsetYrs' ;
loop((g,k,f,i,o)$( mapgenplant(g,k,i,o) * mapf_k(f,k) ),
  put / g.tl, k.tl, f.tl, i.tl, o.tl, exist(g), coal(f), lignite(f), gas(f), diesel(f), cogen(k), peaker(k), hydroSched(k), hydroPumped(k), wind(k), renew(k)
  thermalTech(k), demandGen(k), schedHydroUpg(g) ;
  if(sum(mapSH_Upg(gg,g), 1), loop(mapSH_Upg(gg,g), put gg.tl ) else put '-' ) ;
  put i_nameplate(g), i_heatrate(g), i_fixedOM(g), i_varOM(g), i_varFuelCosts(g), i_fixedFuelCosts(g), i_hydroPeakingFactor(g), i_HVDCshr(o), i_fof(g), i_capFacTech(k)
  i_peakContribution(k), i_NWpeakContribution(k), i_minHydroCapFact(g), i_maxHydroCapFact(g), i_PumpedHydroMonth(g), i_PumpedHydroEffic(g), i_UnitLargestProp(g)
  i_baseload(g), i_offlineReserve(g), i_plantLife(k), i_capitalCost(g), i_connectionCost(g), i_FixComYr(g), i_EarlyComYr(g), i_ExogenousRetireYr(g) ;
  if( (exist(g) * refurbish(k) * i_refurbcapitalcost(g) * i_refurbDecisionYear(g) ),
    put refurbish(k), i_refurbishmentLife(k), i_refurbDecisionYear(g), i_refurbCapitalCost(g), i_retireOffsetYrs(k) else put '-' '-' '-' '-' '-' ;
  ) ;
) ;

put // 'Data defined by year'  / '' loop(y, put y.tl ) ;
put /  'Fuel prices, $/GJ'          loop(f$sum(y, i_fuelPrices(f,y)),     put / f.tl loop(y, put i_fuelPrices(f,y)) ) ; 
put /  'Fuel quantity, GJ'          loop(f$sum(y, i_fuelQuantities(f,y)), put / f.tl loop(y, put i_fuelQuantities(f,y)) ) ; 
put // 'CO2 tax, $/t CO2e'          loop(y, put i_co2tax(y) ) ;
put /  'i_HVDClevy, $/kW'           loop(y, put i_HVDClevy(y) ) ;
put /  'i_inflation'                loop(y, put i_inflation(y):5:3 ) ;
put /  'i_winterCapacityMargin, MW' loop(y, put i_winterCapacityMargin(y) ) ;
put /  'i_SIACrisk, MW'             loop(y, put i_SIACrisk(y) ) ;
put /  'i_fkSI, MW'                 loop(y, put i_fkSI(y) ) ;
put /  'i_fkNI, MW'                 loop(y, put i_fkNI(y) ) ; 
put /  'i_HVDClossesAtMaxXfer, MW'  loop(y, put i_HVDClossesAtMaxXfer(y) ) ;
put /  'i_largestGenerator, MW'     loop(y, put i_largestGenerator(y) ) ; 
put /  'i_P200ratioNZ'              loop(y, put i_P200ratioNZ(y) ) ; 
put /  'i_P200ratioNI'              loop(y, put i_P200ratioNI(y) ) ; 

put // 'Transmission data' / 'FrReg' 'ToReg' 'State' 'CapMW' 'CapPO_MW' 'Resistance' 'Reactance' '$m' 'EarlyYr' 'FixedYr' 'FrState' 'ToState' 'Upgrade' 'Upgrade description' ;
loop((r,rr,ps)$i_txCapacity(r,rr,ps),
  put / r.tl, rr.tl, ps.tl, i_txCapacity(r,rr,ps), i_txCapacityPO(r,rr,ps), i_txResistance(r,rr,ps):9:7, i_txReactance(r,rr,ps):6:4 ;
  loop((tupg,pss)$( txUpgradeTransitions(tupg,r,rr,pss,ps) or txUpgradeTransitions(tupg,rr,r,pss,ps) ),
    if(i_txCapitalCost(tupg), put ( i_txCapitalCost(tupg) * i_txCapacity(r,rr,ps) / ( i_txCapacity(r,rr,ps) + i_txCapacity(rr,r,ps)) ) else put '' ) ;
    if(i_txEarlyComYr(tupg),  put i_txEarlyComYr(tupg) else put '' ) ;
    if(i_txFixedComYr(tupg),  put i_txFixedComYr(tupg) else put '' ) ;
    put pss.tl, ps.tl, tupg.tl, tupg.te(tupg) ;
  ) ;
) ;

** Complete creation of this file. Group stuff in a sensible order. Check that output is reliable.
** Need to write out i_inflexiblePlantFactor(g,lb), among other symbols not yet written.



*===============================================================================================
* 3. Initialise sets and parameters.

* a) Time/date-related sets and parameters.
firstYear = %firstYear% ;
lastYear = %lastYear% ;

firstYr(y)$(ord(y) = 1) = yes ;
lastYr(y)$(ord(y) = card(y)) = yes ;

allButFirstYr(y)$(not firstYr(y)) = yes ;

yearNum(y) = firstYear + ord(y) - 1 ;

firstPeriod(t)$( ord(t) = 1 ) = yes ;

* Abort if modelled years are not a subset of data years.
abort$( firstYear < i_firstDataYear ) "First modelled year precedes first data year",             i_firstDataYear, firstYear, firstYr ;
abort$( lastYear  > i_lastDataYear )  "Last modelled year is later than the last data year",      i_lastDataYear, lastYear, lastYr ;
abort$( firstYear > lastYear )        "First modelled year is later than the last modelled year", firstYear, firstYr, lastYear, lastYr ;

* Denote each historical hydro year set element with a real number corresponding to that year, i.e. 1932 = 1932, 1933 = 1933,...2002 = 2002, etc.
hydroYearNum(hY) = i_firstHydroYear + ord(hY) - 1 ;

lastHydroYear = sum(hY$( ord(hY) = card(hY) ), hydroYearNum(hY)) ;

* Count hours per load block per time period.
hoursPerBlock(t,lb) = sum(mapm_t(m,t), 0.5 * i_HalfHrsPerBlk(m,lb)) ;


* b) Various mappings, subsets and counts.
* Location mappings
loop(maplocations(i,r,e,ild),
  mapi_r(i,r) = yes ;
  mapi_e(i,e) = yes ;
  mapild_r(ild,r) = yes ;
) ;
mapAggR_r('nz',r) = yes ;
mapAggR_r('ni',r) = yes$sum(mapild_r('ni',r), 1) ;
mapAggR_r('si',r) = yes$sum(mapild_r('si',r), 1) ;
* Figure out if there are just 2 regions and whether their names are identical to the names of the 2 islands.
loop(mapild_r(ild,r)$( sameas(r,'ni') or sameas(r,'si') ), isIldEqReg(ild,r) = yes ) ;
isIldEqReg(ild,r)$( card(isIldEqReg) <> 2 ) = no ;
* Generation plant mappings
loop(mapgenplant(g,k,i,o),
  mapg_k(g,k) = yes ;
  mapg_o(g,o) = yes ;
  mapg_i(g,i) = yes ;
) ;
mapg_f(g,f)     = yes$sum(mapg_k(g,k), mapf_k(f,k) ) ;
mapg_r(g,r)     = yes$sum(mapg_i(g,i), mapi_r(i,r) ) ;
mapg_e(g,e)     = yes$sum(mapg_i(g,i), mapi_e(i,e) ) ;
mapg_ild(g,ild) = yes$sum(mapg_r(g,r), mapild_r(ild,r) ) ;
* Reservoir mappings
loop(mapreservoirs(v,i,g),
  mapv_g(v,g) = yes ;
) ;
* Fuel mappings
loop((f,fg,thermalTech(k))$( mapf_fg(f,fg) * mapf_k(f,k) ), thermalFuel(f) = yes ) ;

* Count number of regions
numreg = card(r) ;

* Identify generation plant types
loop(hydroSched(k),  schedHydroPlant(g)$mapg_k(g,k) = yes ) ;
loop(hydroPumped(k), pumpedHydroPlant(g)$mapg_k(g,k) = yes ) ;

* Figure out which load blocks are immediately to the right of any given block. 
rightAdjacentBlocks(lb,lbb)$( ord(lbb) = ord(lb) + 1 ) = yes ;


* c) Financial parameters.
discountRates('WACCg') = WACCg ;
discountRates('WACCt') = WACCt ;
discountRates('dLow')  = discRateLow ;
discountRates('dMed')  = discRateMed ;
discountRates('dHigh') = discRateHigh ;

PVfacG(y,t) = 1 / ( 1 + WACCg ) ** ( (yearNum(y) - firstYear) + (ord(t) * 2 - 1) / ( 2 * card(t) ) ) ;

PVfacT(y,t) = 1 / ( 1 + WACCt ) ** ( (yearNum(y) - firstYear) + (ord(t) * 2 - 1) / ( 2 * card(t) ) ) ;

PVfacsM(y,t,d) = 1 / ( 1 + discountRates(d) ) ** ( (yearNum(y) - firstYear) + (ord(t) * 2 - 1) / ( 2 * card(t) ) ) ;

PVfacsEY(y,d)  = 1 / ( 1 + discountRates(d) ) ** (  yearNum(y) - firstYear + 1 ) ;

PVfacs(y,t,d,'mid') = PVfacsM(y,t,d) ;
PVfacs(y,t,d,'eoy') = PVfacsEY(y,d) ;

capexLife(k,'genplt') = i_plantLife(k) ;
capexLife(k,'refplt') = i_refurbishmentLife(k) ;

annuityFacN(y,k,ct)$WACCg = ( 1 - ( 1 + WACCg + i_inflation(y) ) ** (-capexLife(k,ct)) ) / WACCg ;

annuityFacR(k,ct)$WACCg   = ( 1 - ( 1 + WACCg ) ** (-capexLife(k,ct)) ) / WACCg ;

TxAnnuityFacN(y) = ( 1 - ( 1 + WACCt + i_inflation(y) ) ** (-txPlantLife) ) / WACCt ;
TxAnnuityFacR =    ( 1 - ( 1 + WACCt ) ** (-txPlantLife) ) / WACCt ;

* depType = 0 implies straight line depreciation; depType = 1 implies a declining balance method.
if(depType = 0,
  capRecFac(y,k,ct)$annuityFacR(k,ct)   = ( 1 - taxRate * annuityFacN(y,k,ct) / capexLife(k,ct) )  / annuityFacR(k,ct) ;
  depTCrecFac(y,k,ct)$annuityFacR(k,ct) =     ( taxRate * annuityFacN(y,k,ct) / capexLife(k,ct) )  / annuityFacR(k,ct) ;
  txCapRecFac(y)$txAnnuityFacR          = ( 1 - taxRate * txAnnuityFacN(y)    / txPlantLife )    / txAnnuityFacR ;
  txDepTCrecFac(y)$txAnnuityFacR        =     ( taxRate * txAnnuityFacN(y)    / txPlantLife )    / txAnnuityFacR ;
  else
  capRecFac(y,k,ct)$annuityFacR(k,ct)   = ( 1 - i_depRate(k) * taxRate / (WACCg + i_inflation(y) + i_depRate(k)) ) / annuityFacR(k,ct) ;
  depTCrecFac(y,k,ct)$annuityFacR(k,ct) =     ( i_depRate(k) * taxRate / (WACCg + i_inflation(y) + i_depRate(k)) ) / annuityFacR(k,ct) ;
  txCapRecFac(y)$txAnnuityFacR          = ( 1 - txDepRate    * taxRate / (WACCt + i_inflation(y) + txDepRate) )    / txAnnuityFacR ;
  txDepTCrecFac(y)$txAnnuityFacR        =     ( txDepRate    * taxRate / (WACCt + i_inflation(y) + txDepRate) )    / txAnnuityFacR ;
) ;


* d) Generation data.
* Derive various generating plant subsets.
* i) Set the V2G plant capacities to zero when the V2GtechnologyOn flag is zero (this takes those plant out of the pick list).
if(not V2GtechnologyOn, i_nameplate(g)$sum(mapg_k(g,k)$sameas(k,'V2G'), 1) = 0 ) ;

* ii) Existing plant - remove any plant where i_nameplate(g) = 0 from exist(g).
exist(g)$( i_nameplate(g) = 0 ) = no ;

* iii) A plant is not an existing plant if it hasn't been defined to be existing - remove any plant where i_nameplate(g) = 0 from noExist(g).
noExist(g)$( not exist(g) ) = yes ;
noExist(g)$( i_nameplate(g) = 0 ) = no ;

* iv) Define plant that are never able to be built. A plant is never to be built if it already exists, if (i_fixComYr or i_EarlyComYr) > lastYear,
* or if i_nameplate <= 0.
neverBuild(noExist(g))$( (i_fixComYr(g) > lastYear) or (i_EarlyComYr(g) > lastYear) ) = yes ;
neverBuild(g)$( i_nameplate(g) <= 0 ) = yes ;

* v) Define committed plant. To be a committed plant, the plant must not exist, it must not be in the neverBuild set, and
* it must have a fixed commissioning year that is greater than or equal to the first modelled year.
commit(noExist(g))$( (i_fixComYr(g) >= firstYear) * (not neverBuild(g)) ) = yes ;

* vi) Define new plant. A plant is (potentially) new if it is not existing, not committed, and not a member of the neverBuild set.
new(noExist(g))$( not ( commit(g) or neverBuild(g) ) ) = yes ;

* vii) Define the years in which it is valid for a generating plant to be built. The plant must either be committed or (potentially)
* new, and the plant can't be a member of the neverBuild set
validYrBuild(commit(g),y)$( yearNum(y) = i_fixComYr(g) ) = yes ;
validYrBuild(new(g),y)$( yearNum(y) >= i_EarlyComYr(g) ) = yes ;
validYrBuild(neverBuild(g),y) = no ;

* viii) Identify the plant that may be built, i.e. it doesn't already exist or it is not otherwise prevented from being built.
possibleToBuild(g)$sum(y$validYrBuild(g,y), 1) = yes ;

* ix) Identify generation plant that can be linearly or incrementally built.
loop((g,k)$( noExist(g) * linearBuildTech(k) * mapg_k(g,k) * ( not i_fixComYr(g) ) ),
  linearPlantBuild(g)$( i_nameplate(g) >= i_linearBuildMW(k) ) = yes ;
  linearPlantBuild(g)$( i_EarlyComYr(g) >= i_linearBuildYr(k) ) = yes ;
) ;

* x) Identify generation plant that must be integer build (must be integer if not linear).
integerPlantBuild(noExist(g))$( not linearPlantBuild(g) ) = yes ;
integerPlantBuild(neverBuild(g)) = no ;

* xi) Identify exceptions to the technology-determined list of plant movers, i.e. if user fixes build year to a legitimate value, then
* don't allow the plant to be a mover.
loop(movers(k), moverExceptions(noExist(g))$( mapg_k(g,k) * ( i_fixComYr(g) >= firstYear ) * ( i_fixComYr(g) <= lastYear ) ) = yes ) ;

* xii) Define the years in which it is valid for a generating plant to operate. The plant must exist; if plant is committed, it is valid to
* operate it in any year beginning with the year in which it is commissioned; if plant is new, it is valid to operate it in any year beginning
* with the earliest year in which it may be commissioned; it is not valid to operate any plant that has come to the end of its refurbished life
* (i.e. can't repeatedly refurbish); it is not valid to operate any plant that has been exogenously retired, or decommissioned; and it is not
* valid to operate any plant that is never able to be built.
validYrOperate(exist(g),y) = yes ;
validYrOperate(commit(g),y)$( yearNum(y) >= i_fixComYr(g) ) = yes ;
validYrOperate(new(g),y)$( yearNum(y) >= i_EarlyComYr(g) ) = yes ;
validYrOperate(g,y)$( i_refurbDecisionYear(g) * ( yearNum(y) > i_refurbDecisionYear(g) + sum(mapg_k(g,k), i_refurbishmentLife(k)) ) ) = no ;
validYrOperate(g,y)$( i_ExogenousRetireYr(g) * ( yearNum(y) >= i_ExogenousRetireYr(g) ) ) = no ;
validYrOperate(neverBuild(g),y) = no ;

* xiii) North and South Island plant
nigen(g)$mapg_ild(g,'ni') = yes ;  nigen(neverBuild(g)) = no ;
sigen(g)$mapg_ild(g,'si') = yes ;  sigen(neverBuild(g)) = no ;

* Define capacity of existing plant in first modelled year. Be aware that if capacity is committed in the first modelled year, the plant
* will be in the commit(g) and noExist(g) sets.
initialCapacity(exist(g)) = i_nameplate(g) ;

* Define exogenously retired MW by plant and year.
exogMWretired(g,y)$( i_ExogenousRetireYr(g) * ( yearNum(y) = i_ExogenousRetireYr(g) ) ) = i_nameplate(g) ;

* Identify all generation plant that may be endogenously retired.
possibleToEndogRetire(g)$i_refurbDecisionYear(g) = yes ;

* Identify all generation plant that may be retired (endogenously or exogenously).
possibleToRetire(g)$( possibleToEndogRetire(g) or i_ExogenousRetireYr(g) ) = yes ;

* Define contribution to peak capacity by plant
peakConPlant(g,y)   = sum(mapg_k(g,k), i_peakContribution(k) ) ;
NWpeakConPlant(g,y) = sum(mapg_k(g,k), i_NWpeakContribution(k) ) ;

* Initialise the FOF multiplier - compute a weighted average using annual hours per load block as the weights.
WtdAvgFOFmultiplier(k,lb) = sum(t, hoursPerBlock(t,lb) * i_FOFmultiplier(k,lb)) / sum(t, hoursPerBlock(t,lb)) ;

* Derive the minimum and maximum capacity factors for each plant and period.
* First average over time (i.e. over the months mapped to each period t) and assign to maxCapFactPlant.
maxCapFactPlant(g,t,lb)$sum(mapm_t(m,t), 1) = sum(mapm_t(m,t), i_PltCapFact(g,m) ) / sum(mapm_t(m,t), 1) ;
* Then, set all scheduable hydro max capacity factors to zero.
loop(mapg_k(g,hydroSched(k)), maxCapFactPlant(g,t,lb) = 0 ) ;
* Now, overwrite max capacity factor for hydro with user-defined, non-zero i_maxHydroCapFact(g) values.
maxCapFactPlant(g,t,lb)$i_maxHydroCapFact(g) = i_maxHydroCapFact(g) ;
* Now adjust all max capacity factors for forced outage factor.
maxCapFactPlant(g,t,lb) = maxCapFactPlant(g,t,lb) * sum(mapg_k(g,k), (1 - i_fof(g) * WtdAvgFOFmultiplier(k,lb)) ) ;
* Min capacity factor only meaningfully defined for hydro units.
minCapFactPlant(schedHydroPlant(g),y,t) = i_minHydroCapFact(g) ;
* But it is also 'non-meaningfully' defined to a low non-zero value for wind plant.
loop(mapg_k(g,wind(k)), minCapFactPlant(g,y,t) = .001 ) ;

* Identify all the generation plant that may possibly be refurbished or endogenously retired and the years in which that retirement may/will occur.
loop((g,k)$( exist(g) * refurbish(k) * mapg_k(g,k) * i_refurbcapitalcost(g) * i_refurbDecisionYear(g) ),
  possibleToRefurbish(g) = yes ;
  if(endogretire(k),
    endogenousRetireDecisnYrs(g,y)$( ( yearNum(y) >= firstYear + noRetire                        ) * ( yearNum(y) <= i_refurbDecisionYear(g) ) ) = yes ;
    endogenousRetireYrs(g,y)$(       ( yearNum(y) >= firstYear + noRetire + i_retireOffsetYrs(k) ) * ( yearNum(y) <= i_refurbDecisionYear(g) + i_retireOffsetYrs(k) ) ) = yes ;
  else
    endogenousRetireDecisnYrs(g,y)$( ( yearNum(y) >= firstYear + noRetire                        ) * ( yearNum(y) = i_refurbDecisionYear(g) ) ) = yes ;
    endogenousRetireYrs(g,y)$(       ( yearNum(y) >= firstYear + noRetire + i_retireOffsetYrs(k) ) * ( yearNum(y) = i_refurbDecisionYear(g) + i_retireOffsetYrs(k) ) ) = yes ;
  ) ;
) ;

* Compute the years a plant must keep going for after the decision to endogenously retire it has been made
loop(refurbish(k),
  continueAftaEndogRetire(g)$( possibleToEndogRetire(g) * mapg_k(g,k) ) = i_retireOffsetYrs(k) ;
) ;

* Define capital costs for new generation plant:
* - Capital costs are first calculated as if capex is lumpy. After any adjustments, they are then converted
*   to a levelised or annualised basis (i.e. see capCharge).

* First, transfer i_capitalCost to capexPlant and i_refurbCapitalCost to refurbCapexPlant, and convert both to $/MW.
capexPlant(g)       = 1e3 * i_capitalCost(g) ;
refurbCapexPlant(g) = 1e3 * i_refurbCapitalCost(g) ;

* Next, randomly adjust capexPlant to create mathematically different costs - this helps the solver but makes no
* appreciable economic difference provided randomCapexCostAdjuster is small.
loop(randomiseCapex(k),
  capexPlant(noExist(g))$mapg_k(g,k) =
  uniform( (capexPlant(g) - randomCapexCostAdjuster * capexPlant(g)),(capexPlant(g) + randomCapexCostAdjuster * capexPlant(g)) ) ;
) ;


* Zero out any refubishment capex costs if the plant is not actually a candidate for refurbishment.
refurbCapexPlant(g)$( not possibleToRefurbish(g) ) = 0 ;

* Now add on the 'variablised' connection costs to the adjusted plant capital costs - continue to yield NZ$/MW.
vbleConCostPlant(g)$i_nameplate(g) = 1e6 * i_connectionCost(g) / i_nameplate(g) ;
capexPlant(g) = capexPlant(g) + vbleConCostPlant(g) ;

* Finally, convert lumpy capital costs to levelised capital charge (units are now NZ$/MW/yr).
capCharge(g,y)       = capexPlant(g) * sum(mapg_k(g,k), capRecFac(y,k,'genplt')) ;
refurbCapCharge(g,y) = refurbCapexPlant(g) * sum(mapg_k(g,k), capRecFac(y,k,'refplt')) ;
refurbCapCharge(g,y)$( yearNum(y) < i_refurbDecisionYear(g) ) = 0 ;
refurbCapCharge(g,y)$( yearNum(y) > i_refurbDecisionYear(g) + sum(mapg_k(g,k), i_refurbishmentLife(k)) ) = 0 ;


* Calculate reserve capability per generating plant.
reservesCapability(g,rc)$i_plantReservesCap(g,rc) = i_nameplate(g) * i_plantReservesCap(g,rc) ;

* Add any fixed costs associated with fuel production and delivery to the fixed OM costs by plant.
i_fixedOM(g) = i_fixedOM(g) + i_fixedFuelCosts(g) * i_heatrate(g) / 1000 * 8.76 ;

* Compute the marginal loss location factors by plant (reciprocal of the zonally-based location factors).
* Also, set equal to 1 if it should be zero (it shouldn't be) or if there are more than 2 regions.
locationFactor(g) = sum(mapg_e(g,e)$i_zonalLocFacs(e), 1 / i_zonalLocFacs(e) ) ;
locationFactor(g)$( locationFactor(g) = 0 ) = 1 ;
locationFactor(g)$( numreg > 2 ) = 1 ;

* Collect up the various cost factors into the so-called ensemble factor. 
ensembleFactor(g)$i_hydroPeakingFactor(g)         = locationFactor(g) * (1 / i_hydroPeakingFactor(g) ) ; 
ensembleFactor(g)$( i_hydroPeakingFactor(g) = 0 ) = locationFactor(g) ;



* e) Transmission data.
* Let the last region declared be the slack bus (note that set r may not be ordered if users don't maintain unique set elements).
slackBus(r) = no ;
slackBus(r)$( ord(r) = card(r) ) = yes ;

* Define the lower triangular part of region-region matrix, i.e. ord(r) > ord(rr).
regLower(r,rr) = no ;
regLower(r,rr)$( ord(r) > ord(rr) ) = yes ;

* Define regions at each end of NI-SI HVDC link.
loop((Benmore(i),Haywards(ii)),
  nwd(r,rr)$( mapi_r(i,r) * mapi_r(ii,rr) ) = yes ;
  swd(r,rr)$( mapi_r(ii,r) * mapi_r(i,rr) ) = yes ;
) ;

* Define interisland pairings.
interIsland(ild,ild1)$( ord(ild) <> ord(ild1) ) = yes ;
interIslandRegions(r,rr)$( nwd(r,rr) or swd(r,rr) ) = yes ;

* Make sure i_txCapacityPO is not specified for anything but the current HVDC link.
i_txCapacityPO(r,rr,ps)$( not (nwd(r,rr) or swd(r,rr)) ) = 0 ;

* Make sure i_txEarlyComYr equals the first modelled year if it isn't already defined 
i_txEarlyComYr(tupg)$( i_txEarlyComYr(tupg) = 0 ) = firstYear ;
i_txEarlyComYr(tupg)$sameas(tupg,'exist') = 0 ;

* Make sure intraregional capacities and line characteristics are zero.
i_txCapacity(r,r,ps) = 0 ;    i_txCapacityPO(r,r,ps) = 0 ;
i_txResistance(r,r,ps) = 0 ;  i_txReactance(r,r,ps) = 0 ;

* Assign allowable transitions from one transmission state to another.
transitions(tupg,r,rr,ps,pss) = txUpgradeTransitions(tupg,r,rr,ps,pss) ;
transitions(tupg,rr,r,ps,pss)$txUpgradeTransitions(tupg,r,rr,ps,pss) = txUpgradeTransitions(tupg,r,rr,ps,pss) ;
* Now remove any illegitimate values from transitions.
transitions(tupg,r,rr,ps,pss)$( i_txFixedComYr(tupg) > lastYear ) = no ;
transitions(tupg,r,rr,ps,pss)$( i_txEarlyComYr(tupg) > lastYear ) = no ;
transitions(tupg,r,rr,ps,pss)$( i_txCapacity(r,rr,pss) = 0 )  = no ;
transitions(tupg,r,rr,ps,pss)$sameas(tupg,'exist') = no ;
transitions(tupg,r,rr,ps,pss)$sameas(pss,'initial') = no ;
transitions(tupg,r,r,ps,pss) = no ;
transitions(tupg,r,rr,ps,ps) = no ;

* Identify all possible states on all paths by recursively applying 'transitions'. First, kick things off
* by initialising the cases where a non-zero capacity is defined on existing paths.
allowedStates(r,rr,'initial')$i_txCapacity(r,rr,'initial') = yes ;
counter = 0 ;
repeat
  counter = card(allowedStates) ;
  allowedStates(r,rr,pss)$sum(transitions(tupg,r,rr,ps,pss)$allowedStates(r,rr,ps), 1 ) = yes ;
until counter = card(allowedStates) ;

* Count the allowed upgrade states for each active path.
numAllowedStates(r,rr) = sum(ps$allowedStates(r,rr,ps), 1 ) ;

* Identify all r-rr-ps tuples not in allowedStates.
notAllowedStates(r,rr,ps) = yes ;
notAllowedStates(allowedStates) = no ;

* Zero out transmission capacities for states not allowed (i.e. something other than capacity may have resulted in a null transition above).
i_txCapacity(notAllowedStates) = 0 ;
i_txCapacityPO(notAllowedStates) = 0 ;

* Identify and count all existing or potential interregional transmission paths.
paths(r,rr)$sum(allowedStates(r,rr,ps), 1 ) = yes ;
numPaths = card(paths) ;

* Identify all allowable states of upgrade on each path.
upgradeableStates(allowedStates(r,rr,ps))$( not sameas(ps,'initial') ) = yes ;

* Identify the last allowed transmission upgrade state on each path.
counter = 0 ;
loop(paths(r,rr),
  loop(allowedStates(paths,ps), counter = counter + 1 ; lastAllowedState(allowedStates)$( numAllowedStates(paths) = counter ) = yes ) ;
  counter = 0 ;
) ;

* Identify the allowable upgrade transition sequence for each valid transmission path.
validTransitions(paths(r,rr),ps,pss)$sum(transitions(tupg,r,rr,ps,pss), 1 ) = yes ;

* Assign earliest and fixed transmission upgrade years (let earliest year be the first year if no earliest year is specified).
txEarlyComYr(transitions(tupg,r,rr,ps,pss))$i_txEarlyComYr(tupg) = i_txEarlyComYr(tupg) ;
txEarlyComYr(transitions(tupg,rr,r,ps,pss))$txEarlyComYr(tupg,r,rr,ps,pss) = txEarlyComYr(tupg,r,rr,ps,pss) ;
txEarlyComYr(transitions)$( not txEarlyComYr(transitions) ) = firstYear ;
txEarlyComYr(tupg,paths,ps,pss)$( not transitions(tupg,paths,ps,pss) ) = 0 ;

txFixedComYr(transitions(tupg,r,rr,ps,pss))$i_txFixedComYr(tupg) = i_txFixedComYr(tupg) ;
txFixedComYr(transitions(tupg,rr,r,ps,pss))$txFixedComYr(tupg,r,rr,ps,pss) = txFixedComYr(tupg,r,rr,ps,pss) ;
txFixedComYr(tupg,paths,ps,pss)$( not transitions(tupg,paths,ps,pss) ) = 0 ;

* Transfer transmission capital cost from a project basis (tupg) to path (r-rr) basis. Apportion cost to each direction based on
* pro-rated transmission capcity in each direction. Convert the lumpy txCapitalCost ($m) to levelised TxCapCharge ($m/yr).
txCapitalCost(r,rr,ps) = sum(transitions(tupg,r,rr,pss,ps)$( i_txCapacity(r,rr,ps) or i_txCapacity(rr,r,ps) ),
                         i_txCapitalCost(tupg) * i_txCapacity(r,rr,ps) / ( i_txCapacity(r,rr,ps) + i_txCapacity(rr,r,ps) ) ) ;
txCapCharge(r,rr,ps,y) = txCapitalCost(r,rr,ps) * txCapRecFac(y) ;

* Identify transmission group constraints as valid if LHS and RHS coefficients are non-zero. 
validTGC(tgc)$( sum(p$i_txGrpConstraintsLHS(tgc,p), 1) * i_txGrpConstraintsRHS(tgc) ) = yes ;

* Calculate reactance and susceptance by year - this assumes exogenous or fixed timing of transmission expansion
* decisions, otherwise it stays at the level of initial year.
reactanceYr(paths,y) = i_txReactance(paths,'initial') ;
loop((tupg,paths,ps,pss)$txFixedComYr(tupg,paths,ps,pss),
  reactanceYr(paths,y)$( yearNum(y) >= txFixedComYr(tupg,paths,ps,pss) ) = i_txReactance(paths,pss) ;
) ;

susceptanceYr(paths,y)$reactanceYr(paths,y) = 1 / reactanceYr(paths,y) ;

* Assign bus-branch incidence and group constraint data.
loop(mapArcNode(p,r,rr),
  BBincidence(p,r)  =  1 ;
  BBincidence(p,rr) = -1 ;
) ;

* Compute slopes and intercepts for transmission loss functions - assume integerized BTX in first instance:
* Initialise the loss tranches set (i.e. number tranches = card(n) - 1).
trnch(n)$( ord(n) < card(n) ) = yes ;

* Determine capacity of each loss tranche, i.e. uniform between 0 and i_txCapacity(r,rr,ps). Note that there is no
* special reason why the segments must be of uniform sizes. Note too that the 'capacity by tranche' and 'loss by
* tranche' are only used in determining the intercepts and slopes - they play no explicit role in the model.
pCap(paths(r,rr),ps,n)$(card(n) - 1 ) = (ord(n) - 1) * i_txCapacity(paths,ps) / (card(n) - 1 ) ;

* Then use the quadratic loss function to compute losses at max capacity of each tranche.
pLoss(paths(r,rr),ps,n) = i_txResistance(paths,ps) * ( pCap(paths,ps,n)**2 ) ;

* Figure out the upper bound on losses, i.e. losses at max capacity of path in each state.
bigLoss(paths,ps) = smax(n, pLoss(paths,ps,n) ) ;

* Now compute the slope and intercept terms to be used in the GEM loss functions.
lossSlopeMIP(allowedStates(paths,ps),trnch(n))$[ (pCap(paths,ps,n+1) - pCap(paths,ps,n)) > eps ] =
  [pLoss(paths,ps,n+1) - pLoss(paths,ps,n)] / [pCap(paths,ps,n+1) - pCap(paths,ps,n)] ;

lossIntercept(paths(r,rr),ps,trnch(n)) = pLoss(paths,ps,n) - lossSlopeMIP(paths,ps,n) * pCap(paths,ps,n) ;

* Overwrite some of the above - pCap, pLoss, lossIntercept and both lossSlopeMIP and lossSlopeRMIP - if integerization of BTX is not to be employed.
if(txLossesRMIP,
* Accept values from above for the initial state, and populate slopes for all states using initial state slopes.
  pCap(paths,ps,n)$(not sameas(ps,'initial') ) = 0 ;
  pLoss(paths,ps,n)$(not sameas(ps,'initial') ) = 0 ;
  lossIntercept(paths,ps,n)$(not sameas(ps,'initial') ) = 0 ;
  lossSlopeRMIP(paths,n) = lossSlopeMIP(paths,'initial',n) ;

* Now loop over paths, states and loss tranches, iteratively computing pCap, pLoss and lossIntercept.
  loop((allowedStates(paths(r,rr),ps),n)$( (ord(n) > 1) and (not sameas(ps,'initial')) ),
    pCap(paths,ps,n) = ( lossSlopeRMIP(paths,n-1) +
                         sqrt( lossSlopeRMIP(paths,n-1)**2 + 4 * i_txResistance(paths,ps) * lossIntercept(paths,ps,n-1) ) ) / (2 * i_txResistance(paths,ps)) ;
    pLoss(paths,ps,n) = i_txResistance(paths,ps) * pCap(paths,ps,n)**2 ;
    lossIntercept(paths,ps,n)$( ord(n) < card(n) ) = pLoss(paths,ps,n) - lossSlopeRMIP(paths,n) * pCap(paths,ps,n) ;
  ) ;
* Finally, assign the RMIP slopes (same for all states) to the MIP slopes parameter - this makes the reporting further below robust across either assumption.
lossSlopeMIP(paths,ps,n)$lossSlopeMIP(paths,ps,n) = lossSlopeRMIP(paths,n) ;
) ;


* f) Reserve energy data.
reservesAreas(rc) = min(2, max(1, i_ReserveAreas(rc) ) ) ;

singleReservesReqF(rc)$( reservesAreas(rc) = 1 ) = 1 ;

penaltyViolateReserves(ild,rc) = max(0, i_ReservePenalty(ild,rc) ) ;

windCoverPropn(rc) = min(1, max(0, i_propWindCover(rc) ) ) ;

bigM(ild1,ild) =
 smax((paths(r,rr),ps)$( mapild_r(ild1,r) * mapild_r(ild,rr) ), i_txCapacity(paths,ps) ) -
 smin((paths(r,rr),ps)$( mapild_r(ild1,r) * mapild_r(ild,rr) ), i_txCapacityPO(paths,ps) ) ;


* g) Non-free reserves
* Estimate free reserves by path state.
freeReserves(nwd(r,rr),ps)$allowedStates(nwd,ps) = i_txCapacityPO(nwd,ps) + largestNIplant ;
freeReserves(swd(r,rr),ps)$allowedStates(swd,ps) = i_txCapacityPO(swd,ps) + largestSIplant ;

* Estimate non-free reserves by path state.
nonFreeReservesCap(paths(r,rr),ps)$( allowedStates(paths,ps) and (nwd(paths) or swd(paths)) ) = i_txCapacity(paths,ps) - freeReserves(paths,ps) ;

* Figure out capacities (really, upper bounds) of non-free reserves by level.
* i) Find the biggest value in each direction
bigSwd(swd) = smax(ps, nonFreeReservesCap(swd,ps)) ;
bigNwd(nwd) = smax(ps, nonFreeReservesCap(nwd,ps)) ;

* ii) Set the first level to be 100
pNFresvCap(paths(r,rr),lvl)$( nwd(paths) or swd(paths) ) = 100 ;

* iii) Set subsequent levels to be 100 more than the previous level
loop(lvl$( ord(lvl) > 1),
  pNFresvCap(paths(r,rr),lvl)$( ord(lvl) > 1 and (nwd(paths) or swd(paths)) ) = pNFresvCap(paths,lvl-1) + 100 ;
) ;

* iv) Set the last level to be the biggest value over all states
pNFresvCap(swd,lvl)$( ord(lvl) = card(lvl) ) = bigswd(swd) ;
pNFresvCap(nwd,lvl)$( ord(lvl) = card(lvl) ) = bignwd(nwd) ;

* Figure out costs by level - increment by $5/MWh each level.
pNFresvCost(paths(r,rr),lvl)$( (ord(lvl) = 1 ) and (nwd(paths) or swd(paths)) ) = 5 ;
loop(lvl$( ord(lvl) > 1),
  pNFresvCost(paths(r,rr),lvl)$( ord(lvl) > 1 and (nwd(paths) or swd(paths)) ) = pNFresvCost(paths,lvl-1) + 5 ;
) ;


* h) Create set of VOLL plant - one per region.
put VOLLplant
'Set s /'           loop(r, put / "'VOLL" r.tl "'" ) ; put '  /;' //
'Set maps_r(s,r) /' loop(r, put / "'VOLL" r.tl "'.'" r.tl "'" ) ; put '  /;' ;



*===============================================================================================
* 4. Prepare the scenario-dependent input data; key user-specified settings are obtained from GEMstochastic.inc.

$include GEMstochastic.inc

* Pro-rate weightScenariosBySet values so that weights sum to exactly one for each scenarioSets:
counter = 0 ;
loop(scenSet,
  counter = sum(scen, weightScenariosBySet(scenSet,scen)) ;
  weightScenariosBySet(scenSet,scen)$counter = weightScenariosBySet(scenSet,scen) / counter ;
) ;

* Compute the short-run marginal cost (and its components) for each generating plant, $/MWh.
totalFuelCost(g,y,scen) = 1e-3 * scenarioFuelCostFactor(scen) * i_heatrate(g) * sum(mapg_f(g,f), i_fuelPrices(f,y) + i_varFuelCosts(g) ) ;

CO2taxByPlant(g,y,scen) = 1e-9 * i_heatrate(g) * sum((mapg_f(g,f),mapg_k(g,k)), i_co2tax(y) * scenarioCO2TaxFactor(scen) * i_emissionFactors(f) ) ;

SRMC(g,y,scen) = i_varOM(g) + totalFuelCost(g,y,scen) + CO2taxByPlant(g,y,scen) ;

* If SRMC is zero or negligible (< .05) for any plant, assign a positive small value.
SRMC(g,y,scen)$( SRMC(g,y,scen) < .05 ) = 1e-3 * ord(g) / card(g) ;

* Capture the island-wide AC loss adjustment factors.
AClossFactors('ni') = %AClossesNI% ;
AClossFactors('si') = %AClossesSI% ;

* Transfer i_NrgDemand to NrgDemand and adjust for intraregional AC transmission losses and the scenario-specific energy factor.
NrgDemand(r,y,t,lb,scen) = sum(mapild_r(ild,r), (1 + AClossFactors(ild)) * i_NrgDemand(r,y,t,lb)) * scenarioNRGfactor(scen) ;

* Use the GWh of NrgDemand and hours per LDC block to compute ldcMW (MW).
ldcMW(r,y,t,lb,scen)$hoursPerBlock(t,lb) = 1e3 * NrgDemand(r,y,t,lb,scen) / hoursPerBlock(t,lb) ;

* Calculate peak load as peak:average ratio and adjust by the scenario-specific peak load factor.
peakLoadNZ(y,scen) = scenarioPeakLoadFactor(scen) * i_P200ratioNZ(y) * ( 1 / 8.76 ) * sum((r,t,lb)$mapAggR_r('nz',r), NrgDemand(r,y,t,lb,scen)) ;
peakLoadNI(y,scen) = scenarioPeakLoadFactor(scen) * i_P200ratioNI(y) * ( 1 / 8.76 ) * sum((r,t,lb)$mapAggR_r('ni',r), NrgDemand(r,y,t,lb,scen)) ;

* Transfer hydro output for all historical hydro years from i_historicalHydroOutput to historicalHydroOutput (no scenario-specific adjustment factors at this time).
historicalHydroOutput(v,hY,m) = i_historicalHydroOutput(v,hY,m) ;



*===============================================================================================
* 5. Ascertain which dispatch solves ought to be summed and averaged for reporting purposes, i.e. pick out all cases where the scenario
*    to scenarioSet mapping is one-to-one and exists solely for the purpose of introducing variability in hydrology. 

loop((allSolves(experiments,steps,scenSet),scenarios,hydroSeqTypes,hY)$
      (
*       Condition: steps = dispatch; hydro sequence type = sequential; and sequential sequence types are mapped to scenarios
        sameas(steps,'dispatch') * sameas(hydroSeqTypes,'sequential') * mapSC_hydroSeqTypes(scenarios,hydroSeqTypes) *
*       Condition: scenarios are mapped to scenario sets; and the weighting on each scenario = 1
        mapScenarios(scenSet,scenarios) * (weightScenariosBySet(scenSet,scenarios) = 1) *
*       Condition: the scenario has historical hydro years mapped to it
        mapSC_hY(scenarios,hY)
      ),

  figureOutAvgDispatch(experiments,steps,scenSet,scenarios,hydroSeqTypes,hY) = yes ;

  allAvgDispatchSolves(experiments,steps,scenSet) = yes ;

) ;

allNotAvgDispatchSolves(experiments,steps,scenSet)$allSolves(experiments,steps,scenSet) = yes ;

allNotAvgDispatchSolves(experiments,steps,scenSet)$allAvgDispatchSolves(experiments,steps,scenSet) = no ;

* Use the figureOutAvgDispatch set to write a temporary file that GEMsolve includes and then dumps into the selected prepared input data GDX. 
file tempSets / tempSets.inc / ; tempSets.lw = 0 ; put tempSets
  "Set repSteps 'Steps in an experiment' / timing, reopt, dispatch, avgDispatch / ;" //
  'Set avgDispatchSteptoRepStep(experiments,repSteps,steps,scenSet,scenarios) "Map reporting steps to steps to accomodate dispatch solves to be averaged" /'
loop(figureOutAvgDispatch(experiments,steps,scenSet,scenarios,hydroSeqTypes,hY),
  put / '  ' experiments.tl '.avgDispatch.' steps.tl, '.' scenSet.tl, '.' scenarios.tl ;
) ; put ' /;' ;



*===============================================================================================
* 6. Display sets and parameters.

$ontext 

** This piece of code has not kept pace with all the changes to GEMdata.

Display
* Sets
* Time/date-related sets and parameters.
  firstYr, lastYr, allButFirstYr, firstPeriod
* Various mappings, subsets and counts.
  mapg_k, mapg_f, mapg_o, mapg_i, mapg_r, mapg_e, mapg_ild, mapi_r, mapi_e, mapild_r, mapv_g, thermalFuel
* Financial parameters.
* Fuel prices and quantity limits.
* Generation data.
  noExist, nigen, sigen, schedHydroPlant, pumpedHydroPlant, moverExceptions, validYrBuild, integerPlantBuild, linearPlantBuild
  possibleToBuild, possibleToRefurbish, possibleToEndogRetire, possibleToRetire, endogenousRetireDecisnYrs, endogenousRetireYrs, validYrOperate
* Load data.
* Transmission data.
  slackBus, regLower, interIsland, nwd, swd, paths, transitions, validTransitions, allowedStates, notAllowedStates
  upgradeableStates, validTGC, trnch,
* Reserve energy data.
* Parameters
* Time/date-related sets and parameters.
  counter, yearNum, hydroYearNum, hoursPerBlock
* Various mappings, subsets and counts.
  numReg
* Financial parameters.
  discountRates, PVfacG, PVfacT, PVfacsM, PVfacsEY, PVfacs, capexLife, annuityFacN, annuityFacR, txAnnuityFacN, txAnnuityFacR
  capRecFac, depTCrecFac, txCapRecFac, txDeptCRecFac
* Fuel prices and quantity limits.
* Generation data.
  initialCapacity, capexPlant, capCharge, refurbCapexPlant, refurbCapCharge, exogMWretired, continueAftaEndogRetire
  WtdAvgFOFmultiplier, reservesCapability, peakConPlant, NWpeakConPlant, maxCapFactPlant, minCapFactPlant
* Load data.
* Transmission data.
  txEarlyComYr, txFixedComYr, reactanceYr, susceptanceYr, BBincidence, pCap, pLoss, bigLoss, lossSlopeMIP, lossSlopeRMIP, lossIntercept
  txCapitalCost, txCapCharge
* Reserve energy data.
  reservesAreas, penaltyViolateReserves, windCoverPropn, bigM
  i_inflexiblePlantFactor, rightAdjacentBlocks
  ;
$offtext



*===============================================================================================
* 7. Create input data summaries.

* a) Declare input data summary files.
Files
  rvs            / "%OutPath%\%runName%\runVersions.txt" /
  GEMrepConfig   / "%OutPath%\%runName%\Input data checks\Configuration info for use in GEMreports - %runName%_%runVersionName%.inc" /
  rvConfig       / "%OutPath%\%runName%\Input data checks\Run version configuration summary - %runName%_%runVersionName%.txt" /
  txData         / "%OutPath%\%runName%\Input data checks\Transmission summary - %runName%_%runVersionName%.txt" /
  plantData      / "%OutPath%\%runName%\Input data checks\Plant summary - %runName%_%runVersionName%.txt" /
  capexStats     / "%OutPath%\%runName%\Input data checks\Capex, MW and GWh summaries - %runName%_%runVersionName%.txt" /
  loadSummary    / "%OutPath%\%runName%\Input data checks\Load summary - %runName%_%runVersionName%.txt" /
  lrmc_inData    / "%OutPath%\%runName%\Input data checks\LRMC estimates based on GEM input data (non-existing plant only) - %runName%_%runVersionName%.csv" / ;

rvs.lw = 0 ;            rvs.ap = 1 ;
GEMrepConfig.lw = 0 ;   GEMrepConfig.pw = 999 ;
rvConfig.lw = 0 ;       rvConfig.pw = 999 ;
txData.lw = 0 ;         txData.pw = 999 ;
plantData.lw = 0 ;      plantData.pw = 999 ;
capexStats.lw = 0 ;     capexStats.pw = 999 ;
loadSummary.lw = 0 ;    loadSummary.pw = 999 ;
lrmc_inData.pc = 5 ;    lrmc_inData.nd = 1 ;


* b) Do the calculations.
numExperiments  = sum(experiments$sum(allSolves(experiments,steps,scenSet), 1), 1) ;
numSteps        = sum(steps$sum(allSolves(experiments,steps,scenSet), 1), 1) ;
numScenarioSets = sum(scenSet$sum(allSolves(experiments,steps,scenSet), 1), 1) ;
numScenarios    = sum(scen$sum((allSolves(experiments,steps,scenSet),mapScenarios(scenSet,scen)), 1), 1) ;

loop(lb,
  xFoFm(g)$( sum(mapg_k(g,k), WtdAvgFOFmultiplier(k,lb)) > 1.5 ) = yes ;
  xFoFm(g)$( sum(mapg_k(g,k), WtdAvgFOFmultiplier(k,lb)) < 0.5 ) = yes ;
) ;
avgPeakCon(g) = sum(y, peakConPlant(g,y)) / card(y) ;
avgMaxCapFact(g) = sum((t,lb), hoursPerBlock(t,lb) * maxCapFactPlant(g,t,lb)) / sum((t,lb), hoursPerBlock(t,lb)) ;
avgMinCapFact(g) = sum((y,t),  minCapFactPlant(g,y,t)) / ( card(y) * card(t) ) ;
avgMinUtilisation(g) = sum(y, i_minUtilisation(g,y)) / card(y) ;

assumedGWh(g) = sum(mapg_k(g,k), 8.76 * i_capFacTech(k) * i_nameplate(g)) ;
assumedGWh(pumpedHydroPlant(g)) = i_PumpedHydroEffic(g) * sum(mapm_t(m,t), 1) * i_PumpedHydroMonth(g) ;

MWtoBuild(k,aggR)  = sum((possibleToBuild(g),r)$( mapg_k(g,k) * mapg_r(g,r) * mapAggR_r(aggR,r) ), i_nameplate(g)) ;
GWhtoBuild(k,aggR) = sum((possibleToBuild(g),r)$( mapg_k(g,k) * mapg_r(g,r) * mapAggR_r(aggR,r) ), assumedGWh(g)) ;

loop(defaultScenario(scen),
* NB: defaultScenario comes from GEMstochastic.inc.

  avgSRMC(g) = sum(y, SRMC(g,y,scen)) / card(y) ;

  loadByRegionYear(r,y) = sum((t,lb), NrgDemand(r,y,t,lb,scen)) ;
  loadByAggRegionYear(aggR,y) = sum(mapAggR_r(aggR,r), loadByRegionYear(r,y)) ; 

  peakLoadByYearAggR(y,'nz') = peakLoadNZ(y,scen) ;
  peakLoadByYearAggR(y,'ni') = peakLoadNI(y,scen) ;
  peakLoadByYearAggR(y,'si') = peakLoadByYearAggR(y,'nz') - peakLoadByYearAggR(y,'ni') ;

) ;

capexStatistics(k,aggR,'count') = sum((g,r)$( mapg_k(g,k) * mapg_r(g,r) * mapAggR_r(aggR,r) * possibleToBuild(g) ), 1 ) ; 
capexStatistics(k,aggR,'min')$capexStatistics(k,aggR,'count')   = smin((g,r)$( mapg_k(g,k) * mapg_r(g,r) * mapAggR_r(aggR,r) * possibleToBuild(g) ), 1e-3 * capexPlant(g) ) ; 
capexStatistics(k,aggR,'max')$capexStatistics(k,aggR,'count')   = smax((g,r)$( mapg_k(g,k) * mapg_r(g,r) * mapAggR_r(aggR,r) * possibleToBuild(g) ), 1e-3 * capexPlant(g) ) ; 
capexStatistics(k,aggR,'range')$capexStatistics(k,aggR,'count') = capexStatistics(k,aggR,'max') - capexStatistics(k,aggR,'min') ;
capexStatistics(k,aggR,'mean')$capexStatistics(k,aggR,'count')  =
  sum((g,r)$( mapg_k(g,k) * mapg_r(g,r) * mapAggR_r(aggR,r) * possibleToBuild(g) ), 1e-3 * capexPlant(g) ) / capexStatistics(k,aggR,'count') ; 
capexStatistics(k,aggR,'variance')$capexStatistics(k,aggR,'count') =
  sum((g,r)$( mapg_k(g,k) * mapg_r(g,r) * mapAggR_r(aggR,r) * possibleToBuild(g) ), sqr(1e-3 * capexPlant(g) - capexStatistics(k,aggR,'mean')) ) / capexStatistics(k,aggR,'count') ; 
capexStatistics(k,aggR,'stdDev') = sqrt(capexStatistics(k,aggR,'variance')) ;
capexStatistics(k,aggR,'stdDev%')$capexStatistics(k,aggR,'mean') = 100 * capexStatistics(k,aggR,'stdDev') / capexStatistics(k,aggR,'mean') ;


* c) Write miscellaneous configuration information required later by GEMreports.
put GEMrepConfig ;
put 'Set scenarios "The various individual stochastic scenarios, or futures, or states of uncertainty" /' ;
loop(scen$sum((allSolves(experiments,steps,scenSet),mapScenarios(scenSet,scen)), 1), put / '  "' scen.tl, '" "', scen.te(scen), '"' ) put / '  "averageDispatch" /;' // ;  

put 'Set defaultScenario(scenarios) "Identify a default scenario to use when reporting input data summaries. Applies only to input data defined over scenarios (see GEMdata)" /' ;
loop(defaultScenario(scen), put / '  "' scen.tl '"' ) put ' /;' // ;  

put 'Set scenarioSets "Sets of scenarios to be used in the same solve" /' ;
loop(scenSet$sum(allSolves(experiments,steps,scenSet), 1), put / '  "' scenSet.tl, '" "', scenSet.te(scenSet), '"' ) put / '  "avg" /;' // ;  

put 'Set experiments "A collection of experiments, each potentially containing timing, re-optimisation and dispatch steps" /' ;
loop(experiments$sum(allSolves(experiments,steps,scenSet), 1), put / '  "' experiments.tl, '" "', experiments.te(experiments), '"' ) put ' /;' // ;  

put 'Set mapScenarios(scenarioSets,scenarios) "Map each scenario to a scenarioSet (i.e. 1 or more scenarios make up an scenario set)" /' ;
loop(mapScenarios(scenSet,scen), put / '  "' scenSet.tl, '"."', scen.tl, '"' ) put / '  "avg"."averageDispatch" /;' // ;

put 'Parameter weightScenariosBySet(scenarioSets,scenarios) "Assign weights to the scenarios comprising each set of scenarios" /' ;
loop((scenSet,scen)$weightScenariosBySet(scenSet,scen), put / '  "' scenSet.tl, '"."', scen.tl, '"  ', weightScenariosBySet(scenSet,scen):<10:8 ) put / '  "avg"."averageDispatch" 1 /;' // ;

put '$setglobal firstYear %firstYear%' / '$setglobal lastYear %lastYear%' //
    'Scalar taxRate / ',                taxRate:<5:2, ' /;' /
    'Scalar VOLLcost / ',               VOLLcost:<10:1, ' /;' /
    'Scalar penaltyViolatePeakLoad / ', penaltyViolatePeakLoad:<10:1, ' /;' /
    'Scalar penaltyViolateRenNrg / ',   penaltyViolateRenNrg:<10:1, ' /;' /
    'Scalar slackCost / ',              slackCost:<10:1, ' /;' ;


* d) Write the run configuration summary.
put rvConfig 'Run name:' @26 "%runName%" / 'Run version:' @26 "%runVersionName%" / 'Initiated at:' @26 system.time ' on ' system.date //
  'Main input GDX:' @26 "%DataPath%\%GEMinputGDX%" / 'Region/network GDX:' @26 "%DataPath%\%GEMnetworkGDX%" / 'Demand GDX:' @26 "%DataPath%\%GEMdemandGDX%" / 
$ if %useOverrides%==0 $goto noOverrides3
  'Override GDX:' @26 "%DataPath%\%GEMoverrideGDX%" / 
$ label noOverrides3
$ if %GRscheduleRead%==0 $goto noGRschedule
  'Build schedule file:' @26 "%GRscheduleFile%" / 
$ label noGRschedule
 /'Number of experiments:'   @26 numExperiments:<3:0  '-- ' loop(experiments$sum(allSolves(experiments,steps,scenSet), 1), put experiments.tl:<15 ) put /
  'Number of steps:'         @26 numSteps:<3:0        '-- ' loop(steps$sum(allSolves(experiments,steps,scenSet), 1), put steps.tl:<15 ) ; counter = 0 put /
  'Number of scenario sets:' @26 numScenarioSets:<3:0 '-- ' loop(scenSet$sum(allSolves(experiments,steps,scenSet), 1),
                                                            counter = counter + 1 if((mod(counter,10) + 1) < 10, put scenSet.tl:<15 else put / @32 scenSet.tl:<15 ) ) ; counter = 0 put /
  'Number of scenarios:'     @26 numScenarios:<3:0    '-- ' loop(scen$sum((allSolves(experiments,steps,scenSet),mapScenarios(scenSet,scen)), 1),
                                                            counter = counter + 1 if((mod(counter,10) + 1) < 10, put scen.tl:<15 else put / @32 scen.tl:<15 ) ) put /
  'Number of solves:'        @26 card(allSolves):<6:0 'For each solve, the scenarios are mapped to the experiment-step-scenarioSet groupings as follows:' / @29 '#  '
  'Experiments' @45 'Steps'  @55 'scenarioSets' @73 'Scenarios' ;
  counter = 0 ;
  loop(allSolves(experiments,steps,scenSet),
    counter = counter + 1 ;
    put / @29 counter:<3:0 experiments.tl @45 steps.tl @55 scenSet.tl @69 '<-- ' ;
    loop(scen$mapScenarios(scenSet,scen), put scen.tl ' ' )
  ) ; put //
  'Default scenario:' @26 loop(defaultScenario(scen), put scen.tl ) put ' - only used for input data reporting' //
  'Switches' /
  'Integerized loss functions:'           @40 if(txLossesRMIP,         put 'no'  else put 'yes' ) put /
  'Use V2G generation plant:'             @40 if(V2GtechnologyOn,      put 'yes' else put 'no' ) put /
  'Renewable energy share constraint:'    @40 if(renNrgShrOn,          put 'on'  else put 'off' ) put /
  'Renewable capacity share constraint:'  @40 if(renCapShrOn,          put 'on'  else put 'off' ) put /
  "NI 'no wind' peak constraint:"         @40 if(niNWpeakCnstrntOn,    put 'on'  else put 'off' ) put /
  'Limit energy by fuel constraint:'      @40 if(limitNrgByFuelOn,     put 'on'  else put 'off' ) put /
  'Full-blown reserves formulation:'      @40 if(reservesOn,           put 'on'  else put 'off' ) put /
  'DC load flow formulation:'             @40 if(DCloadFlowOn,         put 'on'  else put 'off' ) put /
  'Write out a GR build schedule:'        @40 if(GRscheduleWrite,      put 'yes' else put 'no' ) put /
  'Read/enforce a GR build schedule:'     @40 if(%GRscheduleRead% = 0, put 'no'  else put 'yes' ) put /
  'Compute LRMCs from input data:'        @40 if(%calcInputLRMCs% = 0, put 'no'  else put 'yes' ) put //
  'Miscellaneous parameters' /
  'Modelled time horizon:'                @40 firstYear:<4:0 '-' lastYear:<4:0 /
  'Corporate tax rate (%):'               @40 (100 * taxRate):<4:1 /
  'Generation WACC (%):'                  @40 (100 * WACCg):<4:1 /
  'Transmission WACC (%):'                @40 (100 * WACCt):<4:1 /
  'Depreciation rate for Tx kit (%):'     @40 (100 * txDepRate):<4:1 /
  'Life of transmission kit, years:'      @40 txPlantLife:<3:0 /
  'Annual MW build limit:'                @40 AnnualMWlimit:<5:0 /
  'Number of VOLL plant:'                 @40 (card(r)):<3:0 /
  'Capacity of all VOLL plant, MW:'       @40 VOLLcap:<5:0 /
  'Cost of all VOLL plant, $/MWh:'        @40 VOLLcost:<6:0 /
  'Number of loss tranches:'              @40 numT:<3:0 /
  'AC loss adjustment (to load), NI (%):' @40 (100 * %AClossesNI%):<5:2 /
  'AC loss adjustment (to load), SI (%):' @40 (100 * %AClossesSI%):<5:2 //
  'Penalties' /
  'Violate peak constraints, $/MW??:'     @40 penaltyViolatePeakLoad:<7:0 /
  'Violate renewable NRG target, $/MWh:'  @40 penaltyViolateRenNrg:<7:0 /
  ;
* Other GEMsettings stuff not yet written out.
*$setglobal RunType 1
*$setglobal GEMtype RMIP
*$setglobal DISPtype RMIP
*+++ Hydrology +++
*Scalar hydroOutputScalar / .97 / ;
*+++ CapitalExpenditure +++
*Scalar discRateLow  / .04 / ;
*Scalar discRateMed  / .07 / ;
*Scalar discRateHigh / .10 / ;
*Scalar depType / 1 / ;
*Scalar randomCapexCostAdjuster / 0 / ;
*+++ GemConstraints +++
*Scalar cGenYr / 2025 / ;
*Scalar noRetire / 2 / ;
*Scalar slackCost / 9999 / ;
*Scalar noVOLLblks / 0 / ;
*+++ Solver +++
*$setglobal Solver Cplex
*$setglobal optcr .0075
*$setglobal CPUsecsGEM 20000
*$setglobal Threads 4
*$setglobal limitOutput 0

put  /// 'Scenario weights and factors by experiment.' / @54 'Scenario' @66 'Scenario factors:' /
'Experiments' @15 'Steps' @25 'scenarioSets' @40 'Scenarios' @55 'Weight' @66 'PeakLoad' @76 'Co2' @86 'FuelCost' @96 'Energy' ;
loop(allSolves(experiments,steps,scenSet),
  put / experiments.tl @15 steps.tl @25 scenSet.tl @40
  loop(mapScenarios(scenSet,scen),
    put scen.tl @50 weightScenariosBySet(scenSet,scen):10:2, scenarioPeakLoadFactor(scen):10:2
      scenarioCO2TaxFactor(scen):10:2, scenarioFuelCostFactor(scen):10:2, scenarioNRGFactor(scen):10:2 ;
  ) ;
) ;

put //// 'Hydrology mappings to scenarios and type of sequence development (Same => averaged over the listed historical hydro years; Sequential => listed historical hydro year maps to first modelled year).' /
'Experiments' @15 'Steps' @25 'scenarioSets' @40 'Scenarios' @56 'SeqType' @68 'Hydro years' ;
loop(allSolves(experiments,steps,scenSet),
  put / experiments.tl @15 steps.tl @25 scenSet.tl @40
  loop(mapScenarios(scenSet,scen),
    put scen.tl @56 loop(mapSC_hydroSeqTypes(scen,hydroSeqTypes), put hydroSeqTypes.tl:<12 ) ; counter = 0 ;
    loop(mapSC_hY(scen,hY),
      counter = counter + 1 ;
      if((mod(counter,16) + 1) < 16, put hY.tl:<5 else put / @68 hY.tl:<5 ) ;
    ) ;
  ) ;
) ;

put //// 'Alternative view of scenario mappings to experiment-step-scenarioSet groups for each solve' / 'Experiments' @15 'Steps' @24 'Scenario sets'
loop(experiments$sum(allSolves(experiments,steps,scenSet), 1),
  put / experiments.tl ;
  loop(steps$sum(allSolves(experiments,steps,scenSet), 1),
    put / @15 steps.tl ;
    loop(allSolves(experiments,steps,scenSet),
      put / @24 scenSet.tl ' <-- ' loop(scen$mapScenarios(scenSet,scen), put scen.tl ' ' ) ;
    ) ;
  ) ;
) ;


* e) Write the transmission data summaries.
put txData, 'Transmission data summarised (default scenario only) - based on user-supplied data and the machinations of GEMdata.gms.' //
  'Network file:'          @26 "%GEMnetworkGDX%" /
  'Integerized losses:'    @26 if(txLossesRMIP, put 'no' else put 'yes' ) put /
  'Regions:'               @26 numReg:<4:0 /
  'Paths:'                 @26 numPaths:<4:0 /
  'Allowed states:'        @26 card(allowedStates):<4:0 /
  'Transitions:'           @26 card(transitions):<4:0 /
  'Loss tranches:'         @26 numT:<4:0 //
  'All scenarios:'         @26 loop(scen, put scen.tl ', ' ) put /
  'Default scenario:'      @26 loop(defaultScenario(scen), put scen.tl ) put //
  'First modelled year:'   @26 firstYear:<4:0 /
  'Last modelled year:'    @26 lastYear:<4:0 //
  'Note that capacity by loss tranche is only used in computation of slope and intercept terms. It plays no explicit role in the model' // @107
  "Loss function parameters by tranche (for the 'from' state only)" / @56
  'Capacity, MW:' @80 'Capacity (PO), MW:' @107 'Intercepts:' @(107+14*numT) 'Slopes:' @(107+28*numT) 'Max capacity, MW:' / @18
  '-- State --' @35 '-- Year --' @56 'From state' @68 'To state' @80 'From state' @92 'To state' @107
  'From region' @(107+7*numT) 'To region' @(107+14*numT) 'From region' @(107+21*numT) 'To region' @(107+28*numT) 'From region' @(107+35*numT) 'To region' / @3
  'Project' @18 'From' @26 'To' @35 'Early Fixed      $m   -->   <--   -->   <--   -->   <--   -->   <--' @102 ;
   counter = 0 ; repeat counter = counter + 1 loop(trnch(n), put n.tl:>7 ) until counter = 6 put '    Project description' // ;
loop((r,rr)$( ( ord(r) > ord(rr) ) and sum((tupg,ps,pss), transitions(tupg,r,rr,ps,pss)) ),
  put 'Path: ' r.tl ' <--> ' rr.tl ;
  loop(transitions(tupg,r,rr,ps,pss),
    put / @3 tupg.tl:<15, ps.tl:<8, pss.tl:<8, txEarlyComYr(tupg,r,rr,ps,pss):>6:0 ;
    if(txFixedComYr(tupg,r,rr,ps,pss), put txFixedComYr(tupg,r,rr,ps,pss):>6:0 else put '     -' ) ;
    put (txCapitalCost(r,rr,pss) + txCapitalCost(rr,r,pss) ):>8:1, i_txCapacity(r,rr,ps):>6:0, i_txCapacity(rr,r,ps):>6:0, i_txCapacity(r,rr,pss):>6:0, i_txCapacity(rr,r,pss):>6:0 ;
    if(i_txCapacityPO(r,rr,ps),  put i_txCapacityPO(r,rr,ps):>6:0  else put '     -' )  if(i_txCapacityPO(rr,r,ps),  put i_txCapacityPO(rr,r,ps):>6:0  else put '     -' )
    if(i_txCapacityPO(r,rr,pss), put i_txCapacityPO(r,rr,pss):>6:0 else put '     -' )  if(i_txCapacityPO(rr,r,pss), put i_txCapacityPO(rr,r,pss):>6:0 else put '     -' )
    loop(trnch(n), put lossIntercept(r,rr,ps,n):>7:1 )    loop(trnch(n), put lossIntercept(rr,r,ps,n):>7:1 )
    loop(trnch(n), put lossSlopeMIP(r,rr,ps,n):>7:3 )     loop(trnch(n), put lossSlopeMIP(rr,r,ps,n):>7:3 )
    loop(n$(ord(n)<card(n)), put pCap(r,rr,ps,n+1):>7:0 ) loop(n$(ord(n)<card(n)), put pCap(rr,r,ps,n+1):>7:0 )
    put '    ' tupg.te(tupg)  ;
  ) put // ;
) ;
put // 'Count of allowed states (including initial state) on each path:' 
loop((r,rr)$( ( ord(r) > ord(rr) ) and sum((tupg,ps,pss), transitions(tupg,r,rr,ps,pss)) ),
  put / r.tl ' <--> ' rr.tl ' -- ' numAllowedStates(r,rr):<2:0 'allowable states.' ;
) ;
put /// "A list dump of key transmission data (except for regions, 'From/Fr' and 'To' refers to the state):" /
  'FromReg' @11 'ToRegion' @21 'FromState' @31 'ToState' @42 'FrCapMW' @50 'ToCapMW' @59 'FrResist' @69 'ToResist' @81 'n' @83 'FrIntcept' @93 'ToIntcept' @105
  'FrSlope' @115 'ToSlope' @124 'FRpCap' @132 'TOpCap' @139 'FrBigLoss' @149 'ToBigLoss' @159 'FRSqrLos' @168 'TOSqrLos' @177 'FrLinLos' @186 'ToLinLos' @195
  'FrLoss%' @203 'ToLoss%' @214 'Project description'  ;
loop((transitions(tupg,r,rr,ps,pss),trnch(n)),
  put / r.tl:<10, rr.tl:<10, ps.tl:<10, pss.tl:<10, i_txCapacity(r,rr,ps):8:0, i_txCapacity(r,rr,pss):8:0, i_txResistance(r,rr,ps):10:6, i_txResistance(r,rr,pss):10:6
  n.tl:>5, lossIntercept(r,rr,ps,n):10:3, lossIntercept(r,rr,pss,n):10:3, lossSlopeMIP(r,rr,ps,n):10:5, lossSlopeMIP(r,rr,pss,n):10:5, pCap(r,rr,ps,n+1):8:1, pCap(r,rr,pss,n+1):8:1, 
  bigLoss(r,rr,ps):10:2, bigLoss(r,rr,pss):10:2, pLoss(r,rr,ps,n+1):9:2, pLoss(r,rr,pss,n+1):9:2, (lossIntercept(r,rr,ps,n) + lossSlopeMIP(r,rr,ps,n) * pCap(r,rr,ps,n+1)):9:2
 (lossIntercept(r,rr,pss,n) + lossSlopeMIP(r,rr,pss,n) * pCap(r,rr,pss,n+1)):9:2, (100 * pLoss(r,rr,ps,n+1) / pCap(r,rr,ps,n+1) ):8:2, (100 * pLoss(r,rr,pss,n+1) / pCap(r,rr,pss,n+1) ):8:2
  @214 tupg.tl @229 tupg.te(tupg)
) ;
* Might also consider writing allowedStates(r,rr,ps), notAllowedStates(r,rr,ps), upgradeableStates(r,rr,ps), validTransitions(r,rr,ps,pss)
*   reactanceYr(r,rr,y), susceptanceYr(r,rr,y), mapArcNode(p,r,rr), BBincidence(p,r), validTGC(tgc), and txCapCharge(r,rr,ps,y).
* NB: Reactance and susceptance by year assumes exogenous or fixed timing of transmission expansion decisions, otherwise it stays at the level of initial year.


* f) Write the plant data summaries.
$set plantDataHdr1 'MW  Capex  varCC  varOM avSRMC  fixOM fixFDC TCsclr     HR  PkCon    FoF  xFoFm mnCapF mxCapF  avMnU  '
$set plantDataHdr2 'Exist noExst Commit New NvaBld ErlyYr FixYr inVbld inVopr Retire EndogY ExogYr  Mover Region Owner  SubStn' ;
put plantData, 'Plant data summarised (default scenario only) - based on user-supplied data and the machinations of GEMdata.gms.' //
  'All scenarios:'                      @38 loop(scen, put scen.tl ', ' ) put /
  'Default scenario:'                   @38 loop(defaultScenario(scen), put scen.tl ) put //
  'Modelled time horizon:'              @38 firstYear:<4:0 '-' lastYear:<4:0 //
  'Summary counts' /
  'Plant in input file:'                @38 card(g):<4:0 /
  'Existing plant:'                     @38 card(exist):<4:0 /
  'Existing plant able to be retired:'  @38 card(possibleToRetire):<4:0 /
  'Committed plant:'                    @38 card(commit):<4:0 /
  'Plant not able to be built:'         @38 card(neverBuild):<4:0 /
  'Plant able to be built:' /
  '  North Island'                      @38 put sum(mapg_ild(g,'ni')$possibleToBuild(g), 1):<4:0 /
  '  South Island'                      @38 put sum(mapg_ild(g,'si')$possibleToBuild(g), 1):<4:0 /
  'MW able to be built:' /
  '  North Island'                      @38 put sum(mapg_ild(g,'ni')$possibleToBuild(g), i_nameplate(g)):<6:0 /
  '  South Island'                      @38 put sum(mapg_ild(g,'si')$possibleToBuild(g), i_nameplate(g)):<6:0 //

  'VoLL plant (NB: one VOLL plant per region; not included in count of generating plant above)' /
  'Count:'                              @38 card(r):<4:0 /
  'Capacity, MW:'                       @38 VOLLcap:<4:0 /
  'Cost, $/MWh:'                        @38 VOLLcost:<5:0 //

  'Technologies with randomised capex:' @38 if(sum(randomiseCapex(k), 1), loop(randomiseCapex(k), put k.tl, ', ' ) else put 'There are none' ) put /
  'Randomised cost range (+/-), %:'     @38 (100 * randomCapexCostAdjuster):<5:1 //

  'Notes:' /
  "For more precise information on input data, inspect 'Selected prepared input data XXX.gdx'" /
  'MW - nameplate MW.' /
  'Capex - Capital cost of new plant, $/kW (as levelised and used in objective function). Includes any connection cost and randomisation.' / 
  'varCC - the variablised connection cost component of the aforementioned capex, $/kW.' / 
  'varOM - variable O&M costs by plant, $/MWh.' /
  'avSRMC - SRMC averaged over all years for the default scenario, $/MWh.' /
  '  NB: SRMC includes variable O&M, CO2 tax, and total fuel costs (which is made up of energy price plus any variable fuel prodcution/delivery cost).' /
  'fixOM - fixed O&M costs by plant (as used in objective function and including any fixed fuel prodcution/delivery costs), $/kW/year.' /
  'fixFDC - fixed fuel prodcution/delivery costs (converted to $/kW/year and included in fixOM above), $/GJ.' /
  'TCsclr - scaling factor applied to all (actually, most) costs in objective function - the so-called ensembleFactor.' /
  'HR - heat rate of generating plant, GJ/GWh.' /
  'PkCon - contribution to peak factor - averaged over years.' /
  'FoF - forced outage factor.' /
  'xFoFm - eXceptional forced outage factor multipliers, i.e. at least one load block less than 0.5 or greater than 1.5.' /
  'mnCapF - minimum capacity factor averaged over years and periods.' /
  'mxCapF - maximum capacity factor averaged over periods and load blocks (hours per block per period are the weights).' /
  'avMnU - minimum utilisation of plant averaged over years.' /
  'Exist - plant already exists.' /
  'noExst - plant does not exist but may be a candidate for building.' /
  'Commit - plant is committed to be built in the single year given in the column entitled FixYr.' /
  'New - plant is potentially able to be built but no earlier than the year given in the ErlyYr column.' /
  'NvaBld - plant is defined by user to be never able to be built.' /
  'ErlyYr - the earliest year in which a new plant can be built.' /
  'FixYr - the year in which committed plant will be built or, if 3333, the plant can never be built.' /
  'inVbld - the plant is in the set called validYrBuild.' /
  'inVopr - the plant is in the set called validYrOperate.' /
  'Retire - plant is able to be retired, either exogenouosly or endogenously (see next 2 columns).' /
  'EndogY - year in which endogenous retire/refurbish decision is made.' /
  'ExogYr - year in which plant is exogenously retired.' /
  'Mover - plant for which the commissioning date is able to move if the timing run is re-optimised.' //
  'Existing plant' /
  'Plant number/name' @20 'Technology' @34 "%plantDataHdr1%" "%plantDataHdr2%" ;
counter = 0 ;
loop((k,exist(g))$mapg_k(g,k),
  counter = counter + 1 ;
  put / counter:<4:0, g.tl:<15, k.tl:<12, i_nameplate(g):4:0, (1e-3*capexPlant(g)):7:0, (1e-3*vbleConCostPlant(g)):7:0, i_varOM(g):7:1, avgSRMC(g):7:1, i_fixedOM(g):7:1
        i_fixedFuelCosts(g):7:1, ensembleFactor(g):7:2, i_heatrate(g):7:0, avgPeakCon(g):7:2, i_fof(g):7:2 @109 ;
  if(xFoFm(g),        put 'Y' else put '-' ) put @113 ;
  put avgMinCapFact(g):7:2, avgMaxCapFact(g):7:2, avgMinUtilisation(g):7:2, @137 'Y' @143 '-' @153 ;
  if(commit(g),       put 'Y' else put '-' ) put @157 ;
  if(new(g),          put 'Y' else put '-' ) put @162 ;
  if(neverBuild(g),   put 'Y' else put '-' ) put @168 ;
  if(i_EarlyComYr(g), put i_EarlyComYr(g):4:0 else put '-' ) put @174 ;
  if(i_fixComYr(g),   put i_fixComYr(g):4:0   else put '-' ) put @182 ;
  if(sum(y, validYrBuild(g,y)),   put 'Y' else put '-' ) put @190 ;
  if(sum(y, validYrOperate(g,y)), put 'Y' else put '-' ) put @196 ;
  if(possibleToRetire(g),     put 'Y' else put '-' ) put @202 ;
  if(i_refurbDecisionYear(g), put i_refurbDecisionYear(g):>4:0 else put '-' ) put @209 ;
  if(i_ExogenousRetireYr(g),  put i_ExogenousRetireYr(g):>4:0  else put '-' ) put @218 '-' @222 ;
  loop(mapg_r(g,r), put r.tl ) put @229 loop(mapg_o(g,o), put o.tl ) put @236 loop(mapg_i(g,i), put i.tl ) put @246 g.te(g) ;
) ;
put // 'Non-existing plant' @34 "%plantDataHdr1%" "%plantDataHdr2%" ;
loop((k,g)$( (not exist(g)) and mapg_k(g,k) ),
  counter = counter + 1 ;
  put / counter:<4:0, g.tl:<15, k.tl:<12, i_nameplate(g):4:0, (1e-3*capexPlant(g)):7:0, (1e-3*vbleConCostPlant(g)):7:0, i_varOM(g):7:1, avgSRMC(g):7:1, i_fixedOM(g):7:1
        i_fixedFuelCosts(g):7:1, ensembleFactor(g):7:2, i_heatrate(g):7:0, avgPeakCon(g):7:2, i_fof(g):7:2 @109 ;
  if(xFoFm(g),        put 'Y' else put '-' ) put @113 ;
  put avgMinCapFact(g):7:2, avgMaxCapFact(g):7:2, avgMinUtilisation(g):7:2, @137 '-' @143 'Y' @153 ;
  if(commit(g),       put 'Y' else put '-' ) put @157 ;
  if(new(g),          put 'Y' else put '-' ) put @162 ;
  if(neverBuild(g),   put 'Y' else put '-' ) put @168 ;
  if(i_EarlyComYr(g), put i_EarlyComYr(g):4:0 else put '-' ) put @174 ;
  if(i_fixComYr(g),   put i_fixComYr(g):4:0   else put '-' ) put @182 ;
  if(sum(y, validYrBuild(g,y)),   put 'Y' else put '-' ) put @190 ;
  if(sum(y, validYrOperate(g,y)), put 'Y' else put '-' ) put @196 ;
  if(possibleToRetire(g),     put 'Y' else put '-' ) put @202 ;
  if(i_refurbDecisionYear(g), put i_refurbDecisionYear(g):>4:0 else put '-' ) put @209 ;
  if(i_ExogenousRetireYr(g),  put i_ExogenousRetireYr(g):>4:0  else put '-' ) put @218 ;
  if(sum(movers(k), 1) and not moverExceptions(g), put 'Y' else put '-' ) ;   put @222 ;
  loop(mapg_r(g,r), put r.tl ) put @229 loop(mapg_o(g,o), put o.tl ) put @236 loop(mapg_i(g,i), put i.tl ) put @246 g.te(g) ;
) ;


* g) Write the capex statistics.
put capexStats 'Descriptive statistics of plant capex (lumpy and including grid connection costs).' //
  'First modelled year:' @22 firstYear:<4:0 /
  'Last modelled year:'  @22 lastYear:<4:0 / ;
loop(stat, put / stat.tl @13 '- ' stat.te(stat) ) ;
put // @24 ; loop(stat, put stat.tl:>10 ) ;
loop(k,
  counter = 0 ;
  put / k.tl @15 ;
  loop(aggR,
    if(counter = 0, put aggR.tl else put / @15 aggR.tl ) ;
    counter = 1 ;
    put @24 ;
    loop(stat,
      if(not ord(stat) = card(stat), put capexStatistics(k,aggR,stat):>10:0 else put capexStatistics(k,aggR,stat):>10:1 ) ;
    ) ;
  ) ;
) ;
put /// 'MW available for installation by technology and island' / @15 loop(aggR, put aggR.te(aggR):>15 ) ;
loop(k,
  put / k.tl @15 loop(aggR, put MWtoBuild(k,aggR):>15:0 ) ;
) ;
put /// 'Assumed GWh from all plant available for installation by technology and island' / @15 loop(aggR, put aggR.te(aggR):>15 ) ;
loop(k,
  put / k.tl @15 loop(aggR, put GWhtoBuild(k,aggR):>15:0 ) ;
) ;


* h) Write the load summaries.
put loadSummary 'Energy and peak load by region/island and year, GWh' /
  ' - GWh energy grossed-up by AC loss factors and scaled by scenario-specific energy factor' /
  ' - GWh energy and peak load reported here relates only to the default scenario (' loop(defaultScenario(scen), put scen.tl ) put ').' /
  ' - Demand file: ' "%GEMdemandGDX%." ;

put // 'Intraregional AC loss factors, %' ;
loop(ild, put / @2 ild.tl @14 (100 * AClossFactors(ild)):>10:2 ) ;

put // 'Scenario-specific energy factors' ;
loop(scen, put / @2 scen.tl @14 scenarioNRGfactor(scen):>10:2 ) ;

put // 'Energy, GWh' @14 loop(r$( card(isIldEqReg) <> 2 ), put r.tl:>10 ) loop(aggR, put aggR.tl:>10 ) ;
loop(y,
  put / @2 y.tl @14
  loop(r$( card(isIldEqReg) <> 2 ), put loadByRegionYear(r,y):>10:0 ) ;
  loop(aggR, put loadByAggRegionYear(aggR,y):>10:0 ) ;
) ;

put // 'Peak load, MW' @14 loop(aggR, put aggR.tl:>10 ) ;
loop(y, put / @2 y.tl @14  loop(aggR, put peakLoadByYearAggR(y,aggR):>10:0 ) ) ;


* i) Include code to compute and write out LRMC of all non-existing plant.
$if %calcInputLRMCs%==0 $goto noLRMC
$include GEMlrmc.gms
$label noLRMC




* End of file.



*$ontext
* Informs paper data
Parameter
  numberYears
* PVfacG(y,t)             "Generation investor's present value factor by period"
  PVfactor(y)              "Generation investor's present value factor by year"

* i_fixedOM(g)             'Fixed O&M costs by plant, $/kW/year'
  ctfixedOM(k)             'Count of plant in fixed OM data by tech'
  allfixedOM(k,g)          'GEM fixed O&M costs by plant grouped by technology, $/kW/year'
  avfixedOM(k)             'GEM fixed O&M costs averaged by technology, $/kW/year'

* SRMC(g,y,scenarios)      'Short run marginal cost of each generation project by year and scenario, $/MWh'
  ctSRMC(k)                'Count of plant in SRMC data by tech'
  allSRMC(k,g,y)           'GEM short run marginal cost of each generation project grouped by technology, $/MWh'
  avSRMC(k,y)              'GEM short run marginal cost of each generation project by year and scenario, $/MWh'

* capCharge(g,y)           'Annualised or levelised capital charge for new generation plant, $/MW/yr'
  ctCapcharge(k)           'Count of plant in capcharge data by tech'
  allCapcharge(k,g,y)      'GEM annualised capex charges grouped by technology, $/MW/yr'
  avCapchargeYr(k,y)       'Average GEM annualised capex charges by technology and year, $/MW/yr'
  avCapcharge(g)           'Average (over years) GEM annualised capex charges by plant, $/MW/yr'
  capChargeStatistics(k,aggR,stat)  'Descriptive statistics of annualised capex charges by technology, island and NZ - UNITS NOT AS ADVERTISED'
  ;

numberYears = card(y) ;

pvfactor(y) = 1 / ( 1 + WACCg ) ** (yearNum(y) - firstYear) ;

allfixedOM(k,g)$mapg_k(g,k)     = i_fixedOM(g) ;
ctfixedOM(k) = sum(mapg_k(g,k)$i_fixedOM(g), 1) ;
avfixedOM(k)$ctfixedOM(k) = sum(mapg_k(g,k), i_fixedOM(g)) / ctfixedOM(k) ;

*SRMC(g,y,scen) = i_varOM(g) + totalFuelCost(g,y,scen) + CO2taxByPlant(g,y,scen) ;
allSRMC(k,g,y)$mapg_k(g,k) = SRMC(g,y,'avgHydro') ;
ctSRMC(k) = sum(mapg_k(g,k)$SRMC(g,'2012','avgHydro'), 1) ;
avSRMC(k,y)$ctSRMC(k) = sum(mapg_k(g,k), SRMC(g,y,'avgHydro')) / ctSRMC(k) ;

allCapcharge(k,g,y)$mapg_k(g,k) = capCharge(g,y) ;
ctCapcharge(k)= sum(mapg_k(g,k)$capcharge(g,'2012'), 1) ;
avCapchargeYr(k,y)$ctCapcharge(k) = sum(mapg_k(g,k), capCharge(g,y)) / ctCapcharge(k) ;
avCapcharge(g) = sum(y, capCharge(g,y)) / numberYears ;

capChargeStatistics(k,aggR,'count') = sum((g,r)$( mapg_k(g,k) * mapg_r(g,r) * mapAggR_r(aggR,r) * possibleToBuild(g) ), 1 ) ; 
capChargeStatistics(k,aggR,'min')$capChargeStatistics(k,aggR,'count')   = smin((g,r)$( mapg_k(g,k) * mapg_r(g,r) * mapAggR_r(aggR,r) * possibleToBuild(g) ), avCapCharge(g) ) ; 
capChargeStatistics(k,aggR,'max')$capChargeStatistics(k,aggR,'count')   = smax((g,r)$( mapg_k(g,k) * mapg_r(g,r) * mapAggR_r(aggR,r) * possibleToBuild(g) ), avCapCharge(g) ) ; 
capChargeStatistics(k,aggR,'range')$capChargeStatistics(k,aggR,'count') = capChargeStatistics(k,aggR,'max') - capChargeStatistics(k,aggR,'min') ;
capChargeStatistics(k,aggR,'mean')$capChargeStatistics(k,aggR,'count')  =
  sum((g,r)$( mapg_k(g,k) * mapg_r(g,r) * mapAggR_r(aggR,r) * possibleToBuild(g) ), avCapCharge(g) ) / capChargeStatistics(k,aggR,'count') ; 
capChargeStatistics(k,aggR,'variance')$capChargeStatistics(k,aggR,'count') =
  sum((g,r)$( mapg_k(g,k) * mapg_r(g,r) * mapAggR_r(aggR,r) * possibleToBuild(g) ), sqr(avCapCharge(g) - capChargeStatistics(k,aggR,'mean')) ) / capChargeStatistics(k,aggR,'count') ; 
capChargeStatistics(k,aggR,'stdDev') = sqrt(capChargeStatistics(k,aggR,'variance')) ;
capChargeStatistics(k,aggR,'stdDev%')$capChargeStatistics(k,aggR,'mean') = 100 * capChargeStatistics(k,aggR,'stdDev') / capChargeStatistics(k,aggR,'mean') ;


*avSRMC(k,y)$capChargeStatistics(k,'nz','count') = sum(mapg_k(g,k), SRMC(g,y,'avgHydro')) / capChargeStatistics(k,'nz','count') ;

option pvfactor:3:0:1 ;
Display avCapcharge, capChargeStatistics, avSRMC, pvfactor, avfixedOM ;

Execute_Unload "InformsData.gdx" numberYears taxRate WACCg PVfacG pvfactor i_fixedOM ctfixedOM allFixedOM avfixedOM allCapcharge capcharge avCapchargeYr avCapcharge capChargeStatistics SRMC ctSRMC allSRMC avSRMC  ; 
*$offtext


Scalar newGeoMW ;
newgeoMW = sum(mapg_k(g,k)$( sameas(k,'geo') * noExist(g)), i_nameplate(g)) ;
display newgeoMW ;

