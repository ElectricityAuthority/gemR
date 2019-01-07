* GEMdeclarations.gms


* Last modified by Dr Phil Bishop, 05/06/2012 (imm@ea.govt.nz)


$ontext
  This program declares all of the symbols (sets, scalars, parameters, variables, equations and files) used in GEM up to
  and including GEMsolve. Symbols required only for post-solve reporting purposes are generally declared in GEMreports. However,
  there are a few reporting-related declarations in this program, as GEMdata and GEMsolve do some preparatory work for GEMreports.
  In a few cases, a handful of sets whose membership never changes, symbols are intialised as well as declared in this program.
  In other words, the membership of those sets is assigned at the time of declaration. In all other cases, set membership and
  scalar/parameter values are obtained from user-specified input files, or are computed within GEMdata using the imported data.

  The GEMdeclarations work file (GEMdeclarations.g00) is saved and used at invocation to restart GEMdata.

  Notes:
  1. GEM has a comprehensive treatment of reserves, emodied in the following variables and equations:
       Variables: RESV, RESVVIOL, RESVTRFR and RESVREQINT; and
       Equations: resvsinglereq1, genmaxresv1, resvtrfr1, resvtrfr2, resvtrfr3, resvrequnit, resvreq2,
       resvreqhvdc, resvtrfr4, resvtrfrdef, resvoffcap, and resvreqwind.
     However, the reserves formulation has not yet been properly parameterized so it is suppressed using the switch 'reservesOn',
     which can be found in GEMsettings.inc. In the meantime, non-free reserves are modelled outside of the reserves formulation.
     At some point, the reserves formulation will be properly parameterized and, perhaps, modified to incorporate a replacement
     treatment of non-free reserves. The treatment of non-free reserves makes use of the set lvl; the parameters largestNIplant,
     largestSIplant, freeReserves, nonFreeReservesCap, bigSwd, bigNwd, pNFresvCap, and pNFresvCost; the variable RESVCOMPONENTS;
     the equations calc_nfreserves and resv_capacity.
  2. See the comment "***txGrpConstraint(validTGC,y,t,lb,sc)$txconstraintactive(y,t,validTGC).." at initialisation of txGrpConstraint.
     What's up with this and does it need fixing, removing? Is txconstraintactive even a known symbol anymore?

 Code sections:
  1. Declare sets and parameters - the data for which is imported from input GDX files.
  2. Declare remaining sets and parameters.
     a) Hard-coded sets.
     b) Scenario-specific sets and parameters.
     c) Various GEM configuration sets and parameters.
     d) Declare all remaining sets and parameters.
  3. Declare model variables and equations.
  4. Specify the equations and declare the models.
  5. Declare the 's_' parameters and specify the statements used to collect up results at end of each experiment.
  6. Declare some (but not all) output files to be created by GEMdata and GEMsolve.
$offtext

* Turn the following maps on/off as desired.
$offuelxref offuellist
*$onuelxref  onuellist
$offsymxref offsymlist
*$onsymxref  onsymlist



*===============================================================================================
* 1. Declare sets and parameters - the data for which is imported from input GDX files.

* First, declare without initialising five sets (with fixed membership) that are not contained in the input GDX file. Rather, the
* membership of these sets comes, either, from a .inc file or it's hard-coded later in GEMdeclarations. But they need to be declared
* now because some sets in the input GDX files are defined on these five sets.
Sets
  y            'Modelled calendar years'
  ild          'Islands'
  m            '12 months'
  geo          'Geographic co-ordinate types'
  col          'RGB color codes'
* 17 fundamental sets
  k            'Generation technologies'
  f            'Fuels'
  fg           'Fuel groups'
  g            'Generation plant'
  o            'Owners of generating plant'
  i            'Substations'
  r            'Regions'
  e            'Zones'
  p            'Transmission paths (or branches)'
  ps           'Transmission path states (state of upgrade)'
  tupg         'Transmission upgrade projects'
  tgc          'Transmission group constraints'
  t            'Time periods (within a year)'
  lb           'Load blocks'
  rc           'Reserve classes'
  hY           'Historical years with hydrology data'
  v            'Hydro reservoirs or river systems'
  aggR         'Aggregate regional entities'
  lvl          'Levels of non-free reserves';

Alias (g,gg), (i,ii), (r,rr), (ild,ild1), (ps,pss), (lb,lbb), (hY,hYY), (col,red,green,blue) ;

* 36 mapping sets and subsets (grouped thematically as per the navigation pane of EMI)
Sets
* 22 technology and fuel
  mapf_k(f,k)                                   'Map technology types to fuel types'
  mapf_fg(f,fg)                                 'Map fuel groups to fuel types'
  techColor(k,red,green,blue)                   'RGB color mix for technologies - to pass to plotting applications'
  fuelColor(f,red,green,blue)                   'RGB color mix for fuels - to pass to plotting applications'
  fuelGrpcolor(fg,red,green,blue)               'RGB color mix for fuel groups - to pass to plotting applications'
  movers(k)                                     'Technologies for which commissioning date can move during re-optimisation of build timing'
  refurbish(k)                                  'Technologies eligible for endogenous "refurbish or retire" decision'
  endogRetire(k)                                'Technologies eligible for endogenous retirement in years prior to and including the refurbish/retire decision'
  cogen(k)                                      'Cogeneration technologies'
  peaker(k)                                     'Peaking plant technologies'
  hydroSched(k)                                 'Schedulable hydro technologies'
  hydroPumped(k)                                'Pumped storage hydro technologies'
  wind(k)                                       'Wind technologies'
  renew(k)                                      'Renewable technologies'
  thermalTech(k)                                'Thermal technologies'
  demandGen(k)                                  'Demand side technologies modelled as generation'
  randomiseCapex(k)                             'Specify the technologies able to have their capital costs randomised within some narrow user-defined range'
  linearBuildTech(k)                            'Specify the technologies able to be partially or linearly built'
  coal(f)                                       'Coal fuel'
  lignite(f)                                    'Lignite fuel'
  gas(f)                                        'Gas fuel'
  diesel(f)                                     'Diesel fuel'
* 4 generation
  mapGenPlant(g,k,i,o)                          'Generation plant mappings'
  exist(g)                                      'Generation plant that are presently operating'
  schedHydroUpg(g)                              'New generation plant that are to be built on an existing schedulable hydro system'
  mapSH_Upg(g,gg)                               'Map existing schedulable hydro plant to what are essentially schedulable hydro upgrades'
* 6 location
  mapLocations(i,r,e,ild)                       'Location mappings'
  Haywards(i)                                   'Haywards substation'
  Benmore(i)                                    'Benmore substation'
  regionCentroid(i,r)                           'Identify the centroid of each region with a substation'
  zoneCentroid(i,e)                             'Identify the centroid of each zone with a substation'
  islandCentroid(i,ild)                         'Identify the centroid of each island with a substation'
* 2 transmission
  txUpgradeTransitions(tupg,r,rr,ps,pss)        'Define the allowable transitions from one transmission path state to another'
  mapArcNode(p,r,rr)                            'Map nodes (actually, regions) to arcs (paths) in order to build the bus-branch incidence matrix'
* 1 load and time
  mapm_t(m,t)                                   'Map time periods to months'
* 0 reserves and security
* 1 hydrology
  mapReservoirs(v,i,g)                          'Reservoir mappings'
  ;

* Declare 80 parameters (again, grouped thematically as per the navigation pane of EMI).
Parameters
* 15 technology and fuel
  i_plantLife(k)                                'Generation plant life, years'
  i_refurbishmentLife(k)                        'Generation plant life following refurbishment, years'
  i_retireOffsetYrs(k)                          'Number of years a technology continues to operate for after the decision to endogenously retire has been made'
  i_linearBuildMW(k)                            'Threshold MW level used to activate linearisation of binary decision variables for plants able to be linearly built'
  i_linearBuildYr(k)                            'Threshold early build year used to activate linearisation of binary decision variables for plants able to be linearly built'
  i_depRate(k)                                  'Depreciation rate for generation plant, technology specific'
  i_peakContribution(k)                         'Contribution to peak by technology'
  i_NWpeakContribution(k)                       'The no wind contribution to peak by technology'
  i_capFacTech(k)                               'An assumed (rather than modelled) technology-specific capacity factor - used when computing LRMCs based on input data (i.e. prior to solving the model)'
  i_FOFmultiplier(k,lb)                         'Forced outage factor multiplier'
  i_maxNrgByFuel(f)                             'Maximum proportion of total energy from any one fuel type (0-1)'
  i_emissionFactors(f)                          'CO2e emissions, tonnes CO2/PJ'
  i_fuelPrices(f,y)                             'Fuel prices by fuel type and year, $/GJ'
  i_fuelQuantities(f,y)                         'Quantitative limit on availability of various fuels by year, PJ'
  i_co2tax(y)                                   'CO2 tax by year, $/tonne CO2-equivalent'
* 32 generation
  i_nameplate(g)                                'Nameplate capacity of generating plant, MW'
  i_UnitLargestProp(g)                          'Largest proportion of generating plant output carried by a single unit at the plant'
  i_baseload(g)                                 'Force plant to be baseloaded, 0/1 (1 = baseloaded)'
  i_offlineReserve(g)                           'Plant-specific offline reserve capability, 1 = Yes, 0 = No'
  i_FixComYr(g)                                 'Fixed commissioning year for potentially new generation plant (includes plant fixed never to be built)'
  i_EarlyComYr(g)                               'Earliest possible commissioning year for each potentially new generation plant'
  i_ExogenousRetireYr(g)                        'Exogenous retirement year for generation plant'
  i_refurbDecisionYear(g)                       'Decision year for endogenous "refurbish or retire" decision for eligble generation plant'
  i_fof(g)                                      'Forced outage factor for generating plant, proportion (0-1)'
  i_heatrate(g)                                 'Heat rate of generating plant, GJ/GWh (default = 3600)'
  i_PumpedHydroMonth(g)                         'Limit on energy per month from pumped hydro plant, GWh'
  i_PumpedHydroEffic(g)                         'Efficiency of pumped energy to stored energy, MWh stored per MWh pumped < 1'
  i_minHydroCapFact(g)                          'Minimum capacity factors for selected schedulable hydro plant'
  i_maxHydroCapFact(g)                          'Maximum capacity factors for selected schedulable hydro plant'
  i_fixedOM(g)                                  'Fixed O&M costs by plant, $/kW/year'
  i_varOM(g)                                    'Variable O&M costs by plant, $/MWh'
  i_varFuelCosts(g)                             'Variable fuel production and delivery costs (over and above the energy cost of fuel at source), $/GJ'
  i_fixedFuelCosts(g)                           'Fixed fuel production and delivery costs (to be added to i_fixedOM), $/GJ'
  i_capitalCost(g)                              'Generation plant capital cost, $/kW'
  i_connectionCost(g)                           'Capital cost for connecting generation plant to grid, $m (NZD)'
  i_refurbCapitalCost(g)                        'Generation plant refurbishment capital cost, $/kW'
  i_hydroPeakingFactor(g)                       'Factor to approximate the impact of hydro variability (correlation and dry year back-up)'
  i_inflexiblePlantFactor(g,lb)                 'Proportion of generation in one load block that inflexible plant must generate in rightward adjacent load block'
  i_plantReservesCap(g,rc)                      'Plant-specific capability per reserve class (0-1 but define only when > 0)'
  i_plantReservesCost(g,rc)                     'Plant-specific cost per reserve class, $/MWh'
  i_PltCapFact(g,m)                             'Plant-specific capacity factor'
  i_minUtilisation(g,y)                         'Minimum utilisation of plant by year, proportion (0-1 but only defined when > 0)'
  i_HVDCshr(o)                                  'Share of HVDC charge to be incurred by plant owner'
  i_renewNrgShare(y)                            'Proportion of total energy to be generated from renewable sources by year (0-1 but define only when > 0)'
  i_renewCapShare(y)                            'Proportion of installed capacity that must be renewable by year (0-1 but define only when > 0)'
  i_distdGenRenew(y)                            'Distributed generation (renewable) installed by year, GWh'
  i_distdGenFossil(y)                           'Distributed generation (fossil) installed by year, GWh'
* 2 location
  i_substnCoordinates(i,geo)                    'Geographic coordinates for substations'
  i_zonalLocFacs(e)                             'Zonal location factors - used to adjust costs to account for marginal loss effects'
* 11 transmission
  i_txCapacity(r,rr,ps)                         'Transmission path capacities (bi-directional), MW'
  i_txCapacityPO(r,rr,ps)                       'Transmission path capacities with largest pole out (bi-directional, HVDC link only), MW'
  i_txResistance(r,rr,ps)                       'Transmission path resistance (not really a resistance but rather a loss function coefficient), p.u. (MW)'
  i_txReactance(r,rr,ps)                        'Reactance by state of each transmission path, p.u. (MW)'
  i_maxReservesTrnsfr(r,rr,ps,rc)               'Maximum reserves transfer capability in the direction of MW flow on the HCDC link, MW'
  i_txCapitalCost(tupg)                         'Transmission upgrade capital cost by upgrade project, $m'
  i_txEarlyComYr(tupg)                          'Earliest year that a transmission upgrade can occur (this is a parameter, not a set)'
  i_txFixedComYr(tupg)                          'Fixed year in which a transmission upgrade must occur (this is a parameter, not a set)'
  i_txGrpConstraintsLHS(tgc,p)                  'Coefficients for left hand side of transmission group constraints'
  i_txGrpConstraintsRHS(tgc)                    'Coefficients for the right hand side of transmission group constraints, MW'
  i_HVDClevy(y)                                 'HVDC charge levied on new South Island plant by year, $/kW'
* 5 load and time
  i_firstDataYear                               'First data year - as a scalar, not a set'
  i_lastDataYear                                'Last data year - as a scalar, not a set'
  i_HalfHrsPerBlk(m,lb)                         'Count of half hours per load block in each month'
  i_NrgDemand(r,y,t,lb)                         'Load by region, year, time period and load block, GWh'
  i_inflation(y)                                'Inflation rates by year'
* 13 reserves and security
  i_ReserveSwitch(rc)                           'Switch to activate reserves by reserves class'
  i_ReserveAreas(rc)                            'Number of reserves areas (Single or system-wide = 1, By island = 2)'
  i_propWindCover(rc)                           'Proportion of wind to cover by reserve class (0-1 but define only when > 0)'
  i_ReservePenalty(ild,rc)                      'Reserve violation penalty, $/MWh'
  i_reserveReqMW(y,ild,rc)                      'Reserve requirement by year, island, and class, MW'
  i_winterCapacityMargin(y)                     'Required winter capacity margin, MW'
  i_SIACrisk(y)                                 'Required cover for South Island AC risk by year, MW'
  i_fkSI(y)                                     'Required frequency keeping in South Island by year, MW'
  i_fkNI(y)                                     'Required frequency keeping in North Island by year, MW'
  i_HVDClossesAtMaxXfer(y)                      'Required cover for HVDC (bi-pole) losses at maximum transfer by year, MW'
  i_largestGenerator(y)                         'Largest generation plant by year, MW'
  i_P200ratioNZ(y)                              'Desired ratio of peak demand MW to average demand MW (derived from forecast GWh energy demand), New Zealand'
  i_P200ratioNI(y)                              'Desired ratio of peak demand MW to average demand MW (derived from forecast GWh energy demand), North Island'
* 2 hydrology
  i_firstHydroYear                              'First year of historical hydrology data'
  i_historicalHydroOutput(v,hY,m)               'Historical hydro output sequences by reservoir and month, GWh' ;

* End of GDX declarations



*===============================================================================================
* 2. Declare remaining sets and parameters.
*    a) Hard-coded sets.
*    b) Scenario-specific sets and parameters.
*    c) Various GEM configuration sets and parameters.
*    d) Declare all remaining sets and parameters.

* a) Hard-coded sets (non-developer users have no need to change these set elements).
Sets
  n                                             'Number of vertices in piecewise linear loss functions'
  ct                                            'Capital expenditure types'           / genplt     'New generation plant'
                                                                                        refplt     'Refurbish existing generation plant' /
  d                                             'Discount rate classes'               / WACCg      "Generation investor's post-tax real weighted average cost of capital"
                                                                                        WACCt      "Transmission investor's post-tax real weighted average cost of capital"
                                                                                        dLow       'Lower discount rate for sensitivity analysis'
                                                                                        dMed       'Central discount rate for sensitivity analysis'
                                                                                        dHigh      'Upper discount rate for sensitivity analysis'    /
  dt                                            'Types of discounting'                / mid        'Middle of the period within each year'
                                                                                        eoy        'End of year'   /
  steps                                         'Steps in an experiment'              / timing     'Solve the timing problem, i.e. timing of new generation/or transmission investment'
                                                                                        reopt      'Solve the re-optimised timing problem (generally with a drier hydro sequence) while allowing peakers to move'
                                                                                        dispatch   'Solve for the dispatch only with investment timing fixed'  /
  hydroSeqTypes                                 'Types of hydro sequences to use'     / Same       'Use the same historical hydro year in every modelled year'
                                                                                        Sequential 'Use a sequentially developed mapping of historical hydro years to modelled years' /
*  ild                                           'Islands'                             / ni         'North Island'
*                                                                                        si         'South Island' /
*  aggR                                          'Aggregate regional entities'         / ni         'North Island'
*                                                                                        si         'South Island'
*                                                                                        nz         'New Zealand' /
  m                                             '12 months'                           / Jan        'January'
                                                                                        Feb        'February'
                                                                                        Mar        'March'
                                                                                        Apr        'April'
                                                                                        May        'May'
                                                                                        Jun        'June'
                                                                                        Jul        'July'
                                                                                        Aug        'August'
                                                                                        Sep        'September'
                                                                                        Oct        'October'
                                                                                        Nov        'November'
                                                                                        Dec        'December'  /
  geo                                           'Geographic co-ordinate types'        / Easting    'New Zealand Transverse Mercator, metres'
                                                                                        Northing   'New Zealand Transverse Mercator, metres'
                                                                                        Long       'Degrees of longitude - note that the minutes are expressed as a decimal'
                                                                                        Lat        'Degrees of latitude - note that the minutes are expressed as a decimal'  /
  stat                                          'Classes of statistics'               / Count      'Number of plant'
                                                                                        Min        'Lowest cost, $/kW'
                                                                                        Max        'Highest cost, $/kW'
                                                                                        Range      'Range of costs, $/kW'
                                                                                        Variance   'Variance, $/kW'
                                                                                        Mean       'Mean, $/kW'
                                                                                        StdDev     'Standard deviation, $/kW'
                                                                                       'StdDev%'   'Standard deviation as a percentage'  /
  col                                           'RGB color codes'                     / 0 * 256 /
*  lvl                                           'Levels of non-free reserves'         / lvl1 * lvl5 /
  ;

* b) Scenario-specific sets and parameters - see (mostly) GEMstochastic.
Sets
  scenarios                                     'The various individual stochastic scenarios, or futures, or states of uncertainty'
  scenarioSets                                  'A coherent collection of scenarios to be simultaneously solved over'
  experiments                                   'A collection of scenarioSets to be solved for in the current runVersion. Experiments must get mapped to steps - timing, re-optimisation and dispatch'
  sc(scenarios)                                 '(Dynamically) selected subsets of elements of scenarios'
  mapScenarios(scenarioSets,scenarios)          'Map the individual scenarios to an scenario set (i.e. 1 or more scenarios make up an scenario set)'
  timingSolves(experiments,scenarioSets)        'Which scenario sets are used for the timing step of each experiment?'
  reoptSolves(experiments,scenarioSets)         'Which scenario sets are used for the reoptimisation step of each experiment?'
  dispatchSolves(experiments,scenarioSets)      'Which scenario sets are used for the dispatch step of each experiment?'
  allSolves(experiments,steps,scenarioSets)     'Scenario sets by experiment and step'
  mapSC_hY(scenarios,hY)                        'Map historical hydro years to scenarios (compute the average if more than one historical year is specified)'
  mapSC_hydroSeqTypes(scenarios,hydroSeqTypes)  'Map the hydrology sequence types (same or sequential) to scenarios'
  defaultScenario(scenarios)                    'Identify a default scenario to use when reporting input data summaries. Applies only to input data defined over scenarios (see GEMdata)'
  mapHydroYearsToModelledYears(experiments,steps,scenarioSets,scenarios,y,hY)     'Collect the mapping of historical hydro years to modelled years for all experiments-steps-scenarioSets tuples'
  allAvgDispatchSolves(experiments,steps,scenarioSets)                            'All solves for which the dispatch simulations are to be averaged over all scenarios mapped to each scenario set'
  allNotAvgDispatchSolves(experiments,steps,scenarioSets)                         'All solves for which the dispatch simulations are not to be averaged over all scenarios mapped to each scenario set'
  figureOutAvgDispatch(experiments,steps,scenarioSets,scenarios,hydroSeqTypes,hY) 'All solves for which the dispatch simulations are to be averaged over all scenarios mapped to each scenario set - and associated mappings'  ;

Alias(scenarios,scen), (scenarioSets,scenSet) ;

Parameters
  scenarioPeakLoadFactor(scenarios)             'Scenario-specific scaling factor for peak load data'
  scenarioCO2TaxFactor(scenarios)               'Scenario-specific scaling factor for CO2 tax data'
  scenarioFuelCostFactor(scenarios)             'Scenario-specific scaling factor for fuel cost data'
  scenarioNRGFactor(scenarios)                  'Scenario-specific scaling factor for energy demand data'
  weightScenariosBySet(scenarioSets,scenarios)  'Assign weights to the scenarios comprising each set of scenarios'
  scenarioWeight(scenarios)                     'Individual scenario weights'
  modelledHydroOutput(g,y,t,scenarios)          'Hydro output used in each modelled year by schedulable hydro plant, GWh'
  hydroOutputUpgrades(g,y,t,scenarios)          'Hydro output normalised for new generation projects that are linked to existing schedulable hydro'
  allModelledHydroOutput(experiments,steps,scenarioSets,g,y,t,scenarios) 'Collect the hydro output used in each modelled year by schedulable hydro plant for all experiments-steps-scenarioSets tuples, GWh'
  allHydroOutputUpgrades(experiments,steps,scenarioSets,g,y,t,scenarios) 'Collect the hydro output normalised for new generation projects that are linked to existing schedulable hydro'
  solveReport(experiments,steps,scenarioSets,*) 'Collect various details about each solve of the models (both GEM and DISP)' ;

* c) Various GEM configuration sets and parameters - see (mostly) GEMsettings.
Parameters
  firstYear                                     'First modelled year - as a scalar, not a set'
  lastYear                                      'Last modelled year - as a scalar, not a set'
  txLossesRMIP                                  'Switch to control usage of the RMIP transmission loss functions (1=0n/0=off=use MIP transmission loss functions)'
  V2GtechnologyOn                               'Switch to control usage of the V2G technology (1=0n/0=namePlate MW set to zero for all V2G plant)'
  renNrgShrOn                                   'Switch to control usage of renewable energy share constraint (1=0n/0=off)'
  renCapShrOn                                   'Switch to control usage of renewable capacity share constraint (1=0n/0=off)'
  niNWpeakCnstrntOn                             'Switch to control usage of the NI no wind peak security constraint (1=0n/0=off)'
  limitNrgByFuelOn                              'Switch to control usage of the constraint that limits energy by fuel type (1=0n for all fuels for which i_maxNrgByFuel is non-zero/0=off for all fuels)'
  reservesOn                                    'Switch to control usage of at least one reserve class (1=at least one class in use/0=no reserve classes)'
  DCloadFlowOn                                  'Switch to control usage of DC load flow formulation (1=DC load flow/0=transshipment)'
  noRetire                                      'Number of years following and including the first modelled year for which endogenous generation plant retirement decisions are prohibited'
  AnnualMWlimit                                 'Upper bound on total MW of new plant able to be built nationwide in any single year'
  VOLLcap                                       'Nameplate capacity of each VOLL plant (1 VOLL plant/region), MW'
  VOLLcost                                      'Value of lost load by each VOLL plant (1 VOLL plant/region), $/MWh'
  penaltyViolatePeakLoad                        'Penalty for failing to meet peak load constraints, $/MW'
  penaltyViolateRenNrg                          'Penalty used to make renewable energy constraint feasible, $/MWh'
  penaltyViolateReserves(ild,rc)                'Penalty for failing to meet certain reserve classes, $/MWh'
  cGenYr                                        'First year in which integer generation build decisions can become continuous, i.e. CGEN or BGEN = 0 in any year'
  noVOLLblks                                    'Number of contiguous load blocks at top of LDC for which the VOLL generators are unavailable'
  randomCapexCostAdjuster                       'Specify the bounds for a small +/- random adjustment to generation plant capital costs'
  slackCost                                     'An arbitrarily high cost for slack variables to be able to enter the objective function'
  slacks                                        'A flag indicating slack variables exist in at least one solution'
  penalties                                     'A flag indicating penalty variables exist in at least one solution'
  genSecs                                       'Number of seconds required to generate the current model'
  numSecs                                       'Number of CPU seconds required to solve the current model'
  numIters                                      'Number of iterations required to solve the current model' ;

* d) Declare all remaining sets and parameters - to be initialised/computed in GEMdata or GEMsolve.
Sets
* Time/date-related parameters.
  firstYr(y)                                    'First modelled year - as a set, not a scalar'
  lastYr(y)                                     'Last modelled year - as a set, not a scalar'
  allButFirstYr(y)                              'All modelled years except the first year - as a set, not a scalar'
  firstPeriod(t)                                'First time period (i.e. period within the modelled year)'
  rightAdjacentBlocks(lb,lbb)                   'Identify the load block adjacent to the right of any given load block'
* VOLL plant (set membership generated automatically in GEMdata)
  s                                             'Shortage or VOLL plants'
  maps_r(s,r)                                   'Map regions to VOLL plants'
* Various mappings and subsets.
  mapg_k(g,k)                                   'Map technology types to generating plant'
  mapg_f(g,f)                                   'Map fuel types to generating plant'
  mapg_o(g,o)                                   'Map plant owners to generating plant'
  mapg_i(g,i)                                   'Map substations to generating plant'
  mapg_r(g,r)                                   'Map regions to generating plant'
  mapg_e(g,e)                                   'Map zones to generating plant'
  mapg_ild(g,ild)                               'Map islands to generating plant'
  mapi_r(i,r)                                   'Map regions to substations'
  mapi_e(i,e)                                   'Map zones to substations'
  mapild_r(ild,r)                               'Map the regions to islands'
  mapAggR_r(aggR,r)                             'Map the regions to the aggregated regional entities (this is primarily to facilitate reporting)'
  mapv_g(v,g)                                   'Map generating plant to reservoirs'
  thermalFuel(f)                                'Thermal fuels'
  isIldEqReg(ild,r)                             'Figure out if the region labels are identical to the North and South island labels (a reporting facilitation device)'
* Generation data.
  noExist(g)                                    'Generation plant that are not presently operating'
  commit(g)                                     'Generation plant that are assumed to be committed'
  new(g)                                        'Potential generation plant that are neither existing nor committed'
  neverBuild(g)                                 'Generation plant that are determined a priori by user never to be built'
  nigen(g)                                      'North Island generation plant'
  sigen(g)                                      'South Island generation plant'
  schedHydroPlant(g)                            'Schedulable hydro generation plant'
  pumpedHydroPlant(g)                           'Pumped hydro generation plant'
  moverExceptions(g)                            'Generating plant to be excepted from the technology-based determination of movers'
  validYrBuild(g,y)                             'Valid years in which new generation plant may be built'
  integerPlantBuild(g)                          'Generating plant that must be integer built, i.e. all or nothing (unless cgenyr in RunGEM is less than LastYr)'
  linearPlantBuild(g)                           'Generating plant able to be linearly or incrementally built'
  possibleToBuild(g)                            'Generating plant that may possibly be built in any valid build year'
  possibleToRefurbish(g)                        'Generating plant that may possibly be refurbished in any valid modelled year'
  possibleToEndogRetire(g)                      'Generating plant that may possibly be endogenously retired'
  possibleToRetire(g)                           'Generating plant that may possibly be retired (exogenously or endogenously)'
  endogenousRetireDecisnYrs(g,y)                'The years in which generation plant able to be endogenously retired can take the decision to retire'
  endogenousRetireYrs(g,y)                      'The years in which generation plant able to be endogenously retired can actually be retired'
  validYrOperate(g,y)                           'Valid years in which an existing, committed or new plant can generate. Use to fix GEN to zero in invalid years'
* Transmission data.
  slackBus(r)                                   'Designate a region to be the slack or reference bus'
  regLower(r,rr)                                'The lower triangular part of region-region matrix, i.e. where ord(r) > ord(rr)'
  interIsland(ild,ild1)                         'Bilateral interisland island pairings - based on the Benmore-Haywards ends of the current HVDC link'
  interIslandRegions(r,rr)                      'Bilateral interisland region pairings - based on the Benmore-Haywards ends of the current HVDC link'
  nwd(r,rr)                                     'Northward direction of flow on Benmore-Haywards HVDC'
  swd(r,rr)                                     'Southward direction of flow on Benmore-Haywards HVDC'
  paths(r,rr)                                   'All valid transmission paths'
  transitions(tupg,r,rr,ps,pss)                 'For all transmission paths, define the allowable transitions from one upgrade state to another'
  validTransitions(r,rr,ps,pss)                 'All allowed upgrade transitions on each valid path'
  allowedStates(r,rr,ps)                        'All of the allowed states (initial and upgraded) for each active path'
  notAllowedStates(r,rr,ps)                     'All r-rr-ps tuples not in the set called allowedStates'
  upgradeableStates(r,rr,ps)                    'Identify all allowable states of upgrade on each path'
  lastAllowedState(r,rr,ps)                     'Identify the last allowed transmission upgrade state on each path'
  validTGC(tgc)                                 'Valid transmission group constraints'
  trnch(n)                                      'Loss tranches for piecewise linear interregional transmission loss function (number of tranches = card(n) - l)'
* Hydrology.
  chooseHydroYears(hY)                          'Used for calculation of hydro sequences'  ;

Parameters
  counter                                       'A recyclable counter - set equal to zero each time before using'
* Time/date-related parameters.
  yearNum(y)                                    'Real number associated with each year'
  hydroYearNum(hY)                              'Real number associated with each historical hydrology output year'
  lastHydroYear                                 'Last year of historical hydrology output data - as an integer'
  hoursPerBlock(t,lb)                           'Hours per load block by time period'
* Financial parameters.
  WACCg                                         "Generation investor's post-tax real weighted average cost of capital"
  WACCt                                         "Transmission investor's post-tax real weighted average cost of capital"
  depType                                       'Flag to indicate depreciation method - 1 for declining value, 0 for straight line'
  taxRate                                       'Corporate tax rate'
  txPlantLife                                   'Life of transmission equipment, years'
  txDepRate                                     'Depreciation rate for transmission equipment'
  discountRates(d)                              'Discount rates - for reporting results only'
  PVfacG(y,t)                                   "Generation investor's present value factor by period"
  PVfacT(y,t)                                   "Transmission investor's present value factor by period"
  PVfacsM(y,t,d)                                'Present value factors as at middle of period for generation, transmission, and sensitivity of discount rates in post-solve calculations'
  PVfacsEY(y,d)                                 'Present value factors as at end of year for generation, transmission, and sensitivity of discount rates in post-solve calculations'
  PVfacs(y,t,d,dt)                              'All present value factors - for generation, transmission, and sensitivity of discount rates in post-solve calculations'
  capexLife(k,ct)                               'Plant life by technology and capex type, years'
  annuityFacN(y,k,ct)                           'Nominal annuity factor by technology, year and type of capex - depends on annual inflation rate'
  annuityFacR(k,ct)                             'Real annuity factor by technology and type of capex'
  txAnnuityFacN(y)                              'Nominal transmission annuity factor and year - depends on annual inflation rate'
  txAnnuityFacR                                 'Real transmission annuity factor'
  capRecFac(y,k,ct)                             'Capital recovery factor by technology including a nominal accounting treatment of depreciation tax credit'
  depTCrecFac(y,k,ct)                           'Recovery factor by technology for just the depreciation tax credit portion of capRecFac'
  txCapRecFac(y)                                'Capital recovery factor for transmission - including a nominal accounting treatment of depreciation tax credit'
  txDeptCRecFac(y)                              'Recovery factor for just the depreciation tax credit portion of txcaprecfac'
* Fuel prices and quantity limits.
  SRMC(g,y,scenarios)                           'Short run marginal cost of each generation project by year and scenario, $/MWh'
  totalFuelCost(g,y,scenarios)                  'Total fuel cost - price plus fuel production and delivery charges all times heatrate - by plant, year and scenario, $/MWh'
  CO2taxByPlant(g,y,scenarios)                  'CO2 tax by plant, year and scenario, $/MWh'
* Generation data.
  initialCapacity(g)                            'Capacity of existing generating plant in the first modelled year'
  vbleConCostPlant(g)                           'Variablised capital cost of connection for new generation plant, $/MW'
  locationFactor(g)                             'Location factors by plant - used to adjust costs to account for marginal loss effects (sourced from zonal factors)'
  ensembleFactor(g)                             'Collection of total cost adjustment factors by plant (e.g. location factors and hydro peaking factors)'
  capexPlant(g)                                 'Capital cost for new generation plant, $/MW'
  capCharge(g,y)                                'Annualised or levelised capital charge for new generation plant, $/MW/yr'
  refurbCapexPlant(g)                           'Capital cost for refurbishing existing generation plant, $/MW'
  refurbCapCharge(g,y)                          'Annualised or levelised capital charge for refurbishing existing generation plant, $/MW/yr'
  exogMWretired(g,y)                            'Exogenously retired MW by plant and year, MW'
  continueAftaEndogRetire(g)                    'Number of years a generation plant keeps going for after the decision to endogenously retire has been made'
  WtdAvgFOFmultiplier(k,lb)                     'FOF multiplier by technology and load block - averaged using hours in block as weights'
  reservesCapability(g,rc)                      'Generating plant reserve capability per reserve class, MW'
  peakConPlant(g,y)                             'Contribution to peak of each generating plant by year'
  NWpeakConPlant(g,y)                           'Contribution to peak when there is no wind of each generating plant by year'
  maxCapFactPlant(g,t,lb)                       'Maximum capacity factor by plant - incorporates forced outage rates'
  minCapFactPlant(g,y,t)                        'Minimum capacity factor - only defined for schedulable hydro (and non-meaningfully for) wind at this stage'
* Load data.
  AClossFactors(ild)                            'Upwards adjustment to load to account for AC (or intraregional) losses'
  NrgDemand(r,y,t,lb,scenarios)                 'Load (or energy demand) by region, year, time period and load block, GWh (used to create ldcMW)'
  ldcMW(r,y,t,lb,scenarios)                     'MW at each block by region, year and period'
  peakLoadNZ(y,scenarios)                       'Peak load for New Zealand by year, MW'
  peakLoadNI(y,scenarios)                       'Peak load for North Island by year, MW'
* Transmission data.
  numReg                                        'Number of regions (or, if you like, nodes or buses)'
  numT                                          'Number of tranches in piecewise linear loss functions'
  numPaths                                      'Number of transmission paths'
  numAllowedStates(r,rr)                        'Number of allowed states for each active path (including initial state)'
  txEarlyComYr(tupg,r,rr,ps,pss)                'Earliest year that a transmission upgrade can occur (a parameter, not a set)'
  txFixedComYr(tupg,r,rr,ps,pss)                'Fixed year in which a transmission upgrade must occur (a parameter, not a set)'
  reactanceYr(r,rr,y)                           'Reactance by year for each transmission path. Units are p.u.'
  susceptanceYr(r,rr,y)                         'Susceptance by year for each transmission path. Units are p.u.'
  BBincidence(p,r)                              'Bus-branch incidence matrix'
  pCap(r,rr,ps,n)                               'Maximum capacity per loss tranche, MW'
  pLoss(r,rr,ps,n)                              'Losses at maximum capacity of each loss tranche, MW'
  bigLoss(r,rr,ps)                              'Upper bound on losses along path r-rr when in state ps, MW'
  lossSlopeMIP(r,rr,ps,n)                       'Slope of interregional transmission loss function for each loss tranche for each path state (MIP only)'
  lossSlopeRMIP(r,rr,n)                         'Slope of interregional transmission loss function for each loss tranche - same slope for all states (RMIP only)'
  lossIntercept(r,rr,ps,n)                      'Intercept of interregional transmission loss function for each loss tranche'
  txCapitalCost(r,rr,ps)                        'Capital cost of transmission upgrades by path and state, $m'
  txCapCharge(r,rr,ps,y)                        'Annualised or levelised capital charge for new transmission investment - $m/yr'
* Reserve energy data.
  reservesAreas(rc)                             'Reserves areas (single area or systemwide = 1, Island-based reserves = 2)'
  singleReservesReqF(rc)                        'Flag to inidicate if there is a single systemwide reserve requirement'
  windCoverPropn(rc)                            'Proportion of wind to be covered by reserves, (0-1)'
  bigM(ild,ild1)                                'A large positive number'
* Non-free reserves
*  largestNIplant                                "Get this from the peak security data - but you can't have it vary by year"   / 385 /
*  largestSIplant                                "Get this from the peak security data - but you can't have it vary by year"   / 125 /
  largestNIplant                                "Get this from the peak security data - but you can't have it vary by year" 
  largestSIplant                                "Get this from the peak security data - but you can't have it vary by year" 
  freeReserves(r,rr,ps)                         'Free reserves, MW'
  nonFreeReservesCap(r,rr,ps)                   'Non-free reserves max capacity (i.e. amount that the system must pay for), MW'
  bigSwd(r,rr)                                  'Biggest value of non-free reserves in southward direction'
  bigNwd(r,rr)                                  'Biggest value of non-free reserves in northward direction'
  pNFresvCap(r,rr,lvl)                          'Capacity of each piece (or level) of non-free reserves, MW'
  pNFresvCost(r,rr,lvl)                         'Constant cost of each non-free piece (or level) of function, $/MWh'
* Hydrology output data
  historicalHydroOutput(v,hY,m)                 'Historical hydro output sequences by reservoir and month, GWh'
  hydroOutputScalar                             'Scale the hydro output sequence used to determine the timing of new builds'
* Input data summaries
  numExperiments                                'Number of active experiments'
  numSteps                                      'Number of active steps in the solve process'
  numScenarioSets                               'Number of active sets of scenarios'
  numScenarios                                  'Number of active scenarios'
  xFoFm(g)                                      'eXceptional forced outage factor multipliers, i.e. at least one load block less than 0.5 or greater than 1.5'
  avgSRMC(g)                                    'Short run marginal cost of each generation project - averaged over years for the default scenario, $/MWh'
  avgPeakCon(g)                                 'Contribution to peak factor - averaged over years for each plant'
  avgMaxCapFact(g)                              'Maximum capacity factor averaged over periods and load blocks for each plant (hours per block per period are the weights)'
  avgMinCapFact(g)                              'Minimum capacity factor averaged over years and periods for each plant'
  avgMinUtilisation(g)                          'Minimum utilisation of plant averaged over years'
  assumedGWh(g)                                 'Gigawatt hours per plant using assumed technology-specific capacity factors'
  MWtoBuild(k,aggR)                             'MW available for installation by technology, island and NZ'
  GWhtoBuild(k,aggR)                            'Assumed GWh from all plant available for installation by technology, island and NZ'
  loadByRegionYear(r,y)                         'Load by region and year, GWh'
  loadByAggRegionYear(aggR,y)                   'Load by aggregated region and year, GWh'
  peakLoadByYearAggR(y,aggR)                    'Peak load by year for each island and NZ as a whole, MW'
  capexStatistics(k,aggR,stat)                  'Descriptive statistics of plant capex (lumpy and including grid connection costs) by technology, island and NZ'
* Solution reporting
  solverStat                                    'Solver status flag for the current model'
  modelStat                                     'Model status flag for the current model' ;



*===============================================================================================
* 3. Declare model variables and equations.

Free Variables
  TOTALCOST                                     'Discounted total system costs over all modelled years, $m (objective function value)'
  SCENARIO_COSTS(scenarios)                     'Discounted costs that might vary by scenario, $m (a component of objective function value)'
  TX(r,rr,y,t,lb,scenarios)                     'Transmission from region to region in each time period, MW (-ve reduced cost equals s_TXprice???)'
  THETA(r,y,t,lb,scenarios)                     'Bus voltage angle'

Binary Variables
  BGEN(g,y)                                     'Binary variable to identify build year for new generation plant'
  BRET(g,y)                                     'Binary variable to identify endogenous retirement year for the eligble generation plant'
  ISRETIRED(g)                                  'Binary variable to identify if the plant has actually been endogenously retired (0 = not retired, 1 = retired)'
  BTX(r,rr,ps,y)                                'Binary variable indicating the current state of a transmission path'
  NORESVTRFR(ild,ild1,y,t,lb,scenarios)         'Is there available capacity on the HVDC link to transfer energy reserves (0 = Yes, 1 = No)'

Positive Variables
  REFURBCOST(g,y)                               'Annualised generation plant refurbishment expenditure charge, $'
  GENBLDCONT(g,y)                               'Continuous variable to identify build year for new scalable generation plant - for plant in linearPlantBuild set'
  CGEN(g,y)                                     'Continuous variable to identify build year for new scalable generation plant - for plant in integerPlantBuild set (CGEN or BGEN = 0 in any year)'
  BUILD(g,y)                                    'New capacity installed by generating plant and year, MW'
  RETIRE(g,y)                                   'Capacity endogenously retired by generating plant and year, MW'
  CAPACITY(g,y)                                 'Cumulative nameplate capacity at each generating plant in each year, MW'
  TXCAPCHARGES(r,rr,y)                          'Cumulative annualised capital charges to upgrade transmission paths in each modelled year, $m'
  GEN(g,y,t,lb,scenarios)                       'Generation by generating plant and block, GWh'
  VOLLGEN(s,y,t,lb,scenarios)                   'Generation by VOLL plant and block, GWh'
  PUMPEDGEN(g,y,t,lb,scenarios)                 'Energy from pumped hydro (treated like demand), GWh'
  LOSS(r,rr,y,t,lb,scenarios)                   'Transmission losses along each path, MW'
  TXPROJVAR(tupg,y)                             'Continuous 0-1 variable indicating whether an upgrade project is applied'
  TXUPGRADE(r,rr,ps,pss,y)                      'Continuous 0-1 variable indicating whether a transmission upgrade is applied'
* Reserve variables
  RESV(g,rc,y,t,lb,scenarios)                   'Reserve energy supplied, MWh'
  RESVVIOL(rc,ild,y,t,lb,scenarios)             'Reserve energy supply violations, MWh'
  RESVTRFR(rc,ild,ild1,y,t,lb,scenarios)        'Reserve energy transferred from one island to another, MWh'
  RESVREQINT(rc,ild,y,t,lb,scenarios)           'Internally determined energy reserve requirement, MWh'
* Non-free reserve variable
  RESVCOMPONENTS(r,rr,y,t,lb,scenarios,lvl)     'Non-free reserve components, MW'
* Penalty variables
  RENNRGPENALTY(y)                              'Penalty with cost of penaltyViolateRenNrg - used to make renewable energy constraint feasible, GWh'
  PEAK_NZ_PENALTY(y,scenarios)                  'Penalty with cost of penaltyViolatePeakLoad - used to make NZ security constraint feasible, MW'
  PEAK_NI_PENALTY(y,scenarios)                  'Penalty with cost of penaltyViolatePeakLoad - used to make NI security constraint feasible, MW'
  NOWINDPEAK_NI_PENALTY(y,scenarios)            'Penalty with cost of penaltyViolatePeakLoad - used to make NI no wind constraint feasible, MW'
* Slack variables
  ANNMWSLACK(y)                                 'Slack with arbitrarily high cost - used to make annual MW built constraint feasible, MW'
  RENCAPSLACK(y)                                'Slack with arbitrarily high cost - used to make renewable capacity constraint feasible, MW'
  HYDROSLACK(y)                                 'Slack with arbitrarily high cost - used to make limit_hydro constraint feasible, GWh'
  MINUTILSLACK(y)                               'Slack with arbitrarily high cost - used to make minutil constraint feasible, GWh'
  FUELSLACK(y)                                  'Slack with arbitrarily high cost - used to make limit_fueluse constraint feasible, PJ'

Equations
  objectivefn                                   'Calculate discounted total system costs over all modelled years, $m'
  calc_scenarioCosts(scenarios)                 'Calculate discounted costs that might vary by scenario'
  calc_nfreserves(r,rr,y,t,lb,scenarios)        'Calculate non-free reserve components'
  resv_capacity(r,rr,y,t,lb,scenarios,lvl)      'Calculate and impose the relevant capacity on each level of free reserves'
  calc_refurbcost(g,y)                          'Calculate the annualised generation plant refurbishment expenditure charge in each year, $'
  calc_txcapcharges(r,rr,y)                     'Calculate cumulative annualised transmission capital charges in each modelled year, $m'
  bldgenonce(g)                                 'If new generating plant is to be built, ensure it is built only once'
  buildcapint(g,y)                              'If new integer plant is built, ensure built capacity is equal to nameplate capacity'
  buildcapcont(g,y)                             'If new scalable plant is built, ensure built capacity does not exceed nameplate capacity'
  annNewMWcap(y)                                'Restrict aggregate new capacity built in a single year to be less than a specified MW'
  endogpltretire(g,y)                           'Calculate the MW to endogenously retire'
  endogretonce(g)                               'Can only endogenously retire a plant once'
  balance_capacity(g,y)                         'Year to year capacity balance relationship for all plant, MW'
  bal_supdem(r,y,t,lb,scenarios)                'Balance supply and demand in each region, year, time period and load block'
  peak_nz(y,scenarios)                          'Ensure enough capacity to meet peak demand and the winter capacity margin in NZ'
  peak_ni(y,scenarios)                          'Ensure enough capacity to meet peak demand in NI subject to contingencies'
  noWindPeak_ni(y,scenarios)                    'Ensure enough capacity to meet peak demand in NI  subject to contingencies when wind is low'
  limit_maxgen(g,y,t,lb,scenarios)              'Ensure generation in each block does not exceed capacity implied by max capacity factors'
  limit_mingen(g,y,t,lb,scenarios)              'Ensure generation in each block exceeds capacity implied by min capacity factors'
  minutil(g,y,scenarios)                        'Ensure certain generation plant meets a minimum utilisation'
  limit_inflexPlant(g,y,t,lb,lbb,scenarios)     'Impose a restriction on generation from inflexible plant in adjacent load blocks'
  limit_fueluse(f,y,scenarios)                  'Quantum of each fuel used and possibly constrained, PJ'
  limit_nrg(f,y,scenarios)                      'Impose a limit on total energy generated by any one fuel type'
  minreq_rennrg(y,scenarios)                    'Impose a minimum requirement on total energy generated from all renewable sources'
  minreq_rencap(y)                              'Impose a minimum requirement on installed renewable capacity'
  limit_hydro(g,y,t,scenarios)                  'Limit hydro generation according to inflows'
  limit_pumpgen1(g,y,t,scenarios)               'Limit output from pumped hydro in a period to the quantity pumped'
  limit_pumpgen2(g,y,t,scenarios)               'Limit output from pumped hydro in a period to the assumed storage'
  limit_pumpgen3(g,y,t,lb,scenarios)            "Pumped MW can be no more than the scheme's installed MW"
  calcTxLossesMIP(r,rr,ps,y,t,lb,n,scenarios)   'Calculate losses for each tranche of the loss function'
  calcTxLossesRMIP(r,rr,y,t,lb,n,scenarios)     'Calculate losses for each tranche of the loss function'
  tx_capacity(r,rr,y,t,lb,scenarios)            'Calculate the relevant transmission capacity'
  tx_projectdef(tupg,r,rr,ps,pss,y)             'Associate projects to individual upgrades'
  tx_onestate(r,rr,y)                           'A link must be in exactly one state in any given year'
  tx_upgrade(r,rr,ps,y)                         'Make sure the upgrade of a link corresponds to a legitimate state-to-state transition'
  tx_oneupgrade(r,rr,y)                         'Only one upgrade per path in a single year'
  tx_dcflow(r,rr,y,t,lb,scenarios)              'DC load flow equation'
  tx_dcflow0(r,rr,y,t,lb,scenarios)             'DC load flow equation'
  equatetxloss(r,rr,y,t,lb,scenarios)           'Ensure that losses in both directions are equal'
  txGrpConstraint(tgc,y,t,lb,scenarios)         'Group transmission constraints'
  resvsinglereq1(rc,ild,y,t,lb,scenarios)       'Single reserve energy requirement constraint 1'
  genmaxresv1(g,y,t,lb,scenarios)               'Limit the amount of energy reserves per generator'
  resvtrfr1(ild,ild1,y,t,lb,scenarios)          'Limit on the amount of reserve energy transfer - constraint 1'
  resvtrfr2(rc,ild,ild1,y,t,lb,scenarios)       'Limit on the amount of reserve energy transfer - constraint 2'
  resvtrfr3(rc,ild,ild1,y,t,lb,scenarios)       'Limit on the amount of reserve energy transfer - constraint 3'
  resvrequnit(g,rc,ild,y,t,lb,scenarios)        'Reserve energy requirement based on the largest dispatched unit'
  resvreq2(rc,ild,y,t,lb,scenarios)             'Island reserve energy requirement - constraint 2'
  resvreqhvdc(rc,ild,y,t,lb,scenarios)          'Reserve energy requirement based on the HVDC transfer taking into account self-cover'
  resvtrfr4(ild1,ild,y,t,lb,scenarios)          'Limit on the amount of reserve energy transfer - constraint 4'
  resvtrfrdef(ild,ild1,y,t,lb,scenarios)        'Constraint that defines if reserve energy transfer is available'
  resvoffcap(g,y,t,lb,scenarios)                'Offline energy reserve capability'
  resvreqwind(rc,ild,y,t,lb,scenarios)          'Reserve energy requirement based on a specified proportion of dispatched wind generation' ;



*===============================================================================================
* 4. Specify the equations and declare the models.

* NB: Uppercase = variables; lowercase = parameters.

* Objective function - discounted total cost, $m.
objectivefn..
  TOTALCOST =e=
* Add in slacks at arbitrarily high cost.
  slackCost * ( sum(y$( AnnualMWlimit > 150 ), ANNMWSLACK(y) ) +
                sum(y$i_renewCapShare(y), RENCAPSLACK(y) ) +
                sum(y, HYDROSLACK(y) ) +
                sum(y, MINUTILSLACK(y) ) +
                sum(y, FUELSLACK(y) )  ) +
* Add in penalties at user-defined high, but not necessarily arbitrarily high, cost.
  1e-3 * penaltyViolateRenNrg * sum(y$renNrgShrOn, RENNRGPENALTY(y) ) +
* Fixed, variable and HVDC costs - discounted and adjusted for tax
* NB: Fixed costs are scaled by 1/card(t) to convert annual costs to a periodic basis coz discounting is done by period.
* NB: The HVDC charge applies only to committed and new SI projects.
  1e-6 * sum((y,t), PVfacG(y,t) * (1 - taxRate) * (
           ( 1/card(t) ) * 1e3 * (
           sum(g, ensembleFactor(g) * i_fixedOM(g) * CAPACITY(g,y)) +
           sum((g,k,o)$((not demandGen(k)) * mapg_k(g,k) * sigen(g) * possibleToBuild(g) * mapg_o(g,o)), i_HVDCshr(o) * ensembleFactor(g) * i_HVDClevy(y) * CAPACITY(g,y))
           )
         ) ) +
* Generation capital expenditure - discounted
  1e-6 * sum((y,firstPeriod(t),possibleToBuild(g)), PVfacG(y,t) * ensembleFactor(g) * capCharge(g,y) * CAPACITY(g,y) ) +
* Generation refurbishment expenditure - discounted
  1e-6 * sum((y,firstPeriod(t),PossibleToRefurbish(g))$refurbCapCharge(g,y), PVfacG(y,t) * REFURBCOST(g,y) ) +
* Transmission capital expenditure - discounted
  sum((paths,y,firstPeriod(t)),   PVfacT(y,t) * TXCAPCHARGES(paths,y) ) +
* Costs by scenario - computed in following equation
  sum(sc, scenarioWeight(sc) * SCENARIO_COSTS(sc)) ;

calc_scenarioCosts(sc)..
  SCENARIO_COSTS(sc) =e=
* Reserve violation costs, $m
  1e-6 * sum((rc,ild,y,t,lb), penaltyViolateReserves(ild,rc) * RESVVIOL(rc,ild,y,t,lb,sc) ) +
* Lost peak load penalties - why aren't they discounted and adjusted for tax?
  1e-6 * penaltyViolatePeakLoad * sum(y, PEAK_NZ_PENALTY(y,sc) + PEAK_NI_PENALTY(y,sc) + NOWINDPEAK_NI_PENALTY(y,sc) ) +
* Various costs, discounted and adjusted for tax
  1e-6 * (1 - taxRate) * sum((y,t,lb), PVfacG(y,t) * (
* Lost load
    sum(s, 1e3 * VOLLcost * VOLLGEN(s,y,t,lb,sc) ) +
* Generation costs
    sum(g$validYrOperate(g,y), 1e3 * ensembleFactor(g) * srmc(g,y,sc) * GEN(g,y,t,lb,sc) ) +
* Cost of providing reserves ($m)
    sum((g,rc), i_plantReservesCost(g,rc) * ensembleFactor(g) * RESV(g,rc,y,t,lb,sc) ) +
* Cost of providing non-free reserves ($m)
    sum((paths,lvl)$( nwd(paths) or swd(paths) ), hoursPerBlock(t,lb) * pNFresvcost(paths,lvl) * RESVCOMPONENTS(paths,y,t,lb,sc,lvl) )
  ) )  ;

* Calculate non-free reserve components.
calc_nfreserves(paths(r,rr),y,t,lb,sc)$( nwd(r,rr) or swd(r,rr) )..
  sum(lvl, RESVCOMPONENTS(r,rr,y,t,lb,sc,lvl)) =g= TX(r,rr,y,t,lb,sc) - sum(allowedStates(r,rr,ps), freereserves(r,rr,ps) * BTX(r,rr,ps,y)) ;

* Calculate and impose the relevant capacity on each level of free reserves.
resv_capacity(paths,y,t,lb,sc,lvl)$( nwd(paths) or swd(paths) )..
  RESVCOMPONENTS(paths,y,t,lb,sc,lvl) =l= pNFresvcap(paths,lvl) ;

* Compute the annualised generation plant refurbishment expenditure charge in each year.
calc_refurbcost(PossibleToRefurbish(g),y)$refurbCapCharge(g,y)..
  REFURBCOST(g,y) =e= (1 - ISRETIRED(g)) * i_nameplate(g) * ensembleFactor(g) * refurbCapCharge(g,y) ;

* Compute the cumulative annualised transmission upgrade capital expenditure charges.
calc_txcapcharges(paths,y)..
  TXCAPCHARGES(paths,y) =e= TXCAPCHARGES(paths,y-1) + sum(validTransitions(paths,pss,ps), txCapCharge(paths,ps,y) * TXUPGRADE(paths,pss,ps,y) ) ;

* Ensure new plant is built no more than once, if at all (NB: =l= c.f. =e= coz build is not mandatory).
bldGenOnce(possibleToBuild(g))..
  sum(validYrBuild(g,y), ( BGEN(g,y) + CGEN(g,y) )$integerPlantBuild(g) + GENBLDCONT(g,y)$linearPlantBuild(g) ) =l= 1 ;

* If new 'integer or continuous' plant is built, ensure built capacity does not exceed nameplate capacity.
buildCapInt(g,y)$( possibleToBuild(g) * integerPlantBuild(g) * validYrBuild(g,y) )..
  BUILD(g,y) =e= ( BGEN(g,y) + CGEN(g,y) ) * i_nameplate(g) ;

* If new 'continuous' plant is built, ensure built capacity does not exceed nameplate capacity.
buildCapCont(g,y)$( possibleToBuild(g) * linearPlantBuild(g) * validYrBuild(g,y) )..
  BUILD(g,y) =e= GENBLDCONT(g,y) * i_nameplate(g) ;

* Restrict aggregate new capacity built in a single year to be less than AnnualMWlimit (provided AnnualMWlimit exceeds 150MW).
annNewMWcap(y)$( AnnualMWlimit > 150 )..
  sum(validYrBuild(possibleToBuild(g),y), BUILD(g,y)) =l= AnnualMWlimit + ANNMWSLACK(y) ;

* Calculate the year and MW to endogenously retire.
endogpltretire(endogenousRetireDecisnYrs(g,y))..
  RETIRE(g,y+continueAftaEndogRetire(g)) =e= BRET(g,y) * i_nameplate(g) ;

* Can only endogenously retire a plant once (ISRETIRED is a binary -- 0 = not retired, 1 = retired).
endogretonce(possibleToEndogRetire(g))..
  sum(endogenousRetireDecisnYrs(g,y), BRET(g,y)) =e= ISRETIRED(g) ;

* Capacity in year y equals capacity in year y-1 plus builds in y minus retirements in y.
balance_capacity(g,y)..
  CAPACITY(g,y) =e= InitialCapacity(g)$firstYr(y) + CAPACITY(g,y-1)$allButFirstYr(y) + BUILD(g,y)$possibleToBuild(g) - RETIRE(g,y)$endogenousRetireYrs(g,y) - exogMWretired(g,y) ;

* VOLL + Supply + net imports = demand in each block + any pumped generation.
bal_supdem(r,y,t,lb,sc)..
  sum(maps_r(s,r), VOLLGEN(s,y,t,lb,sc)) +
  sum(mapg_r(g,r)$validYrOperate(g,y), GEN(g,y,t,lb,sc)) +
* Transmission and losses with transportation formulation
 (sum(rr$paths(rr,r), ( ( TX(rr,r,y,t,lb,sc) - LOSS(rr,r,y,t,lb,sc) ) * hoursPerBlock(t,lb) * 0.001 ) ) -
  sum(rr$paths(r,rr),   ( TX(r,rr,y,t,lb,sc) * hoursPerBlock(t,lb) * 0.001 ) ) )$( DCloadFlowOn = 0 ) +
* Transmission and losses with DC load flow formulation
 (sum(rr$paths(rr,r), ( ( TX(rr,r,y,t,lb,sc) - 0.5 * LOSS(rr,r,y,t,lb,sc) ) * hoursPerBlock(t,lb) * 0.001 ) ) )$DCloadFlowOn
  =g=
  ldcMW(r,y,t,lb,sc) * hoursPerBlock(t,lb) * 0.001 +
  sum(g$( mapg_r(g,r) * pumpedHydroPlant(g) * validYrOperate(g,y) ), PUMPEDGEN(g,y,t,lb,sc)) ;

* Ensure enough capacity to meet peak demand and the winter capacity margin in NZ.
peak_NZ(y,sc)..
  PEAK_NZ_PENALTY(y,sc) +
  sum(g, CAPACITY(g,y) * peakConPlant(g,y) ) - i_winterCapacityMargin(y) - i_SIACrisk(y) - i_fkSI(y) - i_HVDClossesAtMaxXfer(y)
  =g= peakLoadNZ(y,sc) ;

* Ensure enough capacity to meet peak demand in NI subject to contingencies.
peak_NI(y,sc)..
  PEAK_NI_PENALTY(y,sc) +
  sum(nigen(g), CAPACITY(g,y) * peakConPlant(g,y) ) + i_largestGenerator(y) - i_winterCapacityMargin(y) +
  sum(allowedStates(paths,ps)$nwd(paths), i_txCapacityPO(paths,ps) * BTX(paths,ps,y))
  =g= peakLoadNI(y,sc) ;

* Ensure enough capacity to meet peak demand in NI  subject to contingencies when wind is low.
noWindPeak_NI(y,sc)$niNWpeakCnstrntOn..
  NOWINDPEAK_NI_PENALTY(y,sc) +
  sum(mapg_k(g,k)$( nigen(g) and (not wind(k)) ), CAPACITY(g,y) * NWpeakConPlant(g,y) ) - i_fkNI(y) +
  sum(allowedStates(paths,ps)$nwd(paths), i_txCapacityPO(paths,ps) * BTX(paths,ps,y))
  =g= peakLoadNI(y,sc) ;

* Ensure generation is less than capacity times max capacity factor in each block.
limit_maxgen(validYrOperate(g,y),t,lb,sc)$( ( exist(g) or possibleToBuild(g) ) * ( not reservesOn ) )..
  GEN(g,y,t,lb,sc) =l= 1e-3 * CAPACITY(g,y) * maxCapFactPlant(g,t,lb) * hoursPerBlock(t,lb) ;

* Ensure generation is greater than capacity times min capacity factor in each block.
limit_mingen(validYrOperate(g,y),t,lb,sc)$minCapFactPlant(g,y,t)..
  GEN(g,y,t,lb,sc) =g= 1e-3 * CAPACITY(g,y) * minCapFactPlant(g,y,t) * hoursPerBlock(t,lb) ;

* Minimum ultilisation of plant.
minutil(g,y,sc)$i_minUtilisation(g,y)..
  sum((t,lb)$validYrOperate(g,y), GEN(g,y,t,lb,sc)) + MINUTILSLACK(y) =g= i_minUtilisation(g,y) * 8.76 * CAPACITY(g,y) * (1 - i_fof(g)) ;

* Impose restriction on inflexible plant.
limit_inflexPlant(g,y,t,lb,lbb,sc)$( rightAdjacentBlocks(lb,lbb) and i_inflexiblePlantFactor(g,lbb) )..
  GEN(g,y,t,lb,sc) / hoursPerBlock(t,lb) =g= i_inflexiblePlantFactor(g,lbb) * GEN(g,y,t,lbb,sc) / hoursPerBlock(t,lbb) ;

* Thermal fuel limits.
limit_fueluse(thermalfuel(f),y,sc)$( ( gas(f) * (i_fuelQuantities(f,y) > 0) * (i_fuelQuantities(f,y) < 999) ) or ( diesel(f) * (i_fuelQuantities(f,y) > 0) ) )..
  1e-6 * sum((g,t,lb)$( mapg_f(g,f) * validYrOperate(g,y) ), i_heatrate(g) * GEN(g,y,t,lb,sc) ) =l= i_fuelQuantities(f,y) + FUELSLACK(y) ;

* Impose a limit on total energy generated from any one fuel type.
limit_Nrg(f,y,sc)$( limitNrgByFuelOn * i_maxNrgByFuel(f) )..
  sum((g,t,lb)$( mapg_f(g,f) * validYrOperate(g,y) ), GEN(g,y,t,lb,sc)) =l= i_maxNrgByFuel(f) * sum((g,t,lb)$validYrOperate(g,y), GEN(g,y,t,lb,sc)) ;

* Impose a minimum requirement on total energy generated from all renewable sources.
minReq_RenNrg(y,sc)$renNrgShrOn..
  i_renewNrgShare(y) * ( sum((g,t,lb)$validYrOperate(g,y), GEN(g,y,t,lb,sc)) + i_distdGenRenew(y) + i_distdGenFossil(y) ) =l=
  sum((g,k,t,lb)$( mapg_k(g,k) * renew(k) * validYrOperate(g,y)), GEN(g,y,t,lb,sc)) + i_distdGenRenew(y) +
  RENNRGPENALTY(y) ;

* Impose a minimum requirement on installed renewable capacity.
minReq_RenCap(y)$renCapShrOn..
  i_renewCapShare(y) * sum(possibleToBuild(g), 8.76 * CAPACITY(g,y) * (1 - i_fof(g)) ) =l=
  sum((g,k)$( possibleToBuild(g) * mapg_k(g,k) * renew(k) ), 8.76 * CAPACITY(g,y) * (1 - i_fof(g)) ) +
  RENCAPSLACK(y) ;

* Limit hydro according to energy available in inflows.
limit_hydro(validYrOperate(g,y),t,sc)$schedHydroPlant(g)..
  sum(lb, GEN(g,y,t,lb,sc)) =l= modelledHydroOutput(g,y,t,sc) + hydroOutputUpgrades(g,y,t,sc) * CAPACITY(g,y) + HYDROSLACK(y) ;

* Over the period, ensure that generation from pumped hydro is less than the energy pumped.
limit_pumpgen1(validYrOperate(g,y),t,sc)$pumpedHydroPlant(g)..
  sum(lb, GEN(g,y,t,lb,sc)) =l= i_PumpedHydroEffic(g) * sum(lb, PUMPEDGEN(g,y,t,lb,sc) ) ;

* Over the period, ensure that the energy pumped is less than the storage capacity.
limit_pumpgen2(validYrOperate(g,y),t,sc)$pumpedHydroPlant(g)..
  i_PumpedHydroEffic(g) * sum(lb, PUMPEDGEN(g,y,t,lb,sc) ) =l= sum(mapm_t(m,t), 1) * i_PumpedHydroMonth(g) ;

* The MW pumped can be no greater than the capacity of the project.
limit_pumpgen3(validYrOperate(g,y),t,lb,sc)$pumpedHydroPlant(g)..
  PUMPEDGEN(g,y,t,lb,sc) =l= 0.001 * CAPACITY(g,y) * maxCapFactPlant(g,t,lb) * hoursPerBlock(t,lb) ;

* Calculate piecewise linear transmission losses - MIP.
calcTxLossesMIP(paths(r,rr),ps,y,t,lb,trnch(n),sc)$( (txLossesRMIP = 0) and allowedStates(paths,ps) * bigloss(paths,ps) )..
  LOSS(paths,y,t,lb,sc) =g=
  lossIntercept(paths,ps,n) + lossSlopeMIP(paths,ps,n) * TX(paths,y,t,lb,sc) - bigloss(paths,ps) * ( 1 - BTX(paths,ps,y) ) ;

* Calculate piecewise linear transmission losses - RMIP.
calcTxLossesRMIP(paths(r,rr),y,t,lb,trnch(n),sc)$txLossesRMIP..
  LOSS(paths,y,t,lb,sc) =g=
  sum(allowedStates(paths,ps), BTX(paths,ps,y) * lossIntercept(paths,ps,n)) + lossSlopeRMIP(paths,n) * TX(paths,y,t,lb,sc) ;

* Calculate the relevant transmission capacity and impose it.
tx_capacity(paths,y,t,lb,sc)..
  TX(paths,y,t,lb,sc) =l= sum(allowedStates(paths,ps), i_txCapacity(paths,ps) * BTX(paths,ps,y)) ;

* Associate projects to individual upgrades (also ensures both directions of a path get upgraded together).
tx_projectdef(transitions(tupg,paths,ps,pss),y)..
  TXPROJVAR(tupg,y) =e= TXUPGRADE(paths,ps,pss,y) ;

* A link must be in exactly one state in any given year.
tx_onestate(paths,y)..
  sum(allowedStates(paths,ps), BTX(paths,ps,y)) =e= 1 ;

* Make sure the upgrade of a link corresponds to a legitimate state-to-state transition.
tx_upgrade(paths,ps,y)$upgradeableStates(paths,ps)..
  sum(validTransitions(paths,pss,ps), TXUPGRADE(paths,pss,ps,y)) - sum(validTransitions(paths,ps,pss), TXUPGRADE(paths,ps,pss,y) ) =e=
  BTX(paths,ps,y) - BTX(paths,ps,y-1) ;

* Only one upgrade per path in a single year.
tx_oneupgrade(paths,y)..
  sum(upgradeableStates(paths,ps), sum(validTransitions(paths,pss,ps), TXUPGRADE(paths,pss,ps,y) )) =l= 1 ;

* DC load flow equation for all paths.
tx_dcflow(r,rr,y,t,lb,sc)$( DCloadFlowOn * susceptanceYr(r,rr,y) * regLower(r,rr) )..
  TX(r,rr,y,t,lb,sc) =e= susceptanceYr(r,rr,y) * ( THETA(r,y,t,lb,sc) - THETA(rr,y,t,lb,sc) ) ;

* Ensure that for flow on links without reactance the flow from r to rr = - flow from rr to r
tx_dcflow0(r,rr,y,t,lb,sc)$( DCloadFlowOn * paths(r,rr) * regLower(r,rr) )..
  TX(r,rr,y,t,lb,sc) + TX(rr,r,y,t,lb,sc) =e= 0 ;

* Ensure equality of losses in both directions for the DC load flow representation
* NB: No need for this constraint if the maximum losses on a link = 0 since then the loss variable is fixed in GEMsolve
equatetxloss(r,rr,y,t,lb,sc)$( DCloadFlowOn * paths(r,rr) * regLower(r,rr) * sum(ps, bigloss(r,rr,ps)) )..
  LOSS(r,rr,y,t,lb,sc) =e= LOSS(rr,r,y,t,lb,sc) ;

* Transmission group constraints, i.e. in addition to individual branch limits. Use to cater for contingencies, stability limits, etc.
***txGrpConstraint(validTGC,y,t,lb,sc)$txconstraintactive(y,t,validTGC)..
txGrpConstraint(validTGC,y,t,lb,sc)$DCloadFlowOn..
  sum((p,paths(r,rr))$( (bbincidence(p,r) = 1) * (bbincidence(p,rr) = -1) ), i_txGrpConstraintsLHS(validTGC,p) * TX(paths,y,t,lb,sc) )
  =l= i_txGrpConstraintsRHS(validTGC) ;

* Meet the single reserve requirement.
resvsinglereq1(rc,ild,y,t,lb,sc)$( reservesOn * singleReservesReqF(rc) )..
  sum(g, RESV(g,rc,y,t,lb,sc)) + RESVVIOL(rc,ild,y,t,lb,sc) =g= RESVREQINT(rc,ild,y,t,lb,sc) ;

* Generator energy constraint - substitute for limit_maxgen when reserves are used.
genmaxresv1(validYrOperate(g,y),t,lb,sc)$( reservesOn * ( exist(g) or possibleToBuild(g) ) )..
  1000 * GEN(g,y,t,lb,sc) + sum(rc, RESV(g,rc,y,t,lb,sc)) =l= CAPACITY(g,y) * maxCapFactPlant(g,t,lb) * hoursPerBlock(t,lb) ;

* Reserve transfers - Constraint 1.
resvtrfr1(ild1,ild,y,t,lb,sc)$( reservesOn * interIsland(ild1,ild) )..
  sum( rc, RESVTRFR(rc,ild1,ild,y,t,lb,sc) ) +
  hoursPerBlock(t,lb) * sum((r,rr)$( paths(r,rr) * mapild_r(ild1,r) * mapild_r(ild,rr)), TX(r,rr,y,t,lb,sc) )
  =l= hoursPerBlock(t,lb) * sum((r,rr,ps)$( paths(r,rr) * mapild_r(ild1,r) * mapild_r(ild,rr) ), i_txCapacity(r,rr,ps) * BTX(r,rr,ps,y) ) ;

* Reserve transfers - Constraint 2.
resvtrfr2(rc,ild1,ild,y,t,lb,sc)$( reservesOn * interIsland(ild1,ild) * ( not singleReservesReqF(rc) ) )..
  RESVTRFR(rc,ild1,ild,y,t,lb,sc)
  =l= hoursPerBlock(t,lb) * sum((r,rr,ps)$( paths(r,rr) * mapild_r(ild1,r) * mapild_r(ild,rr) ), i_maxReservesTrnsfr(r,rr,ps,rc) * BTX(r,rr,ps,y) ) ;

* Reserve transfers - Constraint 3.
resvtrfr3(rc,ild1,ild,y,t,lb,sc)$( reservesOn * interIsland(ild1,ild) * ( not singleReservesReqF(rc) ) )..
  RESVTRFR(rc,ild1,ild,y,t,lb,sc) =l= sum(mapg_ild(g,ild1), RESV(g,rc,y,t,lb,sc) ) ;

* Internal reserve requirement determined by the largest dispatched unit during each period.
resvrequnit(g,rc,ild,y,t,lb,sc)$( reservesOn *
  validYrOperate(g,y) * ( exist(g) or possibleToBuild(g) ) * mapg_ild(g,ild) * ( (i_reserveReqMW(y,ild,rc) = -1) or (i_reserveReqMW(y,ild,rc) = -3) )  )..
  RESVREQINT(rc,ild,y,t,lb,sc) =g= 1000 * GEN(g,y,t,lb,sc) * i_UnitLargestProp(g) ;

* Internal island reserve requirement.
resvreq2(rc,ild,y,t,lb,sc)$( reservesOn * ( not singleReservesReqF(rc) ) )..
  sum(mapg_ild(g,ild), RESV(g,rc,y,t,lb,sc) ) + sum(interIsland(ild1,ild), RESVTRFR(rc,ild1,ild,y,t,lb,sc) ) +
  RESVVIOL(rc,ild,y,t,lb,sc) =g= RESVREQINT(rc,ild,y,t,lb,sc) ;

* Internal reserve requirement determined by the HVDC transfer taking into account self-cover.
resvreqhvdc(rc,ild,y,t,lb,sc)$( reservesOn * ( not singleReservesReqF(rc) ) )..
  RESVREQINT(rc,ild,y,t,lb,sc) =g=
  hoursPerBlock(t,lb) * sum((r,rr,ild1)$( paths(r,rr) * mapild_r(ild1,r) * mapild_r(ild,rr) * interIsland(ild,ild1) ), TX(r,rr,y,t,lb,sc) ) -
  hoursPerBlock(t,lb) * sum((r,rr,ps,ild1)$( paths(r,rr) * mapild_r(ild1,r) * mapild_r(ild,rr) * interIsland(ild,ild1) ), i_txCapacityPO(r,rr,ps) * BTX(r,rr,ps,y) ) ;

* Reserve energy transfer - Constraint 4.
resvtrfr4(interIsland(ild1,ild),y,t,lb,sc)$reservesOn..
  sum(rc, RESVTRFR(rc,ild1,ild,y,t,lb,sc) )
  =l= hoursPerBlock(t,lb) * sum((r,rr,ps)$(paths(r,rr) * mapild_r(ild1,r) * mapild_r(ild,rr)) , i_txCapacity(r,rr,ps)) * ( 1 - NORESVTRFR(ild1,ild,y,t,lb,sc) ) ;

* Constraint that defines the reserve transfer capability.
resvtrfrdef(interIsland(ild1,ild),y,t,lb,sc)$reservesOn..
  sum((r,rr)$( paths(r,rr) * mapild_r(ild1,r) * mapild_r(ild,rr) ), TX(r,rr,y,t,lb,sc) ) -
  sum((r,rr,ps)$( paths(r,rr) * mapild_r(ild1,r) * mapild_r(ild,rr) ), i_txCapacityPO(r,rr,ps) * BTX(r,rr,ps,y) )
  =l= NORESVTRFR(ild1,ild,y,t,lb,sc) * bigM(ild1,ild) ;

* Constraint to define the offline reserve capability.
resvoffcap(validYrOperate(g,y),t,lb,sc)$( reservesOn * ( exist(g) or possibleToBuild(g) ) * (sum(rc, reservesCapability(g,rc))) * ( not i_offlineReserve(g) ) )..
  sum(rc, RESV(g,rc,y,t,lb,sc)) =l= 1000 * GEN(g,y,t,lb,sc) ;

* Constraint to ensure that reserves cover a certain proportion of wind generation.
resvreqwind(rc,ild,y,t,lb,sc)$( reservesOn * ( (i_reserveReqMW(y,ild,rc) = -2) or (i_reserveReqMW(y,ild,rc) = -3) ) * windCoverPropn(rc) )..
  RESVREQINT(rc,ild,y,t,lb,sc)
  =g= windCoverPropn(rc) * sum(mapg_k(g,k)$( wind(k) * mapg_ild(g,ild) * validYrOperate(g,y) ), 1000 * GEN(g,y,t,lb,sc) ) ;

Model DISP Dispatch model with build forced and timing fixed  /
  objectivefn, calc_scenarioCosts, calc_nfreserves, resv_capacity, calc_refurbcost, calc_txcapcharges,
  balance_capacity, bal_supdem, peak_nz, peak_ni, noWindPeak_ni
  limit_maxgen, limit_mingen, minutil, limit_inflexPlant, limit_fueluse, limit_Nrg, minReq_RenNrg, minReq_RenCap, limit_hydro
  limit_pumpgen1, limit_pumpgen2, limit_pumpgen3
  calcTxLossesMIP, calcTxLossesRMIP, tx_capacity, tx_projectdef, tx_onestate, tx_upgrade, tx_oneupgrade
  tx_dcflow, tx_dcflow0, equatetxloss, txGrpConstraint
  resvsinglereq1, genmaxresv1, resvtrfr1, resvtrfr2, resvtrfr3, resvrequnit
  resvreq2, resvreqhvdc, resvtrfr4, resvtrfrdef, resvoffcap, resvreqwind / ;

* Model GEM is just model DISP with 6 additional constraints added:
Model GEM Generation expansion model / DISP, bldGenOnce, buildCapInt, buildCapCont, annNewMWcap, endogpltretire, endogretonce / ;



*===============================================================================================
* 5. Declare the 's_' parameters and specify the statements used to collect up results at end of each experiment.
*    NB: The 's' prefix denotes 'solution' to model.
*        Multiply $m/GWh by 1000 to get $/MWh.
*        Divide $m/GWh by 100 to get cents per kWh.
*        Units not yet verified in all cases and some descriptions could be made more meaningful.

Parameters
* Free Variables
  s_TOTALCOST(steps,scenarioSets)                                  'Discounted total system costs over all modelled years, $m (objective function value)'
  s_SCENARIO_COSTS(steps,scenarioSets,scenarios)                   'Discounted costs that might vary by scenario, $m (a component of objective function value)'
  s_TX(steps,scenarioSets,r,rr,y,t,lb,scenarios)                   'Transmission from region to region in each time period, MW (-ve reduced cost equals s_TXprice???)'
  s_THETA(steps,scenarioSets,r,y,t,lb,scenarios)                   'Bus voltage angle'
* Binary Variables
  s_BGEN(steps,scenarioSets,g,y)                                   'Binary variable to identify build year for new generation plant'
  s_BRET(steps,scenarioSets,g,y)                                   'Binary variable to identify endogenous retirement year for the eligble generation plant'
  s_ISRETIRED(steps,scenarioSets,g)                                'Binary variable to identify if the plant has actually been endogenously retired (0 = not retired, 1 = retired)'
  s_BTX(steps,scenarioSets,r,rr,ps,y)                              'Binary variable indicating the current state of a transmission path'
  s_NORESVTRFR(steps,scenarioSets,ild,ild1,y,t,lb,scenarios)       'Is there available capacity on the HVDC link to transfer energy reserves (0 = Yes, 1 = No)'
* Positive Variables
  s_REFURBCOST(steps,scenarioSets,g,y)                             'Annualised generation plant refurbishment expenditure charge, $'
  s_GENBLDCONT(steps,scenarioSets,g,y)                             'Continuous variable to identify build year for new scalable generation plant - for plant in linearPlantBuild set'
  s_CGEN(steps,scenarioSets,g,y)                                   'Continuous variable to identify build year for new scalable generation plant - for plant in integerPlantBuild set (CGEN or BGEN = 0 in any year)'
  s_BUILD(steps,scenarioSets,g,y)                                  'New capacity installed by generating plant and year, MW'
  s_RETIRE(steps,scenarioSets,g,y)                                 'Capacity endogenously retired by generating plant and year, MW'
  s_CAPACITY(steps,scenarioSets,g,y)                               'Cumulative nameplate capacity at each generating plant in each year, MW'
  s_TXCAPCHARGES(steps,scenarioSets,r,rr,y)                        'Cumulative annualised capital charges to upgrade transmission paths in each modelled year, $m'
  s_GEN(steps,scenarioSets,g,y,t,lb,scenarios)                     'Generation by generating plant and block, GWh'
  s_VOLLGEN(steps,scenarioSets,s,y,t,lb,scenarios)                 'Generation by VOLL plant and block, GWh'
  s_PUMPEDGEN(steps,scenarioSets,g,y,t,lb,scenarios)               'Energy from pumped hydro (treated like demand), GWh'
  s_LOSS(steps,scenarioSets,r,rr,y,t,lb,scenarios)                 'Transmission losses along each path, MW'
  s_TXPROJVAR(steps,scenarioSets,tupg,y)                           'Continuous 0-1 variable indicating whether an upgrade project is applied'
  s_TXUPGRADE(steps,scenarioSets,r,rr,ps,pss,y)                    'Continuous 0-1 variable indicating whether a transmission upgrade is applied'
* Reserve variables
  s_RESV(steps,scenarioSets,g,rc,y,t,lb,scenarios)                 'Reserve energy supplied, MWh'
  s_RESVVIOL(steps,scenarioSets,rc,ild,y,t,lb,scenarios)           'Reserve energy supply violations, MWh'
  s_RESVTRFR(steps,scenarioSets,rc,ild,ild1,y,t,lb,scenarios)      'Reserve energy transferred from one island to another, MWh'
  s_RESVREQINT(steps,scenarioSets,rc,ild,y,t,lb,scenarios)         'Internally determined energy reserve requirement, MWh'
* Non-free reserve variable
  s_RESVCOMPONENTS(steps,scenarioSets,r,rr,y,t,lb,scenarios,lvl)   'Non-free reserve components, MW'
* Penalty variables
  s_RENNRGPENALTY(steps,scenarioSets,y)                            'Penalty with cost of penaltyViolateRenNrg - used to make renewable energy constraint feasible, GWh'
  s_PEAK_NZ_PENALTY(steps,scenarioSets,y,scenarios)                'Penalty with cost of penaltyViolatePeakLoad - used to make NZ security constraint feasible, MW'
  s_PEAK_NI_PENALTY(steps,scenarioSets,y,scenarios)                'Penalty with cost of penaltyViolatePeakLoad - used to make NI security constraint feasible, MW'
  s_NOWINDPEAK_NI_PENALTY(steps,scenarioSets,y,scenarios)          'Penalty with cost of penaltyViolatePeakLoad - used to make NI no wind constraint feasible, MW'
* Slack variables
  s_ANNMWSLACK(steps,scenarioSets,y)                               'Slack with arbitrarily high cost - used to make annual MW built constraint feasible, MW'
  s_RENCAPSLACK(steps,scenarioSets,y)                              'Slack with arbitrarily high cost - used to make renewable capacity constraint feasible, MW'
  s_HYDROSLACK(steps,scenarioSets,y)                               'Slack with arbitrarily high cost - used to make limit_hydro constraint feasible, GWh'
  s_MINUTILSLACK(steps,scenarioSets,y)                             'Slack with arbitrarily high cost - used to make minutil constraint feasible, GWh'
  s_FUELSLACK(steps,scenarioSets,y)                                'Slack with arbitrarily high cost - used to make limit_fueluse constraint feasible, PJ'
* Equations (ignore the objective function)
  s_calc_scenarioCosts(steps,scenarioSets,scenarios)               'Calculate discounted costs that might vary by scenario'
  s_calc_nfreserves(steps,scenarioSets,r,rr,y,t,lb,scenarios)      'Calculate non-free reserve components'
  s_resv_capacity(steps,scenarioSets,r,rr,y,t,lb,scenarios,lvl)    'Calculate and impose the relevant capacity on each step of free reserves'
  s_calc_refurbcost(steps,scenarioSets,g,y)                        'Calculate the annualised generation plant refurbishment expenditure charge in each year, $'
  s_calc_txcapcharges(steps,scenarioSets,r,rr,y)                   'Calculate cumulative annualised transmission capital charges in each modelled year, $m'
  s_bldgenonce(steps,scenarioSets,g)                               'If new generating plant is to be built, ensure it is built only once'
  s_buildcapint(steps,scenarioSets,g,y)                            'If new integer plant is built, ensure built capacity is equal to nameplate capacity'
  s_buildcapcont(steps,scenarioSets,g,y)                           'If new scalable plant is built, ensure built capacity does not exceed nameplate capacity'
  s_annNewMWcap(steps,scenarioSets,y)                              'Restrict aggregate new capacity built in a single year to be less than a specified MW'
  s_endogpltretire(steps,scenarioSets,g,y)                         'Calculate the MW to endogenously retire'
  s_endogretonce(steps,scenarioSets,g)                             'Can only endogenously retire a plant once'
  s_balance_capacity(steps,scenarioSets,g,y)                       'Year to year capacity balance relationship for all plant, MW'
  s_bal_supdem(steps,scenarioSets,r,y,t,lb,scenarios)              'Balance supply and demand in each region, year, time period and load block'
  s_peak_nz(steps,scenarioSets,y,scenarios)                        'Ensure enough capacity to meet peak demand and the winter capacity margin in NZ'
  s_peak_ni(steps,scenarioSets,y,scenarios)                        'Ensure enough capacity to meet peak demand in NI subject to contingencies'
  s_noWindPeak_ni(steps,scenarioSets,y,scenarios)                  'Ensure enough capacity to meet peak demand in NI  subject to contingencies when wind is low'
  s_limit_maxgen(steps,scenarioSets,g,y,t,lb,scenarios)            'Ensure generation in each block does not exceed capacity implied by max capacity factors'
  s_limit_mingen(steps,scenarioSets,g,y,t,lb,scenarios)            'Ensure generation in each block exceeds capacity implied by min capacity factors'
  s_minutil(steps,scenarioSets,g,y,scenarios)                      'Ensure certain generation plant meets a minimum utilisation'
  s_limit_inflexPlant(steps,scenarioSets,g,y,t,lb,lbb,scenarios)   'Impose a restriction on generation from inflexible plant in adjacent load blocks'
  s_limit_fueluse(steps,scenarioSets,f,y,scenarios)                'Quantum of each fuel used and possibly constrained, PJ'
  s_limit_nrg(steps,scenarioSets,f,y,scenarios)                    'Impose a limit on total energy generated by any one fuel type'
  s_minreq_rennrg(steps,scenarioSets,y,scenarios)                  'Impose a minimum requirement on total energy generated from all renewable sources'
  s_minreq_rencap(steps,scenarioSets,y)                            'Impose a minimum requirement on installed renewable capacity'
  s_limit_hydro(steps,scenarioSets,g,y,t,scenarios)                'Limit hydro generation according to inflows'
  s_limit_pumpgen1(steps,scenarioSets,g,y,t,scenarios)             'Limit output from pumped hydro in a period to the quantity pumped'
  s_limit_pumpgen2(steps,scenarioSets,g,y,t,scenarios)             'Limit output from pumped hydro in a period to the assumed storage'
  s_limit_pumpgen3(steps,scenarioSets,g,y,t,lb,scenarios)          "Pumped MW can be no more than the scheme's installed MW"
  s_calcTxLossesMIP(steps,scenarioSets,r,rr,ps,y,t,lb,n,scenarios) 'Calculate losses for each tranche of the loss function'
  s_calcTxLossesRMIP(steps,scenarioSets,r,rr,y,t,lb,n,scenarios)   'Calculate losses for each tranche of the loss function'
  s_tx_capacity(steps,scenarioSets,r,rr,y,t,lb,scenarios)          'Calculate the relevant transmission capacity'
  s_tx_projectdef(steps,scenarioSets,tupg,r,rr,ps,pss,y)           'Associate projects to individual upgrades'
  s_tx_onestate(steps,scenarioSets,r,rr,y)                         'A link must be in exactly one state in any given year'
  s_tx_upgrade(steps,scenarioSets,r,rr,ps,y)                       'Make sure the upgrade of a link corresponds to a legitimate state-to-state transition'
  s_tx_oneupgrade(steps,scenarioSets,r,rr,y)                       'Only one upgrade per path in a single year'
  s_tx_dcflow(steps,scenarioSets,r,rr,y,t,lb,scenarios)            'DC load flow equation'
  s_tx_dcflow0(steps,scenarioSets,r,rr,y,t,lb,scenarios)           'DC load flow equation'
  s_equatetxloss(steps,scenarioSets,r,rr,y,t,lb,scenarios)         'Ensure that losses in both directions are equal'
  s_txGrpConstraint(steps,scenarioSets,tgc,y,t,lb,scenarios)       'Group transmission constraints'
  s_resvsinglereq1(steps,scenarioSets,rc,ild,y,t,lb,scenarios)     'Single reserve energy requirement constraint 1'
  s_genmaxresv1(steps,scenarioSets,g,y,t,lb,scenarios)             'Limit the amount of energy reserves per generator'
  s_resvtrfr1(steps,scenarioSets,ild,ild1,y,t,lb,scenarios)        'Limit on the amount of reserve energy transfer - constraint 1'
  s_resvtrfr2(steps,scenarioSets,rc,ild,ild1,y,t,lb,scenarios)     'Limit on the amount of reserve energy transfer - constraint 2'
  s_resvtrfr3(steps,scenarioSets,rc,ild,ild1,y,t,lb,scenarios)     'Limit on the amount of reserve energy transfer - constraint 3'
  s_resvrequnit(steps,scenarioSets,g,rc,ild,y,t,lb,scenarios)      'Reserve energy requirement based on the largest dispatched unit'
  s_resvreq2(steps,scenarioSets,rc,ild,y,t,lb,scenarios)           'Island reserve energy requirement - constraint 2'
  s_resvreqhvdc(steps,scenarioSets,rc,ild,y,t,lb,scenarios)        'Reserve energy requirement based on the HVDC transfer taking into account self-cover'
  s_resvtrfr4(steps,scenarioSets,ild1,ild,y,t,lb,scenarios)        'Limit on the amount of reserve energy transfer - constraint 4'
  s_resvtrfrdef(steps,scenarioSets,ild,ild1,y,t,lb,scenarios)      'Constraint that defines if reserve energy transfer is available'
  s_resvoffcap(steps,scenarioSets,g,y,t,lb,scenarios)              'Offline energy reserve capability'
  s_resvreqwind(steps,scenarioSets,rc,ild,y,t,lb,scenarios)        'Reserve energy requirement based on a specified proportion of dispatched wind generation'
  ;

* Now push the statements that collect up results into a file called CollectResults.inc. This file gets $include'd into GEMsolve.gms
$onecho > CollectResults.inc
* Free Variables
  s_TOTALCOST(steps,scenSet)                            = TOTALCOST.l ;
  s_SCENARIO_COSTS(steps,scenSet,sc)                    = SCENARIO_COSTS.l(sc) ;
  if(DCloadFlowOn,
    s_TX(steps,scenSet,r,rr,y,t,lb,sc)$( TX.l(r,rr,y,t,lb,sc) > 0 ) = TX.l(r,rr,y,t,lb,sc) ;
    else
    s_TX(steps,scenSet,r,rr,y,t,lb,sc)                  = TX.l(r,rr,y,t,lb,sc) ;
  ) ;
  s_THETA(steps,scenSet,r,y,t,lb,sc)                    = THETA.l(r,y,t,lb,sc) ;
* Binary Variables
  s_BRET(steps,scenSet,g,y)                             = BRET.l(g,y) ;
  s_ISRETIRED(steps,scenSet,g)                          = ISRETIRED.l(g) ;
  s_BTX(steps,scenSet,r,rr,ps,y)                        = BTX.l(r,rr,ps,y) ;
  s_NORESVTRFR(steps,scenSet,ild,ild1,y,t,lb,sc)        = NORESVTRFR.l(ild,ild1,y,t,lb,sc) ;
* Positive Variables
  s_REFURBCOST(steps,scenSet,g,y)                       = REFURBCOST.l(g,y) ;
  s_BUILD(steps,scenSet,g,y)                            = BUILD.l(g,y) ;
  s_RETIRE(steps,scenSet,g,y)                           = RETIRE.l(g,y) ;
  s_CAPACITY(steps,scenSet,g,y)                         = CAPACITY.l(g,y) ;
  s_TXCAPCHARGES(steps,scenSet,paths,y)                 = TXCAPCHARGES.l(paths,y) ;
  s_GEN(steps,scenSet,g,y,t,lb,sc)                      = GEN.l(g,y,t,lb,sc) ;
  s_VOLLGEN(steps,scenSet,s,y,t,lb,sc)                  = VOLLGEN.l(s,y,t,lb,sc) ;
  s_PUMPEDGEN(steps,scenSet,g,y,t,lb,sc)                = PUMPEDGEN.l(g,y,t,lb,sc) ;
  s_LOSS(steps,scenSet,r,rr,y,t,lb,sc)                  = LOSS.l(r,rr,y,t,lb,sc) ;
  s_TXPROJVAR(steps,scenSet,tupg,y)                     = TXPROJVAR.l(tupg,y) ;
  s_TXUPGRADE(steps,scenSet,r,rr,ps,pss,y)              = TXUPGRADE.l(r,rr,ps,pss,y) ;
* Reserve variables
  s_RESV(steps,scenSet,g,rc,y,t,lb,sc)                  = RESV.l(g,rc,y,t,lb,sc) ;
  s_RESVVIOL(steps,scenSet,rc,ild,y,t,lb,sc)            = RESVVIOL.l(RC,ILD,y,t,lb,sc) ;
  s_RESVTRFR(steps,scenSet,rc,ild,ild1,y,t,lb,sc)       = RESVTRFR.l(rc,ild1,ild,y,t,lb,sc) ;
  s_RESVREQINT(steps,scenSet,rc,ild,y,t,lb,sc)          = RESVREQINT.l(rc,ild,y,t,lb,sc) ;
* Non-free reserve variable
  s_RESVCOMPONENTS(steps,scenSet,r,rr,y,t,lb,sc,lvl)    = RESVCOMPONENTS.l(r,rr,y,t,lb,sc,lvl) ;
* Penalty variables
  s_RENNRGPENALTY(steps,scenSet,y)                      = RENNRGPENALTY.l(y) ;
  s_PEAK_NZ_PENALTY(steps,scenSet,y,sc)                 = PEAK_NZ_PENALTY.l(y,sc) ;
  s_PEAK_NI_PENALTY(steps,scenSet,y,sc)                 = PEAK_NI_PENALTY.l(y,sc) ;
  s_NOWINDPEAK_NI_PENALTY(steps,scenSet,y,sc)           = NOWINDPEAK_NI_PENALTY.l(y,sc) ;
* Slack variables
  s_ANNMWSLACK(steps,scenSet,y)                         = ANNMWSLACK.l(y) ;
  s_RENCAPSLACK(steps,scenSet,y)                        = RENCAPSLACK.l(y) ;
  s_HYDROSLACK(steps,scenSet,y)                         = HYDROSLACK.l(y) ;
  s_MINUTILSLACK(steps,scenSet,y)                       = MINUTILSLACK.l(y) ;
  s_FUELSLACK(steps,scenSet,y)                          = FUELSLACK.l(y) ;
* Equations, i.e. marginal values. (ignore the objective function)
  s_calc_scenarioCosts(steps,scenSet,sc)                = calc_scenarioCosts.m(sc) ;
  s_calc_nfreserves(steps,scenSet,r,rr,y,t,lb,sc)       = calc_nfreserves.m(r,rr,y,t,lb,sc) ;
  s_resv_capacity(steps,scenSet,r,rr,y,t,lb,sc,lvl)     = resv_capacity.m(r,rr,y,t,lb,sc,lvl) ;
  s_calc_refurbcost(steps,scenSet,g,y)                  = calc_refurbcost.m(g,y) ;
  s_calc_txcapcharges(steps,scenSet,paths,y)            = calc_txcapcharges.m(paths,y) ;
  s_balance_capacity(steps,scenSet,g,y)                 = balance_capacity.m(g,y) ;
  s_bal_supdem(steps,scenSet,r,y,t,lb,sc)               = bal_supdem.m(r,y,t,lb,sc) ;
  s_peak_nz(steps,scenSet,y,sc)                         = peak_NZ.m(y,sc) ;
  s_peak_ni(steps,scenSet,y,sc)                         = peak_NI.m(y,sc) ;
  s_noWindPeak_ni(steps,scenSet,y,sc)                   = noWindPeak_NI.m(y,sc) ;
  s_limit_maxgen(steps,scenSet,g,y,t,lb,sc)             = limit_maxgen.m(g,y,t,lb,sc) ;
  s_limit_mingen(steps,scenSet,g,y,t,lb,sc)             = limit_mingen.m(g,y,t,lb,sc) ;
  s_minutil(steps,scenSet,g,y,sc)                       = minutil.m(g,y,sc) ;
  s_limit_inflexPlant(steps,scenSet,g,y,t,lb,lbb,sc)    = limit_inflexPlant.m(g,y,t,lb,lbb,sc) ;
  s_limit_fueluse(steps,scenSet,f,y,sc)                 = limit_fueluse.m(f,y,sc) ;
  s_limit_nrg(steps,scenSet,f,y,sc)                     = limit_nrg.m(f,y,sc) ;
  s_minreq_rennrg(steps,scenSet,y,sc)                   = minReq_renNrg.m(y,sc) ;
  s_minreq_rencap(steps,scenSet,y)                      = minReq_renCap.m(y) ;
  s_limit_hydro(steps,scenSet,g,y,t,sc)                 = limit_hydro.m(g,y,t,sc) ;
  s_limit_pumpgen1(steps,scenSet,g,y,t,sc)              = limit_pumpgen1.m(g,y,t,sc) ;
  s_limit_pumpgen2(steps,scenSet,g,y,t,sc)              = limit_pumpgen2.m(g,y,t,sc) ;
  s_limit_pumpgen3(steps,scenSet,g,y,t,lb,sc)           = limit_pumpgen3.m(g,y,t,lb,sc) ;
  s_calcTxLossesMIP(steps,scenSet,r,rr,ps,y,t,lb,n,sc)  = calcTxLossesMIP.m(r,rr,ps,y,t,lb,n,sc) ;
  s_calcTxLossesRMIP(steps,scenSet,r,rr,y,t,lb,n,sc)    = calcTxLossesRMIP.m(r,rr,y,t,lb,n,sc) ;
  s_tx_capacity(steps,scenSet,r,rr,y,t,lb,sc)           = tx_capacity.m(r,rr,y,t,lb,sc) ;
  s_tx_projectdef(steps,scenSet,tupg,r,rr,ps,pss,y)     = tx_projectdef.m(tupg,r,rr,ps,pss,y) ;
  s_tx_onestate(steps,scenSet,r,rr,y)                   = tx_onestate.m(r,rr,y) ;
  s_tx_upgrade(steps,scenSet,r,rr,ps,y)                 = tx_upgrade.m(r,rr,ps,y) ;
  s_tx_oneupgrade(steps,scenSet,r,rr,y)                 = tx_oneupgrade.m(r,rr,y) ;
  s_tx_dcflow(steps,scenSet,r,rr,y,t,lb,sc)             = tx_dcflow.m(r,rr,y,t,lb,sc) ;
  s_tx_dcflow0(steps,scenSet,r,rr,y,t,lb,sc)            = tx_dcflow0.m(r,rr,y,t,lb,sc) ;
  s_equatetxloss(steps,scenSet,r,rr,y,t,lb,sc)          = equatetxloss.m(r,rr,y,t,lb,sc) ;
  s_txGrpConstraint(steps,scenSet,tgc,y,t,lb,sc)        = txGrpConstraint.m(tgc,y,t,lb,sc) ;
  s_resvsinglereq1(steps,scenSet,rc,ild,y,t,lb,sc)      = resvsinglereq1.m(rc,ild,y,t,lb,sc) ;
  s_genmaxresv1(steps,scenSet,g,y,t,lb,sc)              = genmaxresv1.m(g,y,t,lb,sc) ;
  s_resvtrfr1(steps,scenSet,ild,ild1,y,t,lb,sc)         = resvtrfr1.m(ild,ild1,y,t,lb,sc) ;
  s_resvtrfr2(steps,scenSet,rc,ild,ild1,y,t,lb,sc)      = resvtrfr2.m(rc,ild,ild1,y,t,lb,sc) ;
  s_resvtrfr3(steps,scenSet,rc,ild,ild1,y,t,lb,sc)      = resvtrfr3.m(rc,ild,ild1,y,t,lb,sc) ;
  s_resvrequnit(steps,scenSet,g,rc,ild,y,t,lb,sc)       = resvrequnit.m(g,rc,ild,y,t,lb,sc) ;
  s_resvreq2(steps,scenSet,rc,ild,y,t,lb,sc)            = resvreq2.m(rc,ild,y,t,lb,sc) ;
  s_resvreqhvdc(steps,scenSet,rc,ild,y,t,lb,sc)         = resvreqhvdc.m(rc,ild,y,t,lb,sc) ;
  s_resvtrfr4(steps,scenSet,ild1,ild,y,t,lb,sc)         = resvtrfr4.m(ild1,ild,y,t,lb,sc) ;
  s_resvtrfrdef(steps,scenSet,ild,ild1,y,t,lb,sc)       = resvtrfrdef.m(ild,ild1,y,t,lb,sc) ;
  s_resvoffcap(steps,scenSet,g,y,t,lb,sc)               = resvoffcap.m(g,y,t,lb,sc) ;
  s_resvreqwind(steps,scenSet,rc,ild,y,t,lb,sc)         = resvreqwind.m(rc,ild,y,t,lb,sc) ;
* Now write the statements that are contingent on the model type being solved.
* NB: these statements will not be executed when included in GEMsolve if RunType = 2.
$if %RunType%==2 $goto skip
* Variables
  s_BGEN(steps,scenSet,g,y)                             = BGEN.l(g,y) ;
  s_GENBLDCONT(steps,scenSet,g,y)                       = GENBLDCONT.l(g,y) ;
  s_CGEN(steps,scenSet,g,y)                             = CGEN.l(g,y) ;
* Equations
  s_bldgenonce(steps,scenSet,g)                         = bldGenOnce.m(g) ;
  s_buildcapint(steps,scenSet,g,y)                      = buildCapInt.m(g,y) ;
  s_buildcapcont(steps,scenSet,g,y)                     = buildCapCont.m(g,y) ;
  s_annNewMWcap(steps,scenSet,y)                        = annNewMWcap.m(y) ;
  s_endogpltretire(steps,scenSet,g,y)                   = endogpltretire.m(g,y) ;
  s_endogretonce(steps,scenSet,g)                       = endogretonce.m(g) ;
$label skip
$offecho



*===============================================================================================
* 6. Declare some (but not all) output files to be created by GEMdata and GEMsolve.

Files
  VOLLplant 'Write VOLL plant on the fly'      / VOLLplant.inc /
*  rep       'Write to a solve summary report'  / Report.txt /
  con       'Write to the console'             / con /
  dummy ;

VOLLplant.lw = 0 ;
*rep.lw = 0 ; rep.ap = 1 ;
con.lw = 0 ;




* End of file.
