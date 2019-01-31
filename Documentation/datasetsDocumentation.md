---
title: "gemR data documentation"
output: 
  html_document: 
    keep_md: yes
    theme: simplex
    toc: yes
    toc_float: yes
---

<style>
    body {font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;}
    .table {width: 20%}
    .list-group-item.active, .list-group-item.active:hover, .list-group-item.active:focus {background-color: #00346b; border-color: #00346b; opacity: 0.8;}
    h1, h2, h3 {color: #00346b;}
</style>



## Purpose

To document where **sets**, **subsets**, **parameters** and **setup variables** can be found within the input CSVs.

## Sets

> *./Data/GEMdataInput/Sets*

### fuelGroups 


|Sets |
|:----|
|fg   |
*** 
 
### fuels 


|Sets |
|:----|
|f    |
*** 
 
### generationOwners 


|Sets |
|:----|
|o    |
*** 
 
### generationPlant 


|Sets |
|:----|
|g    |
*** 
 
### generationTechnologies 


|Sets |
|:----|
|k    |
*** 
 
### hydroSystems 


|Sets |
|:----|
|v    |
*** 
 
### hydroYears 


|Sets |
|:----|
|hY   |
*** 
 
### loadBlocks 


|Sets |
|:----|
|lb   |
*** 
 
### regions 


|Sets |
|:----|
|r    |
*** 
 
### reserveClasses 


|Sets |
|:----|
|rc   |
*** 
 
### substations 


|Sets |
|:----|
|i    |
*** 
 
### timePeriods 


|Sets |
|:----|
|t    |
*** 
 
### transmissionGrpConstraints 


|Sets |
|:----|
|tgc  |
*** 
 
### transmissionPaths 


|Sets |
|:----|
|p    |
*** 
 
### transmissionPathStates 


|Sets |
|:----|
|ps   |
*** 
 
### transmissionUpradeProjects 


|Sets |
|:----|
|tupg |
*** 
 
### zones 


|Sets |
|:----|
|e    |
*** 
 

## Subsets

> *./Data/GEMdataInput/Subsets*

### Benmore 


|Sets |
|:----|
|i    |
*** 
 
### coal 


|Sets |
|:----|
|f    |
*** 
 
### cogen 


|Sets |
|:----|
|k    |
*** 
 
### demandGen 


|Sets |
|:----|
|k    |
*** 
 
### diesel 


|Sets |
|:----|
|f    |
*** 
 
### endogRetire 


|Sets |
|:----|
|k    |
*** 
 
### exist 


|Sets |
|:----|
|g    |
*** 
 
### fuelColor 


|Sets  |
|:-----|
|f     |
|red   |
|green |
|blue  |
*** 
 
### fuelGrpColor 


|Sets  |
|:-----|
|fg    |
|red   |
|green |
|blue  |
*** 
 
### gas 


|Sets |
|:----|
|f    |
*** 
 
### Haywards 


|Sets |
|:----|
|i    |
*** 
 
### hydroPumped 


|Sets |
|:----|
|k    |
*** 
 
### hydroSched 


|Sets |
|:----|
|k    |
*** 
 
### islandCentroid 


|Sets |
|:----|
|i    |
|ild  |
*** 
 
### lignite 


|Sets |
|:----|
|f    |
*** 
 
### linearBuildTech 


|Sets |
|:----|
|k    |
*** 
 
### mapArcNode 


|Sets |
|:----|
|p    |
|r    |
|rr   |
*** 
 
### fg 


|Sets |
|:----|
|f    |
|fg   |
*** 
 
### k 


|Sets |
|:----|
|f    |
|k    |
*** 
 
### mapGenPlant 


|Sets |
|:----|
|g    |
|k    |
|i    |
|o    |
*** 
 
### mapLocations 


|Sets |
|:----|
|i    |
|r    |
|e    |
|ild  |
*** 
 
### t 


|Sets |
|:----|
|m    |
|t    |
*** 
 
### mapReservoirs 


|Sets |
|:----|
|v    |
|i    |
|g    |
*** 
 
### Upg 


|Sets |
|:----|
|g    |
|gg   |
*** 
 
### movers 


|Sets |
|:----|
|k    |
*** 
 
### peaker 


|Sets |
|:----|
|k    |
*** 
 
### randomiseCapex 


|Sets |
|:----|
|k    |
*** 
 
### refurbish 


|Sets |
|:----|
|k    |
*** 
 
### regionCentroid 


|Sets |
|:----|
|i    |
|r    |
*** 
 
### renew 


|Sets |
|:----|
|k    |
*** 
 
### schedHydroUpg 


|Sets |
|:----|
|g    |
*** 
 
### techColor 


|Sets  |
|:-----|
|k     |
|red   |
|green |
|blue  |
*** 
 
### thermalTech 


|Sets |
|:----|
|k    |
*** 
 
### txUpgradeTransitions 


|Sets |
|:----|
|tupg |
|r    |
|rr   |
|ps   |
|pss  |
*** 
 
### wind 


|Sets |
|:----|
|k    |
*** 
 
### zoneCentroid 


|Sets |
|:----|
|i    |
|e    |
*** 
 

## Parameters

> *./Data/GEMdataInput/Parameters*

### definedOnFuel 


|Sets |
|:----|
|f    |


|Parameters        |
|:-----------------|
|i_maxNrgByFuel    |
|i_emissionFactors |
*** 
 
### definedOnGen 


|Sets |
|:----|
|g    |


|Parameters           |
|:--------------------|
|i_nameplate          |
|i_UnitLargestProp    |
|i_baseload           |
|i_offlineReserve     |
|i_FixComYr           |
|i_EarlyComYr         |
|i_ExogenousRetireYr  |
|i_refurbDecisionYear |
|i_fof                |
|i_heatrate           |
|i_PumpedHydroMonth   |
|i_PumpedHydroEffic   |
|i_minHydroCapFact    |
|i_maxHydroCapFact    |
|i_fixedOM            |
|i_varOM              |
|i_varFuelCosts       |
|i_fixedFuelCosts     |
|i_capitalCost        |
|i_connectionCost     |
|i_refurbCapitalCost  |
|i_hydroPeakingFactor |
*** 
 
### definedOnReserveClass 


|Sets |
|:----|
|rc   |


|Parameters      |
|:---------------|
|i_ReserveSwitch |
|i_ReserveAreas  |
|i_propWindCover |
*** 
 
### definedOnTech 


|Sets |
|:----|
|k    |


|Parameters           |
|:--------------------|
|i_plantLife          |
|i_refurbishmentLife  |
|i_retireOffsetYrs    |
|i_linearBuildMW      |
|i_linearBuildYr      |
|i_depRate            |
|i_peakContribution   |
|i_NWpeakContribution |
|i_capFacTech         |
*** 
 
### definedOnYear 


|Sets |
|:----|
|y    |


|Parameters             |
|:----------------------|
|i_inflation            |
|i_co2tax               |
|i_HVDClevy             |
|i_winterCapacityMargin |
|i_SIACrisk             |
|i_fkSI                 |
|i_fkNI                 |
|i_HVDClossesAtMaxXfer  |
|i_largestGenerator     |
|i_P200ratioNZ          |
|i_P200ratioNI          |
|i_renewNrgShare        |
|i_renewCapShare        |
|i_distdGenRenew        |
|i_distdGenFossil       |
*** 
 
### energyDemand 


|Sets |
|:----|
|r    |
|y    |
|t    |
|lb   |


|Parameters  |
|:-----------|
|i_NrgDemand |
*** 
 
### fuelPricesAndQuantities 


|Sets |
|:----|
|f    |
|y    |


|Parameters       |
|:----------------|
|i_fuelPrices     |
|i_fuelQuantities |
*** 
 
### halfHoursPerBlock 


|Sets |
|:----|
|m    |
|lb   |


|Parameters      |
|:---------------|
|i_HalfHrsPerBlk |
*** 
 
### historicalHydroOutput 


|Sets |
|:----|
|v    |
|hY   |
|m    |


|Parameters              |
|:-----------------------|
|i_historicalHydroOutput |
*** 
 
### shr 


|Sets |
|:----|
|o    |


|Parameters |
|:----------|
|i_HVDCshr  |
*** 
 
### inflexiblePlantFactor 


|Sets |
|:----|
|g    |
|lb   |


|Parameters              |
|:-----------------------|
|i_inflexiblePlantFactor |
*** 
 
### maxReservesTrnsfr 


|Sets |
|:----|
|r    |
|rr   |
|ps   |
|rc   |


|Parameters          |
|:-------------------|
|i_maxReservesTrnsfr |
*** 
 
### minUtilisation 


|Sets |
|:----|
|g    |
|y    |


|Parameters       |
|:----------------|
|i_minUtilisation |
*** 
 
### pltCapFact 


|Sets |
|:----|
|g    |
|m    |


|Parameters   |
|:------------|
|i_PltCapFact |
*** 
 
### reserveCapAndCost 


|Sets |
|:----|
|g    |
|rc   |


|Parameters          |
|:-------------------|
|i_plantReservesCap  |
|i_plantReservesCost |
*** 
 
### reservePenalty 


|Sets |
|:----|
|ild  |
|rc   |


|Parameters       |
|:----------------|
|i_ReservePenalty |
*** 
 
### reserveReq 


|Sets |
|:----|
|y    |
|ild  |
|rc   |


|Parameters     |
|:--------------|
|i_reserveReqMW |
*** 
 
### scalars 


|Sets  |
|:-----|
|name  |
|value |


|Parameters |
|:----------|
*** 
 
### substationCoordinates 


|Sets |
|:----|
|i    |
|geo  |


|Parameters          |
|:-------------------|
|i_substnCoordinates |
*** 
 
### techFOFmultiplier 


|Sets |
|:----|
|k    |
|lb   |


|Parameters      |
|:---------------|
|i_FOFmultiplier |
*** 
 
### transmissionCapacity 


|Sets |
|:----|
|r    |
|rr   |
|ps   |


|Parameters     |
|:--------------|
|i_txCapacity   |
|i_txCapacityPO |
|i_txResistance |
|i_txReactance  |
*** 
 
### transmissionUpgrades 


|Sets |
|:----|
|tupg |


|Parameters      |
|:---------------|
|i_txCapitalCost |
|i_txEarlyComYr  |
|i_txFixedComYr  |
*** 
 
### txGrpConstraintsLHS 


|Sets |
|:----|
|tgc  |
|p    |


|Parameters            |
|:---------------------|
|i_txGrpConstraintsLHS |
*** 
 
### txGrpConstraintsRHS 


|Sets |
|:----|
|tgc  |


|Parameters            |
|:---------------------|
|i_txGrpConstraintsRHS |
*** 
 
### zonalLocFacs 


|Sets |
|:----|
|e    |


|Parameters     |
|:--------------|
|i_zonalLocFacs |
*** 
 

## Setup variables

### Globals

> *./Data/Setup/globalVariables.csv*


```
FALSE # A tibble: 22 x 2
FALSE    variable       value                                          
FALSE    <chr>          <chr>                                          
FALSE  1 ProgPath       %system.fp%                                    
FALSE  2 DataPath       "%system.fp%..\\..\\Data\\GEMsolveInput\\GDX\\"
FALSE  3 OutPath        "%system.fp%..\\..\\Output\\"                  
FALSE  4 GEMinputGDX    GEMdata.gdx                                    
FALSE  5 GEMoverrideGDX <NA>                                           
FALSE  6 useOverrides   0                                              
FALSE  7 sprsGEMsolve   0                                              
FALSE  8 Mode           0                                              
FALSE  9 GRscheduleFile <NA>                                           
FALSE 10 GRscheduleRead 0                                              
FALSE # ... with 12 more rows
```

</br>

<hr>


### Scalars

> *./Data/Setup/scalarVariables.csv*


```
FALSE # A tibble: 29 x 2
FALSE    variable          value
FALSE    <chr>             <dbl>
FALSE  1 GRscheduleWrite    1   
FALSE  2 hydroOutputScalar  0.97
FALSE  3 WACCg              0.07
FALSE  4 WACCt              0.07
FALSE  5 discRateLow        0.04
FALSE  6 discRateMed        0.07
FALSE  7 discRateHigh       0.1 
FALSE  8 taxRate            0.28
FALSE  9 depType            1   
FALSE 10 txPlantLife       60   
FALSE # ... with 19 more rows
```


