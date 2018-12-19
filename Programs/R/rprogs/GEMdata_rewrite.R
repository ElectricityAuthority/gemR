###############################################
### Title: Intermediate data manipulations   ##
### Description: Read in, modify and         ##
### create new input symbols                 ##
### Date: 19 December 2018                   ##
###############################################

###############################################
### Load libraries ############################
###############################################
library(tidyverse)
library(gdxtools)

###############################################
### Read in all input symbols #################
###############################################
source("Programs/R/rprogs/readInputSymbols.R")

# globalVars <- read_csv("Data/Setup/globalVariables.csv")
# scalarVars <- read_csv("Data/Setup/scalarVariables.csv")
# 
# # Assign a couple of sets previously assigned in GEMdeclarations
# 
# ### ild - Islands
# ild <- data_frame(ild = c("ni", "si"))
# 
# ### lvl - Levels of non-free reserves
# lvl <- data_frame(lvl = paste0("lvl", 1:5))
# 
# ### aggR - Aggregate regional entities
# aggR <- data_frame(aggR = c("ni", "si", "nz"))
# 
# # Assign a couple of scalars previously assigned in GEMdeclarations
# 
# ### largestNIplant
# largestNIplant <- 385
# 
# ### largestSIplant
# largestSIplant <- 125
# 
# # Initialise set 'n' and record the number of loss tranches
# 
# ## n - piecewise linear vertices
# numVertices <- globalVars %>% 
#   filter(variable == "NumVertices") %>% 
#   .$value %>% 
#   as.numeric()
# 
# nSet <- data_frame(n = paste0("n", 1:numVertices))
# 
# ## numT - number of tranches
# numT <- data_frame(value = numVertices - 1)

# a) Time/date-related sets and parameters.

## Year variables

### firstYear - scalar for first calendar year
params$firstYear <- data_frame(value = globalVars$firstYear)

### firstYearNum - numeric version of firstYear
params$firstYearNum <- as.numeric(params$firstYear$value)

### lastYear - scalar for last calendar year
params$lastYear <- data_frame(value = globalVars$lastYear)

### lastYearNum - numeric version of firstYear
params$lastYearNum <- as.numeric(params$lastYear$value)

### y - Modelled calendar years
sets$y <- data_frame(y = params$firstYearNum:params$lastYearNum)

### firstYr(y) - subset of first calendar year
subsets$firstYr <- sets$y %>% 
  filter(y == min(y))

### allButFirstYr(y) - subset of all years except first calendar year
subsets$allButFirstYr <- sets$y %>% 
  filter(y != min(y)) 

### lastYr(y) - subset of last calendar year
subsets$lastYr <- sets$y %>% 
  filter(y == max(y))

### yearNum(y) - parameter of real number associated with each year
params$yearNum <- sets$y %>% 
  mutate(value = y)

### firstPeriod(t) - first time period
subsets$firstPeriod <- sets$t[1,]

## Hydro year variables

### hydroYearNum(hY) - parameter with real number for each hydro year
params$hydroYearNum <- sets$hY %>% 
  mutate(value = hY)

### lastHydroYear - scalar of last hydro year as integer
params$lastHydroYear <- sets$hY %>% 
  filter(hY == max(hY))

## Hours per block

### hoursPerBlock(t,lb) - hours per block defined on time period and load block
params$hoursPerBlock <- params$i_HalfHrsPerBlk %>% 
  inner_join(subsets$mapm_t, by = "m") %>% 
  group_by(t, lb) %>% 
  summarise(
    value = sum(value * 0.5)
  ) %>% 
  ungroup()

# b) Various mappings, subsets and counts

### mapi_r(i,r) - map regions to substations
subsets$mapi_r <- subsets$mapLocations %>% 
  select(i, r) %>% 
  distinct()

### mapi_e(i,e) - map zones to substations
subsets$mapi_e <- subsets$mapLocations %>% 
  select(i, e) %>% 
  distinct()

### mapild_r(ild,r) - map regions to islands 
subsets$mapild_r <- subsets$mapLocations %>% 
  select(ild, r) %>% 
  distinct()

### mapAggR_r(aggR,r) - map regions to aggregated regional entities
subsets$mapAggR_r <- sets$aggR %>% 
  filter(aggR == "nz") %>% 
  crossing(sets$r) %>% 
  union_all(
    subsets$mapild_r %>% 
      rename(
        aggR = ild
      )
  )

## Figure out if there are just 2 regions and whether their names are identical to the names of the 2 islands.

### isIldEqReg(ild,r) - checking if there are 2 regions only
#### Note: I haven't coded this yet - is it needed in GAMS? ##### 

## Generation plant mappings

### mapg_k(g,k) - map tech type to generation plant
subsets$mapg_k <- subsets$mapGenPlant %>% 
  select(g, k) %>% 
  distinct()

### mapg_o(g,o) - map plant owners to generation plant
subsets$mapg_o <- subsets$mapGenPlant %>% 
  select(g, o) %>% 
  distinct()

### mapg_i(g,i) - map substations to generation plant
subsets$mapg_i <- subsets$mapGenPlant %>% 
  select(g, i) %>% 
  distinct()

### mapg_f(g,f) - map fuel to generation plant
subsets$mapg_f <- subsets$mapg_k %>% 
  inner_join(subsets$mapf_k, by = "k") %>% 
  select(g, f)

### mapg_r(g,r) - map region to generation plant
subsets$mapg_r <- subsets$mapg_i %>% 
  inner_join(subsets$mapi_r, by = "i") %>% 
  select(g, r)

### mapg_e(g,e) - map zone to generation plant
subsets$mapg_e <- subsets$mapg_i %>% 
  inner_join(subsets$mapi_e, by = "i") %>%
  select(g, e)

### mapg_ild(g,ild) - map island to generation plant
subsets$mapg_ild <- subsets$mapg_r %>% 
  inner_join(subsets$mapild_r, by = "r") %>% 
  select(g, ild)

## Reservoir mappings

### mapv_g(v,g) - map generation plant to reservoirs
subsets$mapv_g <- subsets$mapReservoirs %>% 
  select(v, g) %>% 
  distinct()

## Fuel mappings
# thermalTech <- read_csv("Data/Subsets/thermalTech.csv")

### thermalFuel(f) - thermal fuels
subsets$thermalFuel <- subsets$thermalTech %>% 
  inner_join(subsets$mapf_k, by = "k") %>% 
  distinct(f)

## Count number of regions

### numReg - scalar for number of regions
params$numReg <- data_frame(value = nrow(sets$r))

## Identify generation plant types

### schedHydroPlant(g) - scheduled hydro generation plant
subsets$schedHydroPlant <- subsets$hydroSched %>% 
  inner_join(subsets$mapg_k, by = "k") %>% 
  distinct(g)

### pumpedHydroPlant(g) - pumped hydro generation plant
subsets$pumpedHydroPlant <- subsets$hydroPumped %>% 
  inner_join(subsets$mapg_k, by = "k") %>% 
  distinct(g)

## Figure out which load blocks are immediately to the right of any given block

### rightAdjacentBlocks(lb,lbb) - assumes set list for loadblocks is in correct order
subsets$rightAdjacentBlocks <- sets$lb %>%
  mutate(lbb = lead(lb)) %>% 
  filter(!is.na(lbb))

# c) Financial parameters

## Load WACC variables from scalar dataset

## Create (ordered) mapping of "y" to "t"
subsets$mapy_t <- data.frame(
  y = rep(sets$y$y, each = nrow(sets$t))
  , t = rep(sets$t$t, nrow(sets$y)),
  stringsAsFactors = FALSE
  ) %>% 
  as_tibble()

## Count of unique periods
params$countT <- data_frame(value = length(unique(sets$t$t)))

## Numeric order of periods (based on row number)
params$orderT <- sets$t %>% 
  mutate(orderT = row_number())

### discountRates(d)
#### Note: not created for now as only used for reporting

### PVfacG(y,t) - generation investor's present value factor by period
params$PVfacG <- subsets$mapy_t %>% 
  inner_join(params$orderT, by = "t") %>% 
  mutate(
    value = 1 / ( 1 + scalarVars$WACCg ) ^ ( ( y - params$firstYearNum) + ( orderT * 2 - 1 ) / ( 2 * params$countT$value ) )
  ) %>% 
  select(-orderT)

### PVfacT(y,t) - transmission investor's present value factor by period
params$PVfacT <- subsets$mapy_t %>% 
  inner_join(params$orderT, by = "t") %>% 
  mutate(
    value = 1 / ( 1 + scalarVars$WACCt ) ^ ( ( y - params$firstYearNum) + ( orderT * 2 - 1 ) / ( 2 * params$countT$value ) )
  ) %>% 
  select(-orderT)

### PVfacsM, PVfacsEY, PVfacs
#### Note: all three of the above are left out for now. Not required by GEMsolve. 

### capexLife(k,ct) - plant life by technology and capex type

#### Read in component parameters

#### capexLife(k,ct)
params$capexLife <- params$i_plantLife %>% 
  mutate(ct = "genplt") %>% 
  union_all(
    params$i_refurbishmentLife %>% 
    mutate(ct = "refplt")
  ) %>% 
  # Filter out NAs
  filter(!is.na(value)) %>% 
  # Reorder columns
  select(k, ct, value)

### annuityFacN(y,k,ct) - nominal annuity factor by tech, year and capex type 

#### Get distinct (y,k,ct) from "capexLife" and "y"
subsets$mapy_k_ct <- params$capexLife %>% 
  select(k, ct) %>% 
  distinct() %>% 
  crossing(sets$y)

if(scalarVars$WACCg != 0){
  
  params$annuityFacN <- subsets$mapy_k_ct %>% 
    inner_join(
      params$i_inflation %>% 
        rename(i_inflation = value)
      , by = "y"
    ) %>% 
    inner_join(
      params$capexLife %>% 
        rename(capexLife = value)
      , by = c("k", "ct")
    ) %>% 
    mutate(
      value = ( 1 - ( 1 + scalarVars$WACCg + i_inflation ) ^ (-capexLife) ) / scalarVars$WACCg
    ) %>% 
    select(y, k, ct, value)
  
}

### annuityFacR(k,ct) - real annuity factor by tech and capex type 
if(scalarVars$WACCg != 0){
  
  params$annuityFacR <- subsets$mapy_k_ct %>% 
    select(k, ct) %>% 
    distinct() %>%  
    inner_join(
      params$capexLife %>% 
        rename(capexLife = value)
      , by = c("k", "ct")
    ) %>% 
    mutate(
      value = ( 1 - ( 1 + scalarVars$WACCg ) ^ (-capexLife) ) / scalarVars$WACCg
    ) %>% 
    select(k, ct, value)
  
}

### txAnnuityFacN(y) - nominal transmission annuity factor by year
params$txAnnuityFacN <- sets$y %>% 
  inner_join(
    params$i_inflation %>% 
      rename(i_inflation = value)
    , by = "y"
  ) %>% 
  mutate(
    value = ( 1 - ( 1 + scalarVars$WACCt + i_inflation ) ^ (-scalarVars$txPlantLife) ) / scalarVars$WACCt
  ) %>% 
  select(y, value)

### txAnnuityFacR - real transmission annuity factor (scalar)
params$txAnnuityFacR <- data_frame(
  value = ( 1 - ( 1 + scalarVars$WACCt ) ** (-scalarVars$txPlantLife) ) / scalarVars$WACCt
)

## Recovery factors

### Conditionally assign recovery factors (based on depType)

# Generation recovery factors
params$generationRecFac <- params$annuityFacN %>% 
  rename(annuityFacN = value) %>% 
  inner_join(
    params$capexLife %>% 
      rename(capexLife = value)
    , by = c("k", "ct")
  ) %>% 
  inner_join(
    params$annuityFacR %>% 
      rename(annuityFacR = value)
    , by = c("k", "ct")
  ) %>% 
  inner_join(
    params$i_depRate %>% 
      rename(i_depRate = value)
    , by = "k"
  ) %>% 
  inner_join(
    params$i_inflation %>% 
      rename(i_inflation = value)
    , by = "y"
  ) %>% 
  mutate(
    
    capRecFac = case_when(
      scalarVars$depType == 0 ~ ( 1 - scalarVars$taxRate * annuityFacN / capexLife ) / annuityFacR
      , TRUE ~ ( 1 - i_depRate * scalarVars$taxRate / (scalarVars$WACCg + i_inflation + i_depRate )) / annuityFacR
    )
    
    , depTCrecFac = case_when(
      scalarVars$depType == 0 ~ ( scalarVars$taxRate * annuityFacN / capexLife ) / annuityFacR
      , TRUE ~ ( i_depRate * scalarVars$taxRate / (scalarVars$WACCg + i_inflation + i_depRate) ) / annuityFacR 
    )
    
  )

# Transmission recovery factors
params$transmissionRecFac <- params$txAnnuityFacN %>% 
  rename(txAnnuityFacN = value) %>%
  inner_join(
    params$i_inflation %>% 
      rename(i_inflation = value)
    , by = "y"
  ) %>%  
  mutate(
    
    txAnnuityFacR = params$txAnnuityFacR$value
    
    , txCapRecFac = case_when(
      scalarVars$depType == 0 ~ ( 1 - scalarVars$taxRate * txAnnuityFacN / scalarVars$txPlantLife ) / txAnnuityFacR 
      , TRUE ~ ( 1 - scalarVars$txDepRate * scalarVars$taxRate / (scalarVars$WACCt + i_inflation + scalarVars$txDepRate) ) / txAnnuityFacR
    )
    , txDepTCrecFac = case_when(
      scalarVars$depType == 0 ~ ( scalarVars$taxRate * txAnnuityFacN / scalarVars$txPlantLife ) / txAnnuityFacR
      , TRUE ~ ( scalarVars$txDepRate * scalarVars$taxRate / (scalarVars$WACCt + i_inflation + scalarVars$txDepRate) ) / txAnnuityFacR
    ) 
  )

### capRecFac(y,k,ct) - capital recovery factor by technology
params$capRecFac <- params$generationRecFac %>% 
  select(y, k, ct, capRecFac) %>% 
  rename(value = capRecFac)

### depTCrecFac(y,k,ct) - depreciation tax credit portion of capRecFac
params$depTCrecFac <- params$generationRecFac %>% 
  select(y, k, ct, depTCrecFac) %>% 
  rename(value = depTCrecFac)

### txCapRecFac(y) - capital recovery factor by technology for transmission
params$txCapRecFac <- params$transmissionRecFac %>% 
  select(y, txCapRecFac) %>% 
  rename(value = txCapRecFac)

### txDepTCrecFac(y) - depreciation tax credit portion of txCapRecFac
params$txDepTCrecFac <- params$transmissionRecFac %>% 
  select(y, txDepTCrecFac) %>% 
  rename(value = txDepTCrecFac)

# d) Generation data
# Derive various generating plant subsets

## i) Set the V2G plant capacities to zero when the V2GtechnologyOn flag is zero (this takes those plant out of the pick list).
V2GtechnologyOn <- scalarVars %>% 
  filter(variable == "V2GtechnologyOn") %>% 
  .$value

if(!V2GtechnologyOn){
  
  i_nameplate <- i_nameplate %>% 
    inner_join(mapg_k, by = "g") %>% 
    mutate(
      i_nameplate = ifelse(k == "V2G", 0, i_nameplate)
    ) %>% 
    select(g, i_nameplate)
  
}

## ii) Existing plant - remove any plant where i_nameplate(g) = 0 from exist(g)

### Load exist(g) subset
exist <- read_csv("Data/Subsets/exist.csv")

### Modify exist(g) to remove those with 0 nameplate
exist <- exist %>% 
  inner_join(
    i_nameplate %>% 
      filter(i_nameplate != 0)
    , by = "g"
  ) %>% 
  select(g)

## iii) A plant is not an existing plant if it hasn't been defined to be existing - 
##   remove any plant where i_nameplate(g) = 0 from noExist(g).

### Read in generation plant set
g <- read_csv("Data/Sets/generationPlant.csv")

### noExist(g)
noExist <- g %>%
  anti_join(exist, by = "g") %>% 
  inner_join(i_nameplate, by = "g") %>% 
  filter(i_nameplate != 0) %>% 
  select(g)

## iv) Define plant that are never able to be built. A plant is never to be built if it already exists, 
##   if (i_fixComYr or i_EarlyComYr) > lastYear, or if i_nameplate <= 0
i_FixComYr <- definedOnGen %>% 
  select(g, i_FixComYr) %>% 
  filter(!is.na(i_FixComYr))

i_EarlyComYr <- definedOnGen %>% 
  select(g, i_EarlyComYr) %>% 
  filter(!is.na(i_EarlyComYr))

### neverBuild(g)
neverBuild <- noExist %>% 
  left_join(i_FixComYr, by = "g") %>% 
  left_join(i_EarlyComYr, by = "g") %>% 
  filter(
    i_FixComYr > lastYear$value | i_EarlyComYr > lastYear$value 
  ) %>% 
  select(g) %>% 
  union_all(
    i_nameplate %>% 
      filter(i_nameplate == 0 | is.na(i_nameplate)) %>% 
      select(g)
  )

## v) Define committed plant. To be a committed plant, the plant must not exist, it must not be in the neverBuild set, and
##   it must have a fixed commissioning year that is greater than or equal to the first modelled year.

### commit(g)
commit <- noExist %>% 
  anti_join(neverBuild, by = "g") %>% 
  inner_join(i_FixComYr, by = "g") %>% 
  filter(i_FixComYr >= firstYear) %>% 
  select(g)

## vi) Define new plant. A plant is (potentially) new if it is not existing, not committed, and not a member of the neverBuild set.

### new(g) - potential generation plant that are neither existing nor committed
newPlant <- noExist %>% 
  anti_join(neverBuild, by = "g") %>% 
  anti_join(commit, by = "g")

## vii) Define the years in which it is valid for a generating plant to be built. The plant must either be committed or (potentially)
##   new, and the plant can't be a member of the neverBuild set

### validYrBuild(g,y)
validYrBuild <- newPlant %>% 
  crossing(y) %>% 
  inner_join(i_EarlyComYr, by = "g") %>% 
  filter(y >= i_EarlyComYr) %>% 
  union_all(
    commit %>% 
      inner_join(i_FixComYr, by = "g") %>% 
      rename(y = i_FixComYr)
  ) %>% 
  select(g, y)

## viii) Identify the plant that may be built, i.e. it doesn't already exist or it is not otherwise prevented from being built

### possibleToBuild(g) - generating plant that may possibly be built in any valid build year
possibleToBuild <- validYrBuild %>% 
  distinct(g)

## ix) Identify generation plant that can be linearly or incrementally built

### Read in linearBuildTech subset
linearBuildTech <- read_csv("Data/Subsets/linearBuildTech.csv")

### Get linearBuildMW and linearBuildYr
linearBuildInfo <- definedOnTech %>% 
  select(k, i_linearBuildMW, i_linearBuildYr) %>% 
  filter(!is.na(i_linearBuildMW) | !is.na(i_linearBuildYr))

### linearPlantBuild(g)
linearPlantBuild <- noExist %>% 
  inner_join(mapg_k, by = "g") %>%
  anti_join(i_FixComYr, by = "g") %>% 
  inner_join(linearBuildTech, by = "k") %>% 
  left_join(linearBuildInfo, by = "k") %>% 
  inner_join(i_nameplate, by = "g") %>% 
  inner_join(i_EarlyComYr, by = "g") %>% 
  filter(
    i_nameplate >= i_linearBuildMW | i_EarlyComYr >= i_linearBuildYr
  ) %>% 
  select(g)

## x) Identify generation plant that must be integer build (must be integer if not linear)

### integerPlantBuild(g)
integerPlantBuild <- noExist %>% 
  anti_join(linearPlantBuild, by = "g") %>% 
  anti_join(neverBuild, by = "g")

## xi) Identify exceptions to the technology-determined list of plant movers, i.e. if user fixes build year 
##   to a legitimate value, then don't allow the plant to be a mover.

### Get movers(k) subset
movers <- read_csv("Data/Subsets/movers.csv")

### moverExceptions(g)
moverExceptions <- noExist %>% 
  inner_join(mapg_k, by = "g") %>% 
  inner_join(movers, by = "k") %>% 
  left_join(i_FixComYr, by = "g") %>% 
  filter(
    i_FixComYr >= firstYear | i_FixComYr <= lastYear
  )

## xii) Define the years in which it is valid for a generating plant to operate. The plant must exist; 
##   if plant is committed, it is valid to operate it in any year beginning with the year in which it 
##   is commissioned; if plant is new, it is valid to operate it in any year beginning with the earliest 
##   year in which it may be commissioned; it is not valid to operate any plant that has come to the end 
##   of its refurbished life (i.e. can't repeatedly refurbish); it is not valid to operate any plant that 
##   has been exogenously retired, or decommissioned; and it is not valid to operate any plant that is never 
##   able to be built.

### Get i_refurbDecisionYear parameter
i_refurbDecisionYear <- definedOnGen %>% 
  select(g, i_refurbDecisionYear) %>% 
  filter(!is.na(i_refurbDecisionYear))

### Get i_ExogenousRetireYr parameter
i_ExogenousRetireYr <- definedOnGen %>% 
  select(g, i_ExogenousRetireYr) %>% 
  filter(!is.na(i_ExogenousRetireYr))

### validYrOperate(g,y)
validYrOperate <- exist %>% 
  crossing(y) %>% 
  union_all(
    
    commit %>% 
      inner_join(i_FixComYr, by = "g") %>% 
      crossing(y) %>% 
      filter(y >= i_FixComYr) %>% 
      select(g, y)
    
  ) %>% 
  union_all(
    
    newPlant %>% 
      inner_join(i_EarlyComYr, by = "g") %>% 
      crossing(y) %>% 
      filter(y >= i_EarlyComYr) %>% 
      select(g, y)
    
  ) %>% 
  left_join(
    
    i_refurbDecisionYear %>% 
      inner_join(
        mapg_k
        , by ="g"
      ) %>% 
      inner_join(
        i_refurbishmentLife
        , by = "k"
      ) %>% 
      mutate(
        refurbishmentUpper = i_refurbDecisionYear + value
      ) %>% 
      select(g, refurbishmentUpper)
    
    , by = "g"
  ) %>% 
  # Filter out plant/year combinations where there is an upper bound on refurbishment
  filter(is.na(refurbishmentUpper) | y <= refurbishmentUpper) %>% 
  left_join(
    
    i_ExogenousRetireYr
    , by = "g"
    
  ) %>% 
  # Filter out plant/year combinations where the plant has been retired
  filter(is.na(i_ExogenousRetireYr) | y < i_ExogenousRetireYr) %>% 
  select(g, y) %>% 
  arrange(g, y)

## xiii) North and South Island plant

### nigen(g)
nigen <- mapg_ild %>% 
  filter(ild == "ni") %>% 
  select(g) %>% 
  anti_join(neverBuild, by = "g")

### sigen(g)
sigen <- mapg_ild %>% 
  filter(ild == "si") %>% 
  select(g) %>% 
  anti_join(neverBuild, by = "g")

## Define capacity of existing plant in first modelled year. Be aware that if capacity is committed in 
##   the first modelled year, the plant will be in the commit(g) and noExist(g) sets

### initialCapacity(g) - capacity of existing generation plant 
initialCapacity <- i_nameplate %>% 
  inner_join(exist, by = "g")

## Define exogenously retired MW by plant and year

### exogMWretired(g,y)
exogMWretired <- i_ExogenousRetireYr %>% 
  rename(y = i_ExogenousRetireYr) %>% 
  inner_join(y, by = "y") %>% 
  inner_join(i_nameplate, by = "g") %>% 
  rename(value = i_nameplate)

## Identify all generation plant that may be endogenously retired

### possibleToEndogRetire(g)
possibleToEndogRetire <- i_refurbDecisionYear %>% 
  distinct(g)

## Identify all generation plant that may be retired (endogenously or exogenously)

### possibleToRetire(g)
possibleToRetire  <- possibleToEndogRetire %>% 
  union_all(
    i_ExogenousRetireYr %>% 
      select(g)
  ) %>% 
  distinct(g)

## Define contribution to peak capacity by plant

### Get i_peakContribution
i_peakContribution <- definedOnTech %>% 
  select(k, i_peakContribution)

### Get i_NWpeakContribution
i_NWpeakContribution <- definedOnTech %>% 
  select(k, i_NWpeakContribution)

### peakConPlant(g,y)
peakConPlant <- mapg_k %>% 
  crossing(y) %>% 
  inner_join(
    i_peakContribution
    , by = "k"
  ) %>% 
  rename(
    value = i_peakContribution
  ) %>% 
  select(g, y, value)

### NWpeakConPlant(g,y)
NWpeakConPlant <- mapg_k %>% 
  crossing(y) %>% 
  inner_join(
    i_NWpeakContribution
    , by = "k"
  ) %>% 
  rename(
    value = i_NWpeakContribution
  ) %>% 
  select(g, y, value) %>% 
  filter(!is.na(value))

## Initialise the FOF multiplier - compute a weighted average using annual hours per load block as the weights

### Get i_FOFmultiplier
i_FOFmultiplier <- read_csv("Data/Parameters/techFOFmultiplier.csv")

### WtdAvgFOFmultiplier(k,lb)
WtdAvgFOFmultiplier <- i_FOFmultiplier %>% 
  inner_join(
    hoursPerBlock %>% 
      rename(hoursPerBlock = value)
    , by = "lb"
  ) %>%
  group_by(k, lb) %>% 
  summarise(
    value = sum(hoursPerBlock * i_FOFmultiplier / sum(hoursPerBlock))
  ) %>% 
  ungroup()

## Derive the minimum and maximum capacity factors for each plant and period.

### Get i_PltCapFact
i_PltCapFact <- read_csv("Data/Parameters/pltCapFact.csv")

### Get i_maxHydroCapFact
i_maxHydroCapFact <- definedOnGen %>% 
  select(g, i_maxHydroCapFact) %>% 
  filter(!is.na(i_maxHydroCapFact))

### Get i_fof(g)
i_fof <- definedOnGen %>% 
  select(g, i_fof) %>% 
  mutate(i_fof = ifelse(is.na(i_fof), 0, i_fof))

### maxCapFactPlant(g,t,lb)
maxCapFactPlant <- i_PltCapFact %>% 
  inner_join(
    mapm_t
    , by = "m"
  ) %>% 
  crossing(loadBlocks) %>% 
  group_by(g, t, lb) %>%
  # First average over time 
  summarise(
    value = sum(i_PltCapFact) / n()
  ) %>% 
  # Then, set all scheduable hydro max capacity factors to zero
  inner_join(
    mapg_k
    , by = "g"
  ) %>% 
  mutate(
    value = ifelse(k %in% hydroSched$k, 0, value)
  ) %>% 
  # Overwrite max capacity factor for hydro with user-defined, non-zero i_maxHydroCapFact(g) values
  left_join(
    i_maxHydroCapFact
    , by = "g"
  ) %>% 
  mutate(
    value = ifelse(is.na(i_maxHydroCapFact), value, i_maxHydroCapFact)
  ) %>% 
  # Adjust all max capacity factors for forced outage factor
  inner_join(
    i_fof
    , by = "g"
  ) %>% 
  left_join(
    WtdAvgFOFmultiplier %>% 
      rename(WtdAvgFOFmultiplier = value)
    , by = c("k", "lb")
  ) %>% 
  mutate(
    # Set NAs to 0
    WtdAvgFOFmultiplier = ifelse(is.na(WtdAvgFOFmultiplier), 0, WtdAvgFOFmultiplier)
    , i_fof = ifelse(is.na(i_fof), 0, i_fof)
    
    # Calculate final value
    , value = 1 - i_fof * WtdAvgFOFmultiplier
  ) %>% 
  select(g, t, lb, value) %>% 
  # Filter to those with a maximum capacity factor greater than 0
  filter(
    value > 0
  ) %>% 
  ungroup()

## Min capacity factor (only meaningfully defined for hydro units)

### Get i_minHydroCapFact
i_minHydroCapFact <- definedOnGen %>% 
  select(g, i_minHydroCapFact) %>% 
  filter(!is.na(i_minHydroCapFact))

### minCapFactPlant(g,y,t)
minCapFactPlant <- schedHydroPlant %>% 
  inner_join(
    i_minHydroCapFact %>% 
      rename(
        value = i_minHydroCapFact
      )
    , by = "g"
  ) %>% 
  # Min capacity factor also 'non-meaningfully' defined to a low non-zero value for wind plant
  union_all(
    mapg_k %>% 
      filter(k == "Wind") %>% 
      mutate(
        value = 0.001
      ) %>% 
      select(g, value)
  ) %>% 
  crossing(y) %>% 
  crossing(timePeriods) %>% 
  select(g, y, t, value)

## Identify all the generation plant that may possibly be refurbished or endogenously retired and 
##   the years in which that retirement may/will occur

### Get "refurbish" subset
refurbish <- read_csv("Data/Subsets/refurbish.csv")

### Get "endogRetire" subset
endogRetire <- read_csv("Data/Subsets/endogRetire.csv")

### Get i_refurbCapitalCost
i_refurbCapitalCost <- definedOnGen %>% 
  select(g, i_refurbCapitalCost) %>% 
  filter(!is.na(i_refurbCapitalCost))

### Get i_retireOffsetYrs
i_retireOffsetYrs <- definedOnTech %>% 
  select(k, i_retireOffsetYrs) %>% 
  filter(!is.na(i_retireOffsetYrs))

### Get "noRetire" scalar
noRetire <- scalarVars %>% 
  filter(variable == "noRetire") %>% 
  .$value

### possibleToRefurbish(g)
possibleToRefurbish <- exist %>% 
  inner_join(
    mapg_k
    , by = "g"
  ) %>% 
  inner_join(
    refurbish
    , by = "k"
  ) %>% 
  inner_join(
    i_refurbDecisionYear
    , by = "g"
  ) %>% 
  inner_join(
    i_refurbCapitalCost
    , by = "g"
  ) %>% 
  select(g)

endogenousRetireInfo <- possibleToRefurbish %>% 
  inner_join(
    mapg_k
    , by = "g"
  ) %>% 
  mutate(
    # Flag if in endogRetire subset
    endogRetireFlag = ifelse(k %in% endogRetire$k, 1, 0)
  ) %>% 
  crossing(y) %>% 
  inner_join(
    i_refurbDecisionYear
    , by = "g" 
  ) %>% 
  inner_join(
    i_retireOffsetYrs
    , by = "k"
  )

### endogenousRetireDecisnYrs(g,y)
endogenousRetireDecisnYrs <- endogenousRetireInfo %>% 
  filter(
    case_when(
      endogRetireFlag == 1 ~ y >= (firstYearNum + noRetire) & y <= i_refurbDecisionYear 
      , TRUE ~ y >= (firstYearNum + noRetire) & y == i_refurbDecisionYear 
    )
  ) %>% 
  select(g, y)

### endogenousRetireYrs(g,y)
endogenousRetireYrs <- endogenousRetireInfo %>% 
  filter(
    case_when(
      endogRetireFlag == 1 ~ y >= (firstYearNum + noRetire + i_retireOffsetYrs) & y <= i_refurbDecisionYear + i_retireOffsetYrs
      , TRUE ~ y >= (firstYearNum + noRetire + i_retireOffsetYrs) & y == i_refurbDecisionYear + i_retireOffsetYrs
    )
  ) %>% 
  select(g, y)

## Compute the years a plant must keep going for after the decision to endogenously retire it has been made

### continueAftaEndogRetire(g)
continueAftaEndogRetire <- possibleToEndogRetire %>% 
  inner_join(
    mapg_k
    , by = "g"
  ) %>% 
  inner_join(
    refurbish
    , by = "k"
  ) %>% 
  inner_join(
    i_retireOffsetYrs
    , by = "k"
  ) %>% 
  rename(
    value = i_retireOffsetYrs
  ) %>% 
  select(g, value)

## Define capital costs for new generation plant:
##   Capital costs are first calculated as if capex is lumpy. After any adjustments, they are then converted
##   to a levelised or annualised basis (i.e. see capCharge).

## First, transfer i_capitalCost to capexPlant and i_refurbCapitalCost to refurbCapexPlant, and convert both to $/MW

### Get i_capitalCost
i_capitalCost <- definedOnGen %>% 
  select(g, i_capitalCost) %>% 
  filter(!is.na(i_capitalCost))

### Get i_connectionCost
i_connectionCost <- definedOnGen %>% 
  select(g, i_connectionCost) %>% 
  filter(!is.na(i_connectionCost))

### Get "randomiseCapex" subset
randomiseCapex <- read_csv("Data/Subsets/randomiseCapex.csv")

### Get randomCapexCostAdjuster scalar
randomCapexCostAdjuster <- scalarVars %>% 
  filter(variable == "randomCapexCostAdjuster") %>% 
  .$value

### vbleConCostPlant(g)
vbleConCostPlant <- i_connectionCost %>% 
  inner_join(
    i_nameplate
    , by = "g"
  ) %>% 
  mutate(
    value = 1e6 * i_connectionCost / i_nameplate
  ) %>% 
  select(g, value)

### capexPlant(g)
capexPlant <- i_capitalCost %>% 
  mutate(
    value = i_capitalCost * 1e3
  ) %>% 
  select(g, value) %>% 
  # Randomly adjust capexPlant to create mathematically different costs - this helps the solver but makes no
  #   appreciable economic difference provided randomCapexCostAdjuster is small
  inner_join(
    mapg_k
    , by = "g"
  ) %>% 
  mutate(
    value = case_when(
      k %in% randomiseCapex$k & g %in% noExist$g ~ runif(
        1
        , min = value - (value * randomCapexCostAdjuster)
        , max = value + (value * randomCapexCostAdjuster)
      )
      , TRUE ~ value
    )
  ) %>% 
  # Add on the 'variablised' connection costs to the adjusted plant capital costs - continue to yield NZ$/MW
  left_join(
    vbleConCostPlant %>% 
      rename(
        vbleConCostPlant = value
      )
    , by = "g"
  ) %>% 
  mutate(
    value = ifelse(!is.na(vbleConCostPlant), value + vbleConCostPlant, value)
  ) %>% 
  select(g, value)

### refurbCapexPlant(g)
refurbCapexPlant <- i_refurbCapitalCost %>% 
  mutate(
    value = i_refurbCapitalCost * 1e3
  ) %>% 
  select(g, value) %>% 
  # Zero out any refubishment capex costs if the plant is not actually a candidate for refurbishment
  mutate(
    value = ifelse(g %in% possibleToRefurbish$g, value, 0)
  )

## Finally, convert lumpy capital costs to levelised capital charge (units are now NZ$/MW/yr)

### capCharge(g,y)
capCharge <- capexPlant %>% 
  inner_join(
    mapg_k
    , by = "g"
  ) %>% 
  inner_join(
    capRecFac %>% 
      filter(ct == "genplt") %>% 
      rename(
        capRecFac = value
      )
    , by = "k"
  ) %>% 
  mutate(
    value = value * capRecFac
  ) %>% 
  select(g, y, value)

## refurbCapCharge(g,y)
refurbCapCharge <- refurbCapexPlant %>% 
  inner_join(
    mapg_k
    , by = "g"
  ) %>% 
  inner_join(
    capRecFac %>% 
      filter(ct == "refplt") %>% 
      rename(
        capRecFac = value
      )
    , by = "k"
  ) %>% 
  mutate(
    value = value * capRecFac
  ) %>% 
  inner_join(
    i_refurbDecisionYear
    , by = "g"
  ) %>% 
  inner_join(
    i_refurbishmentLife %>% 
      rename(
        i_refurbishmentLife = value
      )
    , by = c("k", "ct")
  ) %>% 
  filter(
    y >= i_refurbDecisionYear
    , y <= (i_refurbDecisionYear + i_refurbishmentLife)
  ) %>% 
  select(g, y, value)

## Calculate reserve capability per generating plant

### Get i_plantReservesCap
i_plantReservesCap <- read_csv("Data/Parameters/reserveCapAndCost.csv") %>% 
  select(g, rc, i_plantReservesCap)

### reservesCapability(g,rc)
reservesCapability <- i_plantReservesCap %>% 
  inner_join(
    i_nameplate
    , by = "g"
  ) %>% 
  mutate(
    value = i_plantReservesCap * i_nameplate
  ) %>% 
  select(g, rc, value)

## Add any fixed costs associated with fuel production and delivery to the fixed OM costs by plant

### Get fuel costs 
fuelCosts <- definedOnGen %>% 
  select(g, i_fixedOM, i_fixedFuelCosts, i_heatrate)

### i_fixedOM(g)
i_fixedOM <- fuelCosts %>% 
  mutate(
    # Replace NAs with 0s
    i_fixedOM = ifelse(is.na(i_fixedOM), 0, i_fixedOM)
    , i_fixedFuelCosts = ifelse(is.na(i_fixedFuelCosts), 0, i_fixedFuelCosts)
    , i_heatrate = ifelse(is.na(i_heatrate), 0, i_heatrate)
  ) %>%  
  mutate(
    value = i_fixedOM + i_fixedFuelCosts * i_heatrate / 1000 * 8.76
  ) %>% 
  filter(value > 0) %>% 
  select(g, value)

## Compute the marginal loss location factors by plant (reciprocal of the zonally-based location factors)

### Get i_zonalLocFacs
i_zonalLocFacs <- read_csv("Data/Parameters/zonalLocFacs.csv")

### locationFactor(g)
locationFactor <- mapg_e %>% 
  inner_join(
    i_zonalLocFacs
    , by = "e"         
  ) %>% 
  mutate(
    value = 1 / i_zonalLocFacs
    # Set equal to 1 if it should be zero (it shouldn't be)
    , value = ifelse(value == 0, 1, value)
  ) %>% 
  select(g, value)

### If there are more than 2 regions, set location factors to 1
if(numReg > 2){
  
  locationFactor <- locationFactor %>% 
    mutate(
      value = 1
    )  
  
}

## Collect up the various cost factors into the so-called ensemble factor. 

### Get i_hydroPeakingFactor
i_hydroPeakingFactor <- definedOnGen %>% 
  select(g, i_hydroPeakingFactor)

### ensembleFactor(g)
ensembleFactor <- i_hydroPeakingFactor %>% 
  inner_join(
    locationFactor
    , by = "g"
  ) %>% 
  mutate(
    value = ifelse(is.na(i_hydroPeakingFactor), value, value * (1 / i_hydroPeakingFactor))
  ) %>% 
  select(g, value)

## e) Transmission data

## Let the last region declared be the slack bus (note that set r may not be ordered if users don't maintain unique set elements)

### slackBus(r)
slackBus <- regions %>% 
  tail(1)

## Define the lower triangular part of region-region matrix, i.e. ord(r) > ord(rr)

### regLower(r,rr)
regLower <- regions %>% 
  mutate(
    rr = lag(r)
  ) %>% 
  filter(!is.na(rr))

## Define regions at each end of NI-SI HVDC link

### Get Benmore and Haywards subsets
Benmore <- read_csv("Data/Subsets/Benmore.csv")
Haywards <- read_csv("Data/Subsets/Haywards.csv")

BenmoreReg <- Benmore %>% 
  inner_join(
    mapi_r
    , by = "i"
  ) %>% 
  select(r)

HaywardsReg <- Haywards %>% 
  inner_join(
    mapi_r
    , by = "i"
  ) %>% 
  select(r)

### nwd(r,rr)
nwd <- BenmoreReg %>% 
  add_column(rr = HaywardsReg$r)

### swd(r,rr)
swd <- HaywardsReg %>% 
  add_column(rr = BenmoreReg$r)

## Define interisland pairings

### interIsland(ild,ild1)
interIsland <- ild %>% 
  crossing(
    ild %>%
      rename(ild1 = ild)
  ) %>% 
  filter(
    ild != ild1
  )

### interIslandRegions(r,rr)
interIslandRegions <- nwd %>% 
  union_all(
    swd
  )

## Make sure i_txCapacityPO is not specified for anything but the current HVDC link

### Get transmissionCapacity parameters
transmissionCapacity <- read_csv("Data/Parameters/transmissionCapacity.csv")

i_txCapacityPO <- transmissionCapacity %>% 
  select(r, rr, ps, i_txCapacityPO) %>% 
  filter(!is.na(i_txCapacityPO)) %>% 
  inner_join(
    nwd %>% 
      union_all(
        swd
      )
    , by = c("r", "rr")
  ) %>% 
  rename(
    value = i_txCapacityPO
  ) %>% 
  # Make sure intraregional capacities and line characteristics are zero
  mutate(
    value = ifelse(r == rr, 0, value)
  ) 

## Make sure i_txEarlyComYr equals the first modelled year if it isn't already defined

### Get transmissionUpgrades parameters
transmissionUpgrades <- read_csv("Data/Parameters/transmissionUpgrades.csv")

### i_txEarlyComYr(tupg)
i_txEarlyComYr <- transmissionUpgrades %>% 
  select(tupg, i_txEarlyComYr) %>% 
  mutate(
    i_txEarlyComYr = ifelse(is.na(i_txEarlyComYr), firstYear, i_txEarlyComYr)
  ) %>% 
  rename(
    value = i_txEarlyComYr
  )

## Make sure intraregional capacities and line characteristics are zero

### i_txCapacity(r,rr,ps)
i_txCapacity <- transmissionCapacity %>% 
  select(r, rr, ps, i_txCapacity) %>% 
  mutate(
    i_txCapacity = ifelse(r == rr, 0, i_txCapacity)
  ) %>% 
  rename(
    value = i_txCapacity
  )

### i_txResistance(r,rr,ps)
i_txResistance <- transmissionCapacity %>% 
  select(r, rr, ps, i_txResistance) %>% 
  mutate(
    i_txResistance = ifelse(r == rr, 0, i_txResistance)
  ) %>% 
  rename(
    value = i_txResistance
  )

### i_txReactance(r,rr,ps)
i_txReactance <- transmissionCapacity %>% 
  select(r, rr, ps, i_txReactance) %>% 
  mutate(
    i_txReactance = ifelse(r == rr, 0, i_txReactance)
  ) %>% 
  rename(
    value = i_txReactance
  )

## Assign allowable transitions from one transmission state to another

### Get txUpgradeTransitions subset
txUpgradeTransitions <- read_csv("Data/Subsets/txUpgradeTransitions.csv")

### Get i_txFixedComYr
i_txFixedComYr <- transmissionUpgrades %>% 
  select(tupg, i_txFixedComYr)

### transitions(tupg,r,rr,ps,pss)
transitions <- txUpgradeTransitions %>% 
  union_all(
    txUpgradeTransitions %>% 
      rename(
        r = rr
        , rr = r
      )
  ) %>% 
  # Now remove any illegitimate values from transitions
  left_join(
    i_txFixedComYr
    , by = "tupg"
  ) %>% 
  filter(
    i_txFixedComYr <= lastYear
  ) %>% 
  left_join(
    i_txEarlyComYr %>% 
      rename(i_txEarlyComYr = value)
    , by = "tupg"
  ) %>% 
  filter(
    i_txEarlyComYr <= lastYear
  ) %>% 
  left_join(
    i_txCapacity %>% 
      rename(i_txCapacity = value)
    , by = c("r", "rr", "ps")
  ) %>% 
  filter(
    i_txCapacity != 0
    , tupg != "exist"
    , pss != "initial"
    , r != rr
    , ps != pss
  ) %>% 
  select(
    tupg, r, rr, ps, pss
  )

## Identify all possible states on all paths by recursively applying 'transitions'. First, kick things off
##   by initialising the cases where a non-zero capacity is defined on existing paths.

### allowedStates(r,rr,ps)
allowedStates <- regions %>% 
  crossing(regions %>% rename(rr = r)) %>% 
  filter(r != rr) %>% 
  crossing(data_frame(ps = unique(c(transitions$ps, transitions$pss))))

## Count the allowed upgrade states for each active path

### numAllowedStates(r,rr)
numAllowedStates <- allowedStates %>% 
  group_by(r, rr) %>% 
  count() %>% 
  rename(
    value = n
  ) %>% 
  ungroup()

## Identify all r-rr-ps tuples not in allowedStates

### Get transmissionPathStates set
ps <- read_csv("Data/Sets/transmissionPathStates.csv")

### notAllowedStates(r,rr,ps)
notAllowedStates <- regions %>% 
  crossing(
    regions %>% 
      rename(rr = r)
  ) %>% 
  crossing(
    ps
  ) %>% 
  anti_join(
    allowedStates
    , by = c("r", "rr", "ps")
  )

## Zero out transmission capacities for states not allowed (i.e. something 
##   other than capacity may have resulted in a null transition above)

### i_txCapacity(r,rr,ps)
i_txCapacity <- i_txCapacity %>% 
  left_join(
    notAllowedStates %>% 
      mutate(notAllowedFlag = 1)
    , by = c("r", "rr", "ps")
  ) %>% 
  mutate(
    notAllowedFlag = ifelse(is.na(notAllowedFlag), 0, notAllowedFlag)
    , value = ifelse(notAllowedFlag == 1, 0, value)
  ) %>% 
  select(-notAllowedFlag)

### i_txCapacityPO(r,rr,ps)
i_txCapacityPO <- i_txCapacityPO %>% 
  left_join(
    notAllowedStates %>% 
      mutate(notAllowedFlag = 1)
    , by = c("r", "rr", "ps")
  ) %>% 
  mutate(
    notAllowedFlag = ifelse(is.na(notAllowedFlag), 0, notAllowedFlag)
    , value = ifelse(notAllowedFlag == 1, 0, value)
  ) %>% 
  select(-notAllowedFlag)

## Identify and count all existing or potential interregional transmission paths

### paths(r,rr)
paths <- allowedStates %>% 
  distinct(r, rr)

### numPaths
numPaths <- data_frame(value = nrow(paths))

## Identify all allowable states of upgrade on each path

### upgradeableStates(r,rr,ps)
upgradeableStates <- allowedStates %>% 
  filter(
    ps != "initial"
  )

## Identify the last allowed transmission upgrade state on each path

### lastAllowedState(r,rr,ps)
lastAllowedState <- allowedStates %>% 
  group_by(r, rr) %>% 
  slice(n()) %>% 
  ungroup()

## Identify the allowable upgrade transition sequence for each valid transmission path

### validTransitions(r,rr,ps,pss)
validTransitions <- transitions %>% 
  select(-tupg)

## Assign earliest and fixed transmission upgrade years (let earliest year be the first 
##   year if no earliest year is specified)

### txEarlyComYr(tupg,r,rr,ps,pss)
txEarlyComYr <- transitions %>% 
  inner_join(
    i_txEarlyComYr
    , by = "tupg"
  ) %>% 
  mutate(
    value = ifelse(is.na(value) | value == 0, firstYear, value)
  )

### txFixedComYr(tupg,r,rr,ps,pss)
txFixedComYr <- transitions %>% 
  inner_join(
    i_txFixedComYr
    , by = "tupg"
  ) %>% 
  filter(!is.na(i_txFixedComYr)) %>% 
  rename(
    value = i_txFixedComYr
  )

## Transfer transmission capital cost from a project basis (tupg) to path (r-rr) basis. 
##   Apportion cost to each direction based on pro-rated transmission capcity in each 
##   direction. Convert the lumpy txCapitalCost ($m) to levelised TxCapCharge ($m/yr).

### Get i_txCapitalCost
i_txCapitalCost <- transmissionUpgrades %>% 
  select(tupg, i_txCapitalCost)

### txCapitalCost(r,rr,ps)
txCapitalCost <- transitions %>% 
  select(tupg, r, rr, pss) %>% 
  rename(ps = pss) %>% 
  inner_join(
    i_txCapacity %>% 
      rename(
        i_txCapacity = value
      )
    , by = c("r", "rr", "ps")
  ) %>% 
  inner_join(
    i_txCapitalCost
    , by = "tupg"
  ) %>% 
  group_by(ps) %>% 
  mutate(
    value = (i_txCapitalCost * i_txCapacity) / sum(i_txCapacity)
  ) %>% 
  ungroup() %>% 
  select(r, rr, ps, value)

### txCapCharge(r,rr,ps,y)
txCapCharge <- txCapitalCost %>% 
  rename(
    txCapitalCost = value
  ) %>% 
  crossing(
    txCapRecFac %>% 
      rename(
        txCapRecFac = value
      )
  ) %>% 
  mutate(
    value = txCapitalCost * txCapRecFac
  ) %>% 
  select(r, rr, ps, y, value)

## Identify transmission group constraints as valid if LHS and RHS coefficients are non-zero

### Get transmissionPaths set
p <- read_csv("Data/Sets/transmissionPaths.csv")

### Get transmission group constraints
i_txGrpConstraintsLHS <- read_csv("Data/Parameters/txGrpConstraintsLHS.csv")
i_txGrpConstraintsRHS <- read_csv("Data/Parameters/txGrpConstraintsRHS.csv")

### validTGC(tgc)
validTGC <- p %>% 
  inner_join(
    i_txGrpConstraintsLHS
    , by = "p"
  ) %>% 
  inner_join(
    i_txGrpConstraintsRHS
    , by = "tgc"
  ) %>% 
  filter(
    i_txGrpConstraintsLHS != 0
    , i_txGrpConstraintsRHS != 0
  ) %>% 
  select(tgc)

## Calculate reactance and susceptance by year - this assumes exogenous or fixed timing of transmission expansion
##   decisions, otherwise it stays at the level of initial year.

### reactanceYr(r,rr,y)
reactanceYr <- i_txReactance %>% 
  filter(
    ps == "initial"
  ) %>% 
  crossing(y) %>% 
  select(r, rr, y, value)

for(pathState in unique(txFixedComYr$pss)){
  
  yr <- txFixedComYr %>% 
    filter(pss == pathState) %>% 
    distinct(value)
  
  reactanceValue <- i_txReactance %>% 
    filter %>% 
    filter(ps == pathState) %>% 
    distinct(value)
  
  reactanceYr <- reactanceYr %>% 
    mutate(
      value = case_when(
        y >= yr$value ~ reactanceValue$value
        , TRUE ~ value
      )
    )
  
}

### susceptanceYr(r,rr,y)
susceptanceYr <- reactanceYr %>% 
  mutate(
    value = 1 / value
  )

## Assign bus-branch incidence and group constraint data

### Get mapArcNode 
mapArcNode <- read_csv("Data/Subsets/mapArcNode.csv")

### BBincidence(p,r)
BBincidence <- mapArcNode %>% 
  gather(symbol, region, -p) %>% 
  mutate(
    value = case_when(
      symbol == "r" ~ 1
      , symbol == "rr" ~ -1
    )
  ) %>% 
  rename(
    r = region
  ) %>% 
  select(p, r, value)

## Compute slopes and intercepts for transmission loss functions - assume integerized BTX in first instance:
##   Initialise the loss tranches set (i.e. number tranches = card(n) - 1).

### trnch(n)
trnch <- nSet %>% 
  filter(row_number() < n())

## Determine capacity of each loss tranche, i.e. uniform between 0 and i_txCapacity(r,rr,ps). Note that there is no
##   special reason why the segments must be of uniform sizes. Note too that the 'capacity by tranche' and 'loss by
##   tranche' are only used in determining the intercepts and slopes - they play no explicit role in the model.

### pCap(r,rr,ps,n)
pCap <- i_txCapacity %>% 
  crossing(
    nSet %>% 
      mutate(
        rowNum = row_number()
      ) %>% 
      filter(rowNum != 1)
  ) %>% 
  group_by(
    r, rr, ps
  ) %>% 
  mutate(
    value = (rowNum - 1) * value / n()
  ) %>% 
  ungroup() %>% 
  select(r, rr, ps, n, value)

## Then use the quadratic loss function to compute losses at max capacity of each tranche

### pLoss(r,rr,ps,n)
pLoss <- pCap %>% 
  rename(
    pCap = value
  ) %>% 
  inner_join(
    i_txResistance %>% 
      rename(
        i_txResistance = value
      )
    , by = c("r", "rr", "ps")
  ) %>% 
  mutate(
    value = i_txResistance * (pCap ^2)
  ) %>% 
  select(r, rr, ps, n, value)

## Figure out the upper bound on losses, i.e. losses at max capacity of path in each state

### bigLoss(r,rr,ps)
bigLoss <- pLoss %>% 
  group_by(r, rr, ps) %>% 
  summarise(
    value = max(value)
  )

## Now compute the slope and intercept terms to be used in the GEM loss functions

### lossSlopeMIP(r,rr,ps,n)
lossSlopeMIP <- allowedStates %>% 
  crossing(
    nSet
  ) %>% 
  mutate(
    nbr = str_extract(n, "[:digit:]+")
  ) %>% 
  left_join(
    pLoss %>% 
      rename(
        pLoss = value
      )
    , by = c("r", "rr", "ps", "n")
  ) %>% 
  left_join(
    pCap %>% 
      rename(
        pCap = value
      )
    , by = c("r", "rr", "ps", "n")
  ) %>% 
  mutate(
    pLoss = ifelse(is.na(pLoss), 0, pLoss)
    , pCap = ifelse(is.na(pCap), 0, pCap)
  ) %>% 
  group_by(r, rr, ps) %>% 
  mutate(
    pLossDiff = lead(pLoss) - pLoss
    , pCapDiff = lead(pCap) - pCap
    , value = pLossDiff / pCapDiff
  ) %>% 
  ungroup() %>% 
  select(r, rr, ps, n, value) %>% 
  filter(!is.na(value))

### lossIntercept(r,rr,ps,n)
lossIntercept <- lossSlopeMIP %>% 
  rename(
    lossSlopeMIP = value
  ) %>% 
  inner_join(
    pLoss %>% 
      rename(
        pLoss = value
      )
    , by = c("r", "rr", "ps", "n")
  ) %>% 
  inner_join(
    pCap %>% 
      rename(
        pCap = value
      )
    , by = c("r", "rr", "ps", "n")
  ) %>% 
  mutate(
    value = pLoss - lossSlopeMIP * pCap
  ) %>% 
  select(
    r, rr, ps, n, value
  )

## Overwrite some of the above - pCap, pLoss, lossIntercept and both lossSlopeMIP and lossSlopeRMIP - 
##   if integerization of BTX is not to be employed

### Get txLossesRMIP
txLossesRMIP <- scalarVars %>% 
  filter(variable == "txLossesRMIP") %>% 
  .$value

if(txLossesRMIP){
  
  ## Accept values from above for the initial state, and populate slopes for all states using initial state slopes
  pCap <- pCap %>% 
    mutate(
      value = ifelse(ps != "initial", 0, value)
    )
  
  pLoss <- pLoss %>% 
    mutate(
      value = ifelse(ps != "initial", 0, value)
    )
  
  lossIntercept <- lossIntercept %>% 
    mutate(
      value = ifelse(ps != "initial", 0, value)
    )
  
  ### lossSlopeRMIP(r,rr,n)
  lossSlopeRMIP <- lossSlopeMIP %>% 
    filter(
      ps == "initial"
    ) %>% 
    select(r, rr, n, value)
  
  ## Now loop over paths, states and loss tranches, iteratively computing pCap, pLoss and lossIntercept
  pCap <- pCap %>% 
    mutate(
      # Create lagged 'n' for joining to other tables
      n_lagged = paste0("n", as.numeric(str_extract(n, "[:digit:]+")) - 1)
    ) %>% 
    left_join(
      lossSlopeRMIP %>% 
        rename(
          lossSlopeRMIP = value
        )
      , by = c("r", "rr", "n_lagged" = "n")
    ) %>% 
    left_join(
      i_txResistance %>% 
        rename(
          i_txResistance = value
        )
      , by = c("r", "rr", "ps")
    ) %>% 
    left_join(
      lossIntercept %>% 
        rename(
          lossIntercept = value
        )
      , by = c("r", "rr", "ps", "n" = "n")
    )
  
}

# f) Reserve energy data

### Get parameters defined on reserve class
definedOnReserveClass <- read_csv("Data/Parameters/definedOnReserveClass.csv")

### Get i_reservePenalty
i_ReservePenalty <- read_csv("Data/Parameters/reservePenalty.csv")

### reservesAreas(rc)
reservesAreas <- definedOnReserveClass %>% 
  select(rc, i_ReserveAreas) %>% 
  mutate(
    value = min(2, max(1, i_ReserveAreas))
  ) %>% 
  select(rc, value)

### singleReservesReqF(rc)
singleReservesReqF <- reservesAreas %>% 
  filter(
    value == 1
  )

### penaltyViolateReserves(ild,rc)
penaltyViolateReserves <- i_ReservePenalty %>% 
  mutate(
    value = max(0, i_ReservePenalty)
  ) %>% 
  select(ild, rc, value)

### windCoverPropn(rc)
windCoverPropn <- definedOnReserveClass %>% 
  select(rc, i_propWindCover) %>% 
  # Filter out NAs
  filter(!is.na(i_propWindCover)) %>% 
  mutate(
    value = min(1, max(0, i_propWindCover))
  ) %>% 
  select(rc, value)

### bigM(ild1,ild)
bigM <- paths %>% 
  crossing(ps) %>% 
  inner_join(
    mapild_r
    , by = "r"
  ) %>% 
  inner_join(
    mapild_r %>% 
      rename(ild1 = ild)
    , by = c("rr" = "r")
  ) %>% 
  left_join(
    i_txCapacity %>% 
      rename(
        i_txCapacity = value
      )
    , by = c("r", "rr", "ps")
  ) %>% 
  left_join(
    i_txCapacityPO %>% 
      rename(
        i_txCapacityPO = value
      )
    , by = c("r", "rr", "ps")
  )

# g) Non-free reserves

## Estimate free reserves by path state

### freeReserves(r,rr,ps)
freeReserves <- allowedStates %>% 
  semi_join(
    nwd
    , by = c("r", "rr")
  ) %>% 
  left_join(
    i_txCapacityPO %>% 
      rename(
        i_txCapacityPO = value
      )
    , by = c("r", "rr", "ps")
  ) %>% 
  mutate(
    i_txCapacityPO = ifelse(is.na(i_txCapacityPO), 0, i_txCapacityPO)
    , value = i_txCapacityPO + largestNIplant
  ) %>% 
  union_all(
    allowedStates %>% 
      semi_join(
        swd
        , by = c("r", "rr")
      ) %>% 
      left_join(
        i_txCapacityPO %>% 
          rename(
            i_txCapacityPO = value
          )
        , by = c("r", "rr", "ps")
      ) %>% 
      mutate(
        i_txCapacityPO = ifelse(is.na(i_txCapacityPO), 0, i_txCapacityPO)
        , value = i_txCapacityPO + largestSIplant
      ) 
  ) %>% 
  select(r, rr, ps, value)

## Estimate non-free reserves by path state

### nonFreeReservesCap(r,rr,ps)
nonFreeReservesCap <- allowedStates %>% 
  semi_join(
    nwd %>% 
      union_all(swd)
    , by = c("r", "rr")
  ) %>% 
  left_join(
    i_txCapacity %>% 
      rename(
        i_txCapacity = value
      )
    , by = c("r", "rr", "ps")
  ) %>% 
  left_join(
    freeReserves %>% 
      rename(
        freeReserves = value
      )
    , by = c("r", "rr", "ps")
  ) %>% 
  mutate(
    value = i_txCapacity - freeReserves
  ) %>% 
  select(r, rr, ps, value)

## Figure out capacities (really, upper bounds) of non-free reserves by level.
## i) Find the biggest value in each direction

### bigSwd(r,rr)
bigSwd <- nonFreeReservesCap %>% 
  semi_join(
    swd
    , by = c("r", "rr")
  ) %>% 
  filter(
    value == max(value)
  ) %>% 
  distinct(r, rr, value)

### bigNwd(r,rr)
bigNwd <- nonFreeReservesCap %>% 
  semi_join(
    nwd
    , by = c("r", "rr")
  ) %>% 
  filter(
    value == max(value)
  ) %>% 
  distinct(r, rr, value)

## ii) Set the first level to be 100
## iii) Set subsequent levels to be 100 more than the previous level
## iv) Set the last level to be the biggest value over all states

### pNFresvCap(r,rr,lvl)
pNFresvCap <- bigSwd %>% 
  union_all(
    bigNwd
  ) %>% 
  crossing(
    lvl
  ) %>% 
  group_by(r, rr) %>% 
  mutate(
    value = ifelse(row_number() == max(row_number()), value, row_number() * 100)
  ) %>% 
  ungroup() %>% 
  select(r, rr, lvl, value)

## Figure out costs by level - increment by $5/MWh each level

### pNFresvCost(r,rr,lvl)
pNFresvCost <- swd %>% 
  union_all(
    nwd
  ) %>% 
  crossing(
    lvl
  ) %>% 
  group_by(r, rr) %>% 
  mutate(
    value = row_number() * 5
  ) %>% 
  ungroup() %>% 
  select(r, rr, lvl, value)

