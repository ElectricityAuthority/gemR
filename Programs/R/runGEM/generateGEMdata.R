###############################################
### Title: generateGEMdata()                 ##
### Description: Function for generating     ##
### GEM input GDX. All symbol modification   ##
### occurs in here. A GDX is then created    ##
### that can be read in to GEMsolve.         ##
###############################################
generateGEMdata <- function(
  use_default_demand = FALSE
  , demand_path
  , firstYear
  , lastYear){
  
  # a) Time/date-related sets and parameters.
  
  ## Year variables
  
  ### firstYear - scalar for first calendar year
  # params$firstYear <- tibble(value = globalVars$firstYear)
  params$firstYear <- tibble(value = firstYear)
  
  ### firstYearNum - numeric version of firstYear
  params$firstYearNum <- as.numeric(params$firstYear$value)
  
  ### lastYear - scalar for last calendar year
  # params$lastYear <- tibble(value = globalVars$lastYear)
  params$lastYear <- tibble(value = lastYear)
  
  ### lastYearNum - numeric version of firstYear
  params$lastYearNum <- as.numeric(params$lastYear$value)
  
  ### y - Modelled calendar years
  sets$y <- tibble(y = params$firstYearNum:params$lastYearNum)
  
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
  
  ### thermalFuel(f) - thermal fuels
  subsets$thermalFuel <- subsets$thermalTech %>% 
    inner_join(subsets$mapf_k, by = "k") %>% 
    distinct(f)
  
  ## Count number of regions
  
  ### numReg - scalar for number of regions
  params$numReg <- tibble(value = nrow(sets$r))
  
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
  params$countT <- tibble(value = length(unique(sets$t$t)))
  
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
  params$txAnnuityFacR <- tibble(
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
  
  if(!scalarVars$V2GtechnologyOn){
    
    params$i_nameplate <- params$i_nameplate %>% 
      inner_join(subsets$mapg_k, by = "g") %>% 
      mutate(
        value = ifelse(k == "V2G", 0, value)
      ) %>% 
      select(g, value)
    
  }
  
  ## ii) Existing plant - remove any plant where i_nameplate(g) = 0 from exist(g)
  
  ### Modify exist(g) to remove those with 0 nameplate
  subsets$exist <- subsets$exist %>% 
    inner_join(
      params$i_nameplate %>% 
        filter(value != 0)
      , by = "g"
    ) %>% 
    select(g)
  
  ## iii) A plant is not an existing plant if it hasn't been defined to be existing - 
  ##   remove any plant where i_nameplate(g) = 0 from noExist(g).
  
  ### noExist(g)
  subsets$noExist <- sets$g %>%
    anti_join(subsets$exist, by = "g") %>% 
    inner_join(params$i_nameplate, by = "g") %>% 
    filter(value != 0) %>% 
    select(g)
  
  ## iv) Define plant that are never able to be built. A plant is never to be built if it already exists, 
  ##   if (i_fixComYr or i_EarlyComYr) > lastYear, or if i_nameplate <= 0
  
  ### neverBuild(g)
  subsets$neverBuild <- subsets$noExist %>% 
    left_join(
      params$i_FixComYr %>% 
        rename(i_FixComYr = value)
      , by = "g"
    ) %>% 
    left_join(
      params$i_EarlyComYr %>% 
        rename(i_EarlyComYr = value) 
      , by = "g"
    ) %>% 
    filter(
      i_FixComYr > params$lastYearNum | i_EarlyComYr > params$lastYearNum 
    ) %>% 
    select(g) %>% 
    union_all(
      params$i_nameplate %>% 
        filter(value == 0 | is.na(value)) %>% 
        select(g)
    )
  
  ## v) Define committed plant. To be a committed plant, the plant must not exist, it must not be in the neverBuild set, and
  ##   it must have a fixed commissioning year that is greater than or equal to the first modelled year.
  
  ### commit(g)
  subsets$commit <- subsets$noExist %>% 
    anti_join(subsets$neverBuild, by = "g") %>% 
    inner_join(params$i_FixComYr, by = "g") %>% 
    filter(value >= params$firstYearNum) %>% 
    select(g)
  
  ## vi) Define new plant. A plant is (potentially) new if it is not existing, not committed, and not a member of the neverBuild set.
  
  ### new(g) - potential generation plant that are neither existing nor committed
  subsets$new <- subsets$noExist %>% 
    anti_join(subsets$neverBuild, by = "g") %>% 
    anti_join(subsets$commit, by = "g")
  
  ## vii) Define the years in which it is valid for a generating plant to be built. The plant must either be committed or (potentially)
  ##   new, and the plant can't be a member of the neverBuild set
  
  ### validYrBuild(g,y)
  subsets$validYrBuild <- subsets$new %>% 
    crossing(sets$y) %>% 
    inner_join(
      params$i_EarlyComYr %>% 
        rename(i_EarlyComYr = value)
      , by = "g"
    ) %>% 
    filter(y >= i_EarlyComYr) %>% 
    union_all(
      subsets$commit %>% 
        inner_join(
          params$i_FixComYr
          , by = "g"
        ) %>% 
        rename(y = value) %>% 
        filter(!is.na(y))
    ) %>% 
    select(g, y)
  
  ## viii) Identify the plant that may be built, i.e. it doesn't already exist or it is not otherwise prevented from being built
  
  ### possibleToBuild(g) - generating plant that may possibly be built in any valid build year
  subsets$possibleToBuild <- subsets$validYrBuild %>% 
    distinct(g)
  
  ## ix) Identify generation plant that can be linearly or incrementally built
  
  ### linearPlantBuild(g)
  subsets$linearPlantBuild <- subsets$noExist %>% 
    inner_join(
      subsets$mapg_k
      , by = "g"
    ) %>%
    anti_join(
      params$i_FixComYr %>% 
        filter(!is.na(value))
      , by = "g"
    ) %>% 
    inner_join(
      subsets$linearBuildTech
      , by = "k"
    ) %>% 
    left_join(
      params$i_linearBuildYr %>% 
        filter(!is.na(value)) %>% 
        rename(i_linearBuildYr = value)
      , by = "k"
    ) %>%
    left_join(
      params$i_linearBuildMW %>% 
        filter(!is.na(value)) %>% 
        rename(i_linearBuildMW = value)
      , by = "k"
    ) %>% 
    inner_join(
      params$i_nameplate %>% 
        rename(i_nameplate = value)
      , by = "g"
    ) %>% 
    inner_join(
      params$i_EarlyComYr %>% 
        rename(i_EarlyComYr = value)
      , by = "g"
    ) %>% 
    filter(
      i_nameplate >= i_linearBuildMW | i_EarlyComYr >= i_linearBuildYr
    ) %>% 
    select(g)
  
  ## x) Identify generation plant that must be integer build (must be integer if not linear)
  
  ### integerPlantBuild(g)
  subsets$integerPlantBuild <- subsets$noExist %>% 
    anti_join(subsets$linearPlantBuild, by = "g") %>% 
    anti_join(subsets$neverBuild, by = "g")
  
  ## xi) Identify exceptions to the technology-determined list of plant movers, i.e. if user fixes build year 
  ##   to a legitimate value, then don't allow the plant to be a mover.
  
  ### moverExceptions(g)
  subsets$moverExceptions <- subsets$noExist %>% 
    inner_join(subsets$mapg_k, by = "g") %>% 
    inner_join(subsets$movers, by = "k") %>% 
    left_join(params$i_FixComYr, by = "g") %>% 
    filter(
      value >= params$firstYearNum | value <= params$lastYearNum
    ) %>% 
    select(g)
  
  ## xii) Define the years in which it is valid for a generating plant to operate. The plant must exist; 
  ##   if plant is committed, it is valid to operate it in any year beginning with the year in which it 
  ##   is commissioned; if plant is new, it is valid to operate it in any year beginning with the earliest 
  ##   year in which it may be commissioned; it is not valid to operate any plant that has come to the end 
  ##   of its refurbished life (i.e. can't repeatedly refurbish); it is not valid to operate any plant that 
  ##   has been exogenously retired, or decommissioned; and it is not valid to operate any plant that is never 
  ##   able to be built.
  
  ### validYrOperate(g,y)
  subsets$validYrOperate <- subsets$exist %>% 
    crossing(sets$y) %>% 
    union_all(
      
      subsets$commit %>% 
        inner_join(params$i_FixComYr, by = "g") %>% 
        crossing(sets$y) %>% 
        filter(y >= value) %>% 
        select(g, y)
      
    ) %>% 
    union_all(
      
      subsets$new %>% 
        inner_join(params$i_EarlyComYr, by = "g") %>% 
        crossing(sets$y) %>% 
        filter(y >= value) %>% 
        select(g, y)
      
    ) %>% 
    left_join(
      
      params$i_refurbDecisionYear %>% 
        filter(!is.na(value)) %>% 
        rename(i_refurbDecisionYear = value) %>% 
        inner_join(
          subsets$mapg_k
          , by ="g"
        ) %>% 
        inner_join(
          params$i_refurbishmentLife %>% 
            filter(!is.na(value)) %>% 
            rename(i_refurbishmentLife = value)
          , by = "k"
        ) %>% 
        mutate(
          refurbishmentUpper = i_refurbDecisionYear + i_refurbishmentLife
        ) %>% 
        select(g, refurbishmentUpper)
      
      , by = "g"
    ) %>% 
    # Filter out plant/year combinations where there is an upper bound on refurbishment
    filter(is.na(refurbishmentUpper) | y <= refurbishmentUpper) %>% 
    left_join(
      
      params$i_ExogenousRetireYr %>% 
        filter(!is.na(value)) %>% 
        rename(i_ExogenousRetireYr = value)
      , by = "g"
      
    ) %>% 
    # Filter out plant/year combinations where the plant has been retired
    filter(is.na(i_ExogenousRetireYr) | y < i_ExogenousRetireYr) %>% 
    select(g, y) %>% 
    arrange(g, y)
  
  ## xiii) North and South Island plant
  
  ### nigen(g)
  subsets$nigen <- subsets$mapg_ild %>% 
    filter(ild == "ni") %>% 
    select(g) %>% 
    anti_join(subsets$neverBuild, by = "g")
  
  ### sigen(g)
  subsets$sigen <- subsets$mapg_ild %>% 
    filter(ild == "si") %>% 
    select(g) %>% 
    anti_join(subsets$neverBuild, by = "g")
  
  ## Define capacity of existing plant in first modelled year. Be aware that if capacity is committed in 
  ##   the first modelled year, the plant will be in the commit(g) and noExist(g) sets
  
  ### initialCapacity(g) - capacity of existing generation plant 
  params$initialCapacity <- params$i_nameplate %>% 
    inner_join(subsets$exist, by = "g")
  
  ## Define exogenously retired MW by plant and year
  
  ### exogMWretired(g,y)
  params$exogMWretired <- params$i_ExogenousRetireYr %>% 
    rename(y = value) %>% 
    inner_join(sets$y, by = "y") %>% 
    inner_join(params$i_nameplate, by = "g") 
  
  ## Identify all generation plant that may be endogenously retired
  
  ### possibleToEndogRetire(g)
  subsets$possibleToEndogRetire <- params$i_refurbDecisionYear %>% 
    filter(!is.na(value)) %>% 
    distinct(g)
  
  ## Identify all generation plant that may be retired (endogenously or exogenously)
  
  ### possibleToRetire(g)
  subsets$possibleToRetire  <- subsets$possibleToEndogRetire %>% 
    union_all(
      params$i_ExogenousRetireYr %>%  
        filter(!is.na(value)) %>% 
        distinct(g)
    ) %>% 
    distinct(g)
  
  ## Define contribution to peak capacity by plant
  
  ### peakConPlant(g,y)
  params$peakConPlant <- subsets$mapg_k %>% 
    crossing(sets$y) %>% 
    inner_join(
      params$i_peakContribution
      , by = "k"
    ) %>% 
    select(g, y, value)
  
  ### NWpeakConPlant(g,y)
  params$NWpeakConPlant <- subsets$mapg_k %>% 
    crossing(sets$y) %>% 
    inner_join(
      params$i_NWpeakContribution
      , by = "k"
    ) %>% 
    select(g, y, value) %>% 
    filter(!is.na(value))
  
  ## Initialise the FOF multiplier - compute a weighted average using annual hours per load block as the weights
  
  ### WtdAvgFOFmultiplier(k,lb)
  params$WtdAvgFOFmultiplier <- params$i_FOFmultiplier %>%
    rename(i_FOFmultiplier = value) %>% 
    inner_join(
      params$hoursPerBlock %>% 
        rename(hoursPerBlock = value)
      , by = "lb"
    ) %>%
    group_by(k, lb) %>% 
    summarise(
      value = sum(hoursPerBlock * i_FOFmultiplier / sum(hoursPerBlock))
    ) %>% 
    ungroup()
  
  ## Derive the minimum and maximum capacity factors for each plant and period.
  
  ### maxCapFactPlant(g,t,lb)
  params$maxCapFactPlant <- params$i_PltCapFact %>% 
    rename(i_PltCapFact = value) %>% 
    inner_join(
      subsets$mapm_t
      , by = "m"
    ) %>% 
    crossing(sets$lb) %>% 
    group_by(g, t, lb) %>%
    # First average over time 
    summarise(
      value = sum(i_PltCapFact) / n()
    ) %>% 
    # Then, set all scheduable hydro max capacity factors to zero
    inner_join(
      subsets$mapg_k
      , by = "g"
    ) %>% 
    mutate(
      value = ifelse(k %in% subsets$hydroSched$k, 0, value)
    ) %>% 
    # Overwrite max capacity factor for hydro with user-defined, non-zero i_maxHydroCapFact(g) values
    left_join(
      params$i_maxHydroCapFact %>% 
        rename(i_maxHydroCapFact = value)
      , by = "g"
    ) %>% 
    mutate(
      value = ifelse(is.na(i_maxHydroCapFact), value, i_maxHydroCapFact)
    ) %>% 
    # Adjust all max capacity factors for forced outage factor
    inner_join(
      params$i_fof %>% 
        rename(i_fof = value)
      , by = "g"
    ) %>% 
    left_join(
      params$WtdAvgFOFmultiplier %>% 
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
  
  ### minCapFactPlant(g,y,t)
  params$minCapFactPlant <- subsets$schedHydroPlant %>% 
    inner_join(
      params$i_minHydroCapFact %>% 
        filter(!is.na(value))
      , by = "g"
    ) %>% 
    # Min capacity factor also 'non-meaningfully' defined to a low non-zero value for wind plant
    union_all(
      subsets$mapg_k %>% 
        filter(k == "Wind") %>% 
        mutate(
          value = 0.001
        ) %>% 
        select(g, value)
    ) %>% 
    crossing(sets$y) %>% 
    crossing(sets$t) %>% 
    select(g, y, t, value)
  
  ## Identify all the generation plant that may possibly be refurbished or endogenously retired and 
  ##   the years in which that retirement may/will occur
  
  ### possibleToRefurbish(g)
  subsets$possibleToRefurbish <- subsets$exist %>% 
    inner_join(
      subsets$mapg_k
      , by = "g"
    ) %>% 
    inner_join(
      subsets$refurbish
      , by = "k"
    ) %>% 
    inner_join(
      params$i_refurbDecisionYear%>% 
        filter(!is.na(value))
      , by = "g"
    ) %>% 
    inner_join(
      params$i_refurbCapitalCost %>% 
        filter(!is.na(value))
      , by = "g"
    ) %>% 
    select(g)
  
  params$endogenousRetireInfo <- subsets$possibleToRefurbish %>% 
    inner_join(
      subsets$mapg_k
      , by = "g"
    ) %>% 
    mutate(
      # Flag if in endogRetire subset
      endogRetireFlag = ifelse(k %in% subsets$endogRetire$k, 1, 0)
    ) %>% 
    crossing(sets$y) %>% 
    inner_join(
      params$i_refurbDecisionYear %>% 
        rename(i_refurbDecisionYear = value)
      , by = "g" 
    ) %>% 
    inner_join(
      params$i_retireOffsetYrs %>% 
        rename(i_retireOffsetYrs = value)
      , by = "k"
    )
  
  ### endogenousRetireDecisnYrs(g,y)
  subsets$endogenousRetireDecisnYrs <- params$endogenousRetireInfo %>% 
    filter(
      case_when(
        endogRetireFlag == 1 ~ y >= (params$firstYearNum + scalarVars$noRetire) & y <= i_refurbDecisionYear 
        , TRUE ~ y >= (params$firstYearNum + scalarVars$noRetire) & y == i_refurbDecisionYear 
      )
    ) %>% 
    select(g, y)
  
  ### endogenousRetireYrs(g,y)
  subsets$endogenousRetireYrs <- params$endogenousRetireInfo %>% 
    filter(
      case_when(
        endogRetireFlag == 1 ~ y >= (params$firstYearNum + scalarVars$noRetire + i_retireOffsetYrs) & y <= i_refurbDecisionYear + i_retireOffsetYrs
        , TRUE ~ y >= (params$firstYearNum + scalarVars$noRetire + i_retireOffsetYrs) & y == i_refurbDecisionYear + i_retireOffsetYrs
      )
    ) %>% 
    select(g, y)
  
  ## Compute the years a plant must keep going for after the decision to endogenously retire it has been made
  
  ### continueAftaEndogRetire(g)
  params$continueAftaEndogRetire <- subsets$possibleToEndogRetire %>% 
    inner_join(
      subsets$mapg_k
      , by = "g"
    ) %>% 
    inner_join(
      subsets$refurbish
      , by = "k"
    ) %>% 
    inner_join(
      params$i_retireOffsetYrs
      , by = "k"
    ) %>% 
    select(g, value)
  
  ## Define capital costs for new generation plant:
  ##   Capital costs are first calculated as if capex is lumpy. After any adjustments, they are then converted
  ##   to a levelised or annualised basis (i.e. see capCharge).
  
  ## First, transfer i_capitalCost to capexPlant and i_refurbCapitalCost to refurbCapexPlant, and convert both to $/MW
  
  ### vbleConCostPlant(g)
  params$vbleConCostPlant <- params$i_connectionCost %>% 
    filter(!is.na(value)) %>% 
    rename(i_connectionCost = value) %>% 
    inner_join(
      params$i_nameplate %>% 
        rename(i_nameplate = value)
      , by = "g"
    ) %>% 
    mutate(
      value = 1e6 * i_connectionCost / i_nameplate
    ) %>% 
    select(g, value)
  
  ### capexPlant(g)
  params$capexPlant <- params$i_capitalCost %>% 
    filter(!is.na(value)) %>% 
    mutate(
      value = value * 1e3
    ) %>% 
    select(g, value) %>% 
    # Randomly adjust capexPlant to create mathematically different costs - this helps the solver but makes no
    #   appreciable economic difference provided randomCapexCostAdjuster is small
    inner_join(
      subsets$mapg_k
      , by = "g"
    ) %>% 
    mutate(
      value = case_when(
        k %in% subsets$randomiseCapex$k & g %in% subsets$noExist$g ~ runif(
          1
          , min = value - (value * scalarVars$randomCapexCostAdjuster)
          , max = value + (value * scalarVars$randomCapexCostAdjuster)
        )
        , TRUE ~ value
      )
    ) %>% 
    # Add on the 'variablised' connection costs to the adjusted plant capital costs - continue to yield NZ$/MW
    left_join(
      params$vbleConCostPlant %>% 
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
  params$refurbCapexPlant <- params$i_refurbCapitalCost %>% 
    filter(!is.na(value)) %>% 
    mutate(
      value = value * 1e3
    ) %>% 
    select(g, value) %>% 
    # Zero out any refubishment capex costs if the plant is not actually a candidate for refurbishment
    mutate(
      value = ifelse(g %in% subsets$possibleToRefurbish$g, value, 0)
    )
  
  ## Finally, convert lumpy capital costs to levelised capital charge (units are now NZ$/MW/yr)
  
  ### capCharge(g,y)
  params$capCharge <- params$capexPlant %>% 
    inner_join(
      subsets$mapg_k
      , by = "g"
    ) %>% 
    inner_join(
      params$capRecFac %>% 
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
  params$refurbCapCharge <- params$refurbCapexPlant %>% 
    inner_join(
      subsets$mapg_k
      , by = "g"
    ) %>% 
    inner_join(
      params$capRecFac %>% 
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
      params$i_refurbDecisionYear %>% 
        rename(
          i_refurbDecisionYear = value
        )
      , by = "g"
    ) %>% 
    inner_join(
      params$i_refurbishmentLife %>% 
        rename(
          i_refurbishmentLife = value
        )
      , by = c("k")
    ) %>% 
    filter(
      y >= i_refurbDecisionYear
      , y <= (i_refurbDecisionYear + i_refurbishmentLife)
    ) %>% 
    select(g, y, value)
  
  ## Calculate reserve capability per generating plant
  
  ### reservesCapability(g,rc)
  params$reservesCapability <- params$i_plantReservesCap %>% 
    rename(i_plantReservesCap = value) %>% 
    inner_join(
      params$i_nameplate %>% 
        rename(i_nameplate = value)
      , by = "g"
    ) %>% 
    mutate(
      value = i_plantReservesCap * i_nameplate
    ) %>% 
    select(g, rc, value)
  
  ## Add any fixed costs associated with fuel production and delivery to the fixed OM costs by plant
  
  ### Join together fuel cost symbols 
  params$fuelCosts <- params$i_fixedOM %>% 
    rename(i_fixedOM = value) %>% 
    inner_join(
      params$i_fixedFuelCosts %>% 
        rename(i_fixedFuelCosts = value)
      , by = "g"
    ) %>% 
    inner_join(
      params$i_heatrate %>% 
        rename(i_heatrate = value)
      , by = "g"
    )
  
  ### i_fixedOM(g)
  params$i_fixedOM <- params$fuelCosts %>% 
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
  
  ### locationFactor(g)
  params$locationFactor <- subsets$mapg_e %>% 
    inner_join(
      params$i_zonalLocFacs
      , by = "e"         
    ) %>% 
    mutate(
      value = 1 / value
      # Set equal to 1 if it should be zero (it shouldn't be)
      , value = ifelse(value == 0, 1, value)
    ) %>% 
    select(g, value)
  
  ### If there are more than 2 regions, set location factors to 1
  if(params$numReg > 2){
    
    params$locationFactor <- params$locationFactor %>% 
      mutate(
        value = 1
      )  
    
  }
  
  ## Collect up the various cost factors into the so-called ensemble factor
  
  ### ensembleFactor(g)
  params$ensembleFactor <- params$i_hydroPeakingFactor %>% 
    rename(i_hydroPeakingFactor = value) %>% 
    inner_join(
      params$locationFactor %>% 
        filter(!is.na(value)) %>% 
        rename(locationFactor = value)
      , by = "g"
    ) %>% 
    mutate(
      value = ifelse(is.na(i_hydroPeakingFactor), locationFactor, locationFactor * (1 / i_hydroPeakingFactor))
    ) %>% 
    select(g, value)
  
  ## e) Transmission data
  
  ## Let the last region declared be the slack bus (note that set r may not be 
  ##   ordered if users don't maintain unique set elements)
  
  ### slackBus(r)
  subsets$slackBus <- sets$r %>% 
    tail(1)
  
  ## Define the lower triangular part of region-region matrix, i.e. ord(r) > ord(rr)
  
  ### regLower(r,rr)
  subsets$regLower <- sets$r %>% 
    mutate(
      rr = lag(r)
    ) %>% 
    filter(!is.na(rr))
  
  ## Define regions at each end of NI-SI HVDC link
  
  subsets$BenmoreReg <- subsets$Benmore %>% 
    inner_join(
      subsets$mapi_r
      , by = "i"
    ) %>% 
    select(r)
  
  subsets$HaywardsReg <- subsets$Haywards %>% 
    inner_join(
      subsets$mapi_r
      , by = "i"
    ) %>% 
    select(r)
  
  ### nwd(r,rr)
  subsets$nwd <- subsets$BenmoreReg %>% 
    add_column(rr = subsets$HaywardsReg$r)
  
  ### swd(r,rr)
  subsets$swd <- subsets$HaywardsReg %>% 
    add_column(rr = subsets$BenmoreReg$r)
  
  ## Define interisland pairings
  
  ### interIsland(ild,ild1)
  subsets$interIsland <- sets$ild %>% 
    crossing(
      sets$ild %>%
        rename(ild1 = ild)
    ) %>% 
    filter(
      ild != ild1
    )
  
  ### interIslandRegions(r,rr)
  subsets$interIslandRegions <- subsets$nwd %>% 
    union_all(
      subsets$swd
    )
  
  ## Make sure i_txCapacityPO is not specified for anything but the current HVDC link
  
  params$i_txCapacityPO <- params$i_txCapacityPO %>% 
    filter(!is.na(value)) %>% 
    inner_join(
      subsets$nwd %>% 
        union_all(
          subsets$swd
        )
      , by = c("r", "rr")
    ) %>% 
    # Make sure intraregional capacities and line characteristics are zero
    mutate(
      value = ifelse(r == rr, 0, value)
    ) 
  
  ## Make sure i_txEarlyComYr equals the first modelled year if it isn't already defined
  
  ### i_txEarlyComYr(tupg)
  params$i_txEarlyComYr <- params$i_txEarlyComYr %>% 
    mutate(
      value = ifelse(is.na(value), firstYear, value)
    )
  
  ## Make sure intraregional capacities and line characteristics are zero
  
  ### i_txCapacity(r,rr,ps)
  params$i_txCapacity <- params$i_txCapacity %>% 
    mutate(
      value = ifelse(r == rr, 0, value)
    ) 
  
  ### i_txResistance(r,rr,ps)
  params$i_txResistance <- params$i_txResistance %>% 
    mutate(
      value = ifelse(r == rr, 0, value)
    )
  
  ### i_txReactance(r,rr,ps)
  params$i_txReactance <- params$i_txReactance %>% 
    mutate(
      value = ifelse(r == rr, 0, value)
    ) 
  
  ## Assign allowable transitions from one transmission state to another
  
  ### transitions(tupg,r,rr,ps,pss)
  subsets$transitions <- subsets$txUpgradeTransitions %>% 
    union_all(
      subsets$txUpgradeTransitions %>% 
        rename(
          r = rr
          , rr = r
        )
    ) %>% 
    # Now remove any illegitimate values from transitions
    left_join(
      params$i_txFixedComYr %>% 
        rename(i_txFixedComYr = value)
      , by = "tupg"
    ) %>% 
    filter(
      is.na(i_txFixedComYr) | i_txFixedComYr <= params$lastYearNum
    ) %>% 
    left_join(
      params$i_txEarlyComYr %>% 
        rename(i_txEarlyComYr = value)
      , by = "tupg"
    ) %>% 
    filter(
      is.na(i_txEarlyComYr) | i_txEarlyComYr <= params$lastYearNum
    ) %>% 
    left_join(
      params$i_txCapacity %>% 
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
  subsets$allowedStates <- sets$r %>% 
    crossing(sets$r %>% rename(rr = r)) %>% 
    filter(r != rr) %>% 
    crossing(
      tibble(
        ps = unique(c(subsets$transitions$ps, subsets$transitions$pss)
        )
      )
    )
  
  ## Count the allowed upgrade states for each active path
  
  ### numAllowedStates(r,rr)
  params$numAllowedStates <- subsets$allowedStates %>% 
    group_by(r, rr) %>% 
    count() %>% 
    rename(
      value = n
    ) %>% 
    ungroup()
  
  ## Identify all r-rr-ps tuples not in allowedStates
  
  ### notAllowedStates(r,rr,ps)
  subsets$notAllowedStates <- sets$r %>% 
    crossing(
      sets$r %>% 
        rename(rr = r)
    ) %>% 
    crossing(
      sets$ps
    ) %>% 
    anti_join(
      subsets$allowedStates
      , by = c("r", "rr", "ps")
    )
  
  ## Zero out transmission capacities for states not allowed (i.e. something 
  ##   other than capacity may have resulted in a null transition above)
  
  ### i_txCapacity(r,rr,ps)
  params$i_txCapacity <- params$i_txCapacity %>% 
    left_join(
      subsets$notAllowedStates %>% 
        mutate(notAllowedFlag = 1)
      , by = c("r", "rr", "ps")
    ) %>% 
    mutate(
      notAllowedFlag = ifelse(is.na(notAllowedFlag), 0, notAllowedFlag)
      , value = ifelse(notAllowedFlag == 1, 0, value)
    ) %>% 
    select(-notAllowedFlag)
  
  ### i_txCapacityPO(r,rr,ps)
  params$i_txCapacityPO <- params$i_txCapacityPO %>% 
    left_join(
      subsets$notAllowedStates %>% 
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
  subsets$paths <- subsets$allowedStates %>% 
    distinct(r, rr)
  
  ### numPaths
  params$numPaths <- tibble(value = nrow(subsets$paths))
  
  ## Identify all allowable states of upgrade on each path
  
  ### upgradeableStates(r,rr,ps)
  subsets$upgradeableStates <- subsets$allowedStates %>% 
    filter(
      ps != "initial"
    )
  
  ## Identify the last allowed transmission upgrade state on each path
  
  ### lastAllowedState(r,rr,ps)
  subsets$lastAllowedState <- subsets$allowedStates %>% 
    group_by(r, rr) %>% 
    slice(n()) %>% 
    ungroup()
  
  ## Identify the allowable upgrade transition sequence for each valid transmission path
  
  ### validTransitions(r,rr,ps,pss)
  subsets$validTransitions <- subsets$transitions %>% 
    select(-tupg)
  
  ## Assign earliest and fixed transmission upgrade years (let earliest year be the first 
  ##   year if no earliest year is specified)
  
  ### txEarlyComYr(tupg,r,rr,ps,pss)
  params$txEarlyComYr <- subsets$transitions %>% 
    inner_join(
      params$i_txEarlyComYr
      , by = "tupg"
    ) %>% 
    mutate(
      value = ifelse(is.na(value) | value == 0, firstYear, value)
    )
  
  ### txFixedComYr(tupg,r,rr,ps,pss)
  params$txFixedComYr <- subsets$transitions %>% 
    inner_join(
      params$i_txFixedComYr
      , by = "tupg"
    ) %>% 
    filter(!is.na(value))
  
  ## Transfer transmission capital cost from a project basis (tupg) to path (r-rr) basis. 
  ##   Apportion cost to each direction based on pro-rated transmission capcity in each 
  ##   direction. Convert the lumpy txCapitalCost ($m) to levelised TxCapCharge ($m/yr).
  
  ### txCapitalCost(r,rr,ps)
  params$txCapitalCost <- subsets$transitions %>% 
    select(tupg, r, rr, pss) %>% 
    rename(ps = pss) %>% 
    inner_join(
      params$i_txCapacity %>% 
        rename(
          i_txCapacity = value
        )
      , by = c("r", "rr", "ps")
    ) %>% 
    inner_join(
      params$i_txCapitalCost %>% 
        rename(i_txCapitalCost = value)
      , by = "tupg"
    ) %>% 
    group_by(ps) %>% 
    mutate(
      value = (i_txCapitalCost * i_txCapacity) / sum(i_txCapacity)
    ) %>% 
    ungroup() %>% 
    select(r, rr, ps, value)
  
  ### txCapCharge(r,rr,ps,y)
  params$txCapCharge <- params$txCapitalCost %>% 
    rename(
      txCapitalCost = value
    ) %>% 
    crossing(
      params$txCapRecFac %>% 
        rename(
          txCapRecFac = value
        )
    ) %>% 
    mutate(
      value = txCapitalCost * txCapRecFac
    ) %>% 
    select(r, rr, ps, y, value)
  
  ## Identify transmission group constraints as valid if LHS and RHS coefficients are non-zero
  
  ### validTGC(tgc)
  subsets$validTGC <- sets$p %>% 
    inner_join(
      params$i_txGrpConstraintsLHS %>% 
        rename(i_txGrpConstraintsLHS = value)
      , by = "p"
    ) %>% 
    inner_join(
      params$i_txGrpConstraintsRHS %>% 
        rename(i_txGrpConstraintsRHS = value)
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
  params$reactanceYr <- params$i_txReactance %>% 
    filter(
      ps == "initial"
    ) %>% 
    crossing(sets$y) %>% 
    select(r, rr, y, value)
  
  for(pathState in unique(params$txFixedComYr$pss)){
    
    yr <- params$txFixedComYr %>% 
      filter(pss == pathState) %>% 
      distinct(value)
    
    reactanceValue <- params$i_txReactance %>% 
      filter(ps == pathState) %>% 
      distinct(value)
    
    params$reactanceYr <- params$reactanceYr %>% 
      mutate(
        value = case_when(
          y >= yr$value ~ reactanceValue$value
          , TRUE ~ value
        )
      )
    
    rm(yr, reactanceValue)
    
  }
  
  ### susceptanceYr(r,rr,y)
  params$susceptanceYr <- params$reactanceYr %>% 
    mutate(
      value = 1 / value
    )
  
  ## Assign bus-branch incidence and group constraint data
  
  ### BBincidence(p,r)
  params$BBincidence <- subsets$mapArcNode %>% 
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
  subsets$trnch <- sets$n %>% 
    filter(row_number() < n())
  
  ## Determine capacity of each loss tranche, i.e. uniform between 0 and i_txCapacity(r,rr,ps). Note that there is no
  ##   special reason why the segments must be of uniform sizes. Note too that the 'capacity by tranche' and 'loss by
  ##   tranche' are only used in determining the intercepts and slopes - they play no explicit role in the model.
  
  ### pCap(r,rr,ps,n)
  params$pCap <- params$i_txCapacity %>% 
    crossing(
      sets$n %>% 
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
  params$pLoss <- params$pCap %>% 
    rename(
      pCap = value
    ) %>% 
    inner_join(
      params$i_txResistance %>% 
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
  params$bigLoss <- params$pLoss %>% 
    group_by(r, rr, ps) %>% 
    summarise(
      value = max(value)
    )
  
  ## Now compute the slope and intercept terms to be used in the GEM loss functions
  
  ### lossSlopeMIP(r,rr,ps,n)
  params$lossSlopeMIP <- subsets$allowedStates %>% 
    crossing(
      sets$n
    ) %>% 
    mutate(
      nbr = str_extract(n, "[:digit:]+")
    ) %>% 
    left_join(
      params$pLoss %>% 
        rename(
          pLoss = value
        )
      , by = c("r", "rr", "ps", "n")
    ) %>% 
    left_join(
      params$pCap %>% 
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
  params$lossIntercept <- params$lossSlopeMIP %>% 
    rename(
      lossSlopeMIP = value
    ) %>% 
    inner_join(
      params$pLoss %>% 
        rename(
          pLoss = value
        )
      , by = c("r", "rr", "ps", "n")
    ) %>% 
    inner_join(
      params$pCap %>% 
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
  
  ### lossSlopeRMIP(r,rr,n)
  params$lossSlopeRMIP <- params$lossSlopeMIP %>% 
    filter(
      ps == "initial"
    ) %>% 
    select(r, rr, n, value)
  
  ## Overwrite some of the above - pCap, pLoss, lossIntercept and both lossSlopeMIP and lossSlopeRMIP - 
  ##   if integerization of BTX is not to be employed
  
  # if(scalarVars$txLossesRMIP){
  #   
  #   ## Accept values from above for the initial state, and populate slopes for all states using initial state slopes
  #   pCap <- pCap %>% 
  #     mutate(
  #       value = ifelse(ps != "initial", 0, value)
  #     )
  #   
  #   pLoss <- pLoss %>% 
  #     mutate(
  #       value = ifelse(ps != "initial", 0, value)
  #     )
  #   
  #   lossIntercept <- lossIntercept %>% 
  #     mutate(
  #       value = ifelse(ps != "initial", 0, value)
  #     )
  #   
  #   ### lossSlopeRMIP(r,rr,n)
  #   params$lossSlopeRMIP <- params$lossSlopeMIP %>% 
  #     filter(
  #       ps == "initial"
  #     ) %>% 
  #     select(r, rr, n, value)
  #   
  #   ## Now loop over paths, states and loss tranches, iteratively computing pCap, pLoss and lossIntercept
  #   pCap <- pCap %>% 
  #     mutate(
  #       # Create lagged 'n' for joining to other tables
  #       n_lagged = paste0("n", as.numeric(str_extract(n, "[:digit:]+")) - 1)
  #     ) %>% 
  #     left_join(
  #       lossSlopeRMIP %>% 
  #         rename(
  #           lossSlopeRMIP = value
  #         )
  #       , by = c("r", "rr", "n_lagged" = "n")
  #     ) %>% 
  #     left_join(
  #       i_txResistance %>% 
  #         rename(
  #           i_txResistance = value
  #         )
  #       , by = c("r", "rr", "ps")
  #     ) %>% 
  #     left_join(
  #       lossIntercept %>% 
  #         rename(
  #           lossIntercept = value
  #         )
  #       , by = c("r", "rr", "ps", "n" = "n")
  #     )
  #   
  # }
  
  # f) Reserve energy data
  
  ### reservesAreas(rc)
  # The following assignment was quite convoluted so this parameter has been hard-coded in 'readInputSymbols.R'
  
  # reservesAreas <- definedOnReserveClass %>% 
  #   select(rc, i_ReserveAreas) %>% 
  #   mutate(
  #     value = min(2, max(1, i_ReserveAreas))
  #   ) %>% 
  #   select(rc, value)
  
  ### singleReservesReqF(rc)
  params$singleReservesReqF <- params$reservesAreas %>% 
    filter(
      value == 1
    )
  
  ### penaltyViolateReserves(ild,rc)
  params$penaltyViolateReserves <- params$i_ReservePenalty %>% 
    mutate(
      value = max(0, value)
    ) %>% 
    select(ild, rc, value)
  
  ### windCoverPropn(rc)
  params$windCoverPropn <- params$i_propWindCover %>% 
    # Filter out NAs
    filter(!is.na(value)) %>% 
    mutate(
      value = min(1, max(0, value))
    ) %>% 
    select(rc, value)
  
  ### bigM(ild1,ild)
  params$bigM <- subsets$paths %>% 
    crossing(sets$ps) %>% 
    inner_join(
      subsets$mapild_r
      , by = "r"
    ) %>% 
    inner_join(
      subsets$mapild_r %>% 
        rename(ild1 = ild)
      , by = c("rr" = "r")
    ) %>% 
    left_join(
      params$i_txCapacity %>% 
        rename(
          i_txCapacity = value
        )
      , by = c("r", "rr", "ps")
    ) %>% 
    left_join(
      params$i_txCapacityPO %>% 
        rename(
          i_txCapacityPO = value
        )
      , by = c("r", "rr", "ps")
    ) %>% 
    # Change NAs to 0
    mutate(
      i_txCapacity = ifelse(is.na(i_txCapacity), 0, i_txCapacity)
      , i_txCapacityPO = ifelse(is.na(i_txCapacityPO), 0, i_txCapacityPO)
    ) %>% 
    group_by(ild, ild1) %>% 
    summarise(
      value = max(i_txCapacity) - min(i_txCapacityPO)
    ) %>% 
    # Add -INF where both islands are the same
    union_all(
      tibble(
        ild = c("ni", "si")
        , ild1 = c("ni", "si")
        , value = c(-Inf, -Inf)
      )
    )
  
  # g) Non-free reserves
  
  ## Estimate free reserves by path state
  
  ### freeReserves(r,rr,ps)
  params$freeReserves <- subsets$allowedStates %>% 
    semi_join(
      subsets$nwd
      , by = c("r", "rr")
    ) %>% 
    left_join(
      params$i_txCapacityPO %>% 
        rename(
          i_txCapacityPO = value
        )
      , by = c("r", "rr", "ps")
    ) %>% 
    mutate(
      i_txCapacityPO = ifelse(is.na(i_txCapacityPO), 0, i_txCapacityPO)
      , value = i_txCapacityPO + params$largestNIplant$value
    ) %>% 
    union_all(
      subsets$allowedStates %>% 
        semi_join(
          subsets$swd
          , by = c("r", "rr")
        ) %>% 
        left_join(
          params$i_txCapacityPO %>% 
            rename(
              i_txCapacityPO = value
            )
          , by = c("r", "rr", "ps")
        ) %>% 
        mutate(
          i_txCapacityPO = ifelse(is.na(i_txCapacityPO), 0, i_txCapacityPO)
          , value = i_txCapacityPO + params$largestSIplant$value
        ) 
    ) %>% 
    select(r, rr, ps, value)
  
  ## Estimate non-free reserves by path state
  
  ### nonFreeReservesCap(r,rr,ps)
  params$nonFreeReservesCap <- subsets$allowedStates %>% 
    semi_join(
      subsets$nwd %>% 
        union_all(subsets$swd)
      , by = c("r", "rr")
    ) %>% 
    left_join(
      params$i_txCapacity %>% 
        rename(
          i_txCapacity = value
        )
      , by = c("r", "rr", "ps")
    ) %>% 
    left_join(
      params$freeReserves %>% 
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
  params$bigSwd <- params$nonFreeReservesCap %>% 
    semi_join(
      subsets$swd
      , by = c("r", "rr")
    ) %>% 
    filter(
      value == max(value)
    ) %>% 
    distinct(r, rr, value)
  
  ### bigNwd(r,rr)
  params$bigNwd <- params$nonFreeReservesCap %>% 
    semi_join(
      subsets$nwd
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
  params$pNFresvCap <- params$bigSwd %>% 
    union_all(
      params$bigNwd
    ) %>% 
    crossing(
      sets$lvl
    ) %>% 
    group_by(r, rr) %>% 
    mutate(
      value = ifelse(row_number() == max(row_number()), value, row_number() * 100)
    ) %>% 
    ungroup() %>% 
    select(r, rr, lvl, value)
  
  ## Figure out costs by level - increment by $5/MWh each level
  
  ### pNFresvCost(r,rr,lvl)
  params$pNFresvCost <- subsets$swd %>% 
    union_all(
      subsets$nwd
    ) %>% 
    crossing(
      sets$lvl
    ) %>% 
    group_by(r, rr) %>% 
    mutate(
      value = row_number() * 5
    ) %>% 
    ungroup() %>% 
    select(r, rr, lvl, value)
  
  ## Create VOLLplant sets
  
  ### s
  sets$s <- tibble(s = paste0("VOLL", sets$r$r))
  
  ### maps_r(s,r)
  subsets$maps_r <- sets$s %>% 
    crossing(sets$r) %>% 
    filter(str_detect(s, r))
  
  # If the default demand flag is set to FALSE, replace the default 'i_NrgDemand' parameter with the new one
  if(!use_default_demand){
    
    params$i_NrgDemand <- read_csv(demand_path) %>% 
      rename(
        value = i_NrgDemand
      )
    
  } 
  
  # Subset to the symbols that are required by GEMsolve
  setsForGDX <- list.subset(sets, c(
    "r", "f", "k", "g", "t", "lb", "rc", "hY", "v", "y", "n", "o", "p", "ild",
    "ps", "tupg", "tgc", "aggR", "lvl", "s"
  ))
  
  subsetsForGDX <- list.subset(subsets, c(
    "firstYr", "allButFirstYr", "movers", "moverExceptions", "demandGen", "sigen", "nigen",
    "thermalFuel", "gas", "diesel", "firstPeriod", "trnch", "wind", "renew", "exist", "noExist",
    "commit", "rightAdjacentBlocks", "schedHydroUpg", "pumpedHydroPlant", "schedHydroPlant",
    "mapm_t", "mapg_k", "mapSH_Upg", "mapv_g", "mapg_o", "mapg_f", "mapg_ild",
    "possibleToEndogRetire", "possibleToBuild", "possibleToRefurbish", "validYrBuild", 
    "validYrOperate", "integerPlantBuild", "linearPlantBuild", "paths", "slackBus", 
    "transitions", "validTransitions", "mapg_r", "mapild_r", "allowedStates",
    "notAllowedStates", "upgradeableStates", "regLower", "nwd", "swd", "interIsland",
    "endogenousRetireDecisnYrs", "endogenousRetireYrs", "validTGC", "mapAggR_r", "maps_r"
  ))
  
  paramsForGDX <- list.subset(params, c(
    "numT", "yearNum", "i_nameplate", "i_baseload", "i_largestGenerator", "i_refurbDecisionYear", 
    "i_fixedOM", "i_inflexiblePlantFactor", "i_fof", "i_heatrate", "i_maxNrgByFuel",
    "i_PumpedHydroEffic", "i_PumpedHydroMonth", "i_UnitLargestProp", "bigM", "ensembleFactor",
    "continueAftaEndogRetire", "initialCapacity", "hoursPerBlock",  
    "maxCapFactPlant", "i_HVDCshr", "i_HVDClevy", "capCharge", "refurbCapCharge", 
    "exogMWretired", "minCapFactPlant", "i_minUtilisation", "i_fuelQuantities", "i_reserveReqMW",
    "i_renewCapShare", "i_SIACrisk", "i_fkSI", "i_HVDClossesAtMaxXfer", "i_fkNI",
    "PVfacG", "PVfacT", "peakConPlant", "i_winterCapacityMargin", "NWpeakConPlant", "i_renewNrgShare",
    "i_distdGenRenew", "i_distdGenFossil", "reservesCapability",
    "singleReservesReqF", "penaltyViolateReserves", "i_plantReservesCost", "i_offlineReserve", "windCoverPropn",
    "i_txCapacity", "txEarlyComYr", "txFixedComYr", "i_txCapacityPO", "i_maxReservesTrnsfr",
    "pNFresvCost", "freeReserves", "pNFresvCap", "bigLoss", "lossIntercept", "lossSlopeMIP", "lossSlopeRMIP",
    "txCapCharge", "susceptanceYr", "i_txGrpConstraintsLHS", "i_txGrpConstraintsRHS", "BBincidence",
    "i_fuelPrices", "i_varFuelCosts", "i_co2tax", "i_emissionFactors", "i_varOM", "i_NrgDemand", "i_P200ratioNI",
    "i_P200ratioNZ", "i_historicalHydroOutput"
  ))
  
  # Check for NA symbols in lists
  if(any(is.na(list.names(setsForGDX))))    stop("NA symbol found in 'sets' list.")
  if(any(is.na(list.names(subsetsForGDX)))) stop("NA symbol found in 'subsets' list.")
  if(any(is.na(list.names(paramsForGDX))))  stop("NA symbol found in 'params' list.")
  
  # Change all columns in sets and subsets dataframes to character
  setsForGDX <- lapply(setsForGDX, function(x) x %>% mutate_all(as.character))
  subsetsForGDX <- lapply(subsetsForGDX, function(x) x %>% mutate_all(as.character))
 
  
  # Write to GDX. Note the filtering of sets, subsets and parameters.
  write2.gdx(
    
    "Data/GEMsolveInput/GDX/GEMdata.gdx"
    
    , params = paramsForGDX
    
    , sets = list.merge(
      setsForGDX
      , subsetsForGDX
    )
    
  )
  
}