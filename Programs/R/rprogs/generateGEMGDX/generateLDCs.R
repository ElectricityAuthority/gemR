###############################################
### Title: Generate LDCs                     ##
### Description: This code is used to        ##
### generate load duration curves (LDCs) for ##
### each POC.                                ##
### Date: 6 August 2018                      ##
###############################################

# Assign load to blocks
assign_blocks_9 <- function(x){
  
  # Define a 9-block LDC
  b1l <- 1    ## Low wind top block
  b1w <- 3    ## Windy top block
  b2l <- 12   ## Low wind second block
  b2w <- 36   ## Windy second block
  b3l <- 40   ## Low wind third block
  b3w <- 120  ## Windy third block
  # b4 <-     ## Fourth block (not assigned here. Gets the residual TPs after assigning all others)
  b5 <- 400   ## Fifth block (i.e. N - 560 to N - 160)
  b6 <- 160   ## Sixth/last block (i.e. the last 160 TPs)
  
  # Generate cumulative sums of each block. This is split into two parts.
  tmp <- cumsum(c(b1l, b1w, b2l, b2w, b3l, b3w)) 
  tmp2 <- rev(cumsum(c(length(x), -b6, -b5))) #Note: this goes backwards from the end and the vector is then reversed.
  tmp3 <- c(tmp, tmp2)
  
  # This is what is returned from this function and is ultimately
  # used in a 'mutate' function to assign ranked load to load blocks.
  case_when(
    x %in% 0:tmp3[1] ~ "b1l"
    ,x %in% (tmp3[1] + 1):tmp3[2] ~ "b1w"
    ,x %in% (tmp3[2] + 1):tmp3[3] ~ "b2l"
    ,x %in% (tmp3[3] + 1):tmp3[4] ~ "b2w"
    ,x %in% (tmp3[4] + 1):tmp3[5] ~ "b3l"
    ,x %in% (tmp3[5] + 1):tmp3[6] ~ "b3w"
    ,x %in% (tmp3[6] + 1):tmp3[7] ~ "b4"
    ,x %in% (tmp3[7] + 1):tmp3[8] ~ "b5"
    ,x %in% (tmp3[8] + 1):tmp3[9] ~ "b6"
  )
  
}

# Create LDCs by POC and month (or nationally (i.e. by month only)). Assign to load blocks.
generate_LDCs <- function(demand_data, byPOC = TRUE){
  
  # If byPOC is TRUE then group by POC and month. Else group only by month (i.e. national LDCs)
  if(byPOC == TRUE){
    
    grp_vars <- vars(poc, mn)
    arrange_vars <- vars(poc, mn, load_ranked)
    
  } else {
    
    grp_vars <- vars(mn)
    arrange_vars <- vars(mn, load_ranked)
    
  }
  
  demand_data %>%
    select(-dttm) %>%
    group_by_at(grp_vars) %>%
    mutate(
      
      # Rank load by POC and month (descending). Use "first" method to break ties.
      load_ranked = rank(desc(MWh), ties.method = "first")
      
      # Assign each record to a load block (by POC and month) using the assign function above.
      , lb = assign_blocks_9(load_ranked)
      
      # Create month variable from number
      , month = month(mn, label = TRUE, abbr = FALSE)
      
    ) %>%
    arrange_at(arrange_vars)
  
}

# Sum load by POC, month and load block (or at national level if byPOC is FALSE)
generate_LDCs_sum <- function(ldc_data, byPOC = TRUE){
  
  # If byPOC is TRUE then group by POC, month and load block. Else group only by month and load block (i.e. national LDCs)
  if(byPOC == TRUE){
    
    grp_vars <- vars(poc, mn, lb)
    
  } else {
    
    grp_vars <- vars(mn, lb)
    
  }
  
  ldc_data %>%
    group_by_at(grp_vars) %>%
    summarise(
      MWh = sum(MWh)
      , count_TPs = n()
    )
  
}
