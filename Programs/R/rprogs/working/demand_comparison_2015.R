# Checking the similarities/differences between the new 2015 demand file (demand_2015.csv)
# and the older version (hhByPOC2015.csv)

library(tidyverse)

# Read in CSVs 
new_demand_2015 <- read_csv("Programs/R/output/demand_2015.csv")

old_demand_2015 <- read_tsv("Programs/R/output/hhByPOC2015.csv", col_names = FALSE, skip = 1)
colnames(old_demand_2015) <- c("y", "mn", "d", "tp", "poc",  "MWh")

# Aggregate to day so that we can join without worrying about daylight savings modifications

new_demand_2015_tidy <- new_demand_2015 %>%
  group_by(poc, y, mn, d) %>% 
  summarise(MWh = sum(MWh))

old_demand_2015_tidy <- old_demand_2015 %>%
  group_by(poc, y, mn, d) %>% 
  summarise(MWh = sum(MWh))

# Comparison
compare <- new_demand_2015_tidy %>%
  rename(MWh_new = MWh) %>% 
  full_join(
    old_demand_2015_tidy %>% 
      rename(MWh_old = MWh)
    , by = c("poc", "y", "mn", "d")
  )

## Where new demand amount is NA
compare %>% filter(is.na(MWh_new)) #None

## Where old demand amount is NA
compare %>% filter(is.na(MWh_old)) #10,179, however...

compare %>% filter(is.na(MWh_old), MWh_new > 0) #...mostly where new amount is 0. Only two records where not the case:
# poc       y      mn     d MWh_new   MWh_old
# WTK0331  2015     9    22  0.0120      NA
# WTK0331  2015     9    23  0.0120      NA

## Check difference where not NA

compare %>%
  filter(!is.na(MWh_old)) %>%
  mutate(
    diff = MWh_new - MWh_old
  ) %>%
  # filter(diff >= 1) # Seems to be mostly rounding differences (56 rows out of 70,435 with diff > 1)
  # filter(diff >= 5) # (36 rows out of 70,435 with diff > 5)
  filter(diff >= 10) %>% # (22 rows out of 70,435 with diff > 10)
  arrange(desc(diff))

# Anti join POCs from old and new to check overlap

## In new demand but not in old

(new_demand_2015 %>% 
  select(poc) %>% 
  distinct() %>% 
  anti_join(
    old_demand_2015 %>% 
      select(poc) %>% 
      distinct()
  , by = "poc"
  ))$poc

# 1 COB0661
# 2 KPO1101
# 3 MAN2201
# 4 ROX2201
# 5 WRK2201
#However, readings for these are all 0

## In old demand but not in new

old_demand_2015 %>% 
  select(poc) %>% 
  distinct() %>% 
  anti_join(
    new_demand_2015 %>% 
      select(poc) %>% 
      distinct()
    , by = "poc"
  ) # None


# Plot comparison total demand my month
compare %>%
  group_by(mn) %>%
  summarise(
    total_new = sum(MWh_new)
    ,total_old = sum(MWh_old, na.rm = TRUE)
  ) %>%
  gather(source, GWh, -mn) %>%
  ggplot(aes(mn, GWh, fill = source)) +
  geom_bar(stat = "identity", position = "dodge")

compare %>%
  group_by(poc) %>%
  summarise(
    total_new = sum(MWh_new)
    ,total_old = sum(MWh_old, na.rm = TRUE)
  ) %>%
  mutate(
    diff = (total_new - total_old)
    ,diff_pcnt = (total_new - total_old) / total_new
  ) %>%
  filter(diff >= 20) %>% 
  gather(source, GWh, -c(poc, diff, diff_pcnt)) %>%
  ggplot(aes(poc, GWh, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  coord_flip()
  
