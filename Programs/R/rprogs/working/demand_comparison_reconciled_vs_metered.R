#NOTE: need to run 'generate_GDX' code first

demand_meter <- sqlQuery(
  channel = channel,
  query = paste0(
    "
    select 
        year(Trading_date) as y
      , month(Trading_date) as mn
      , day(Trading_date) as d
      , Period as tp
      , DIM_DTTM as dttm
      , POC as poc
      , sum(Value) / 1000 as MWh
    from 
      [Wholesale_MD].[atomic].[Metering_data] 
    where 
      year(Trading_date) = ",
    
    demand_year,
    
    " and Flow_direction = 'X' 
    group by 
      year(Trading_date)
      , month(Trading_date)
      , day(Trading_date)
      , Period
      , DIM_DTTM
      , POC
    "
  )
) %>% 
  as_tibble() %>%
  arrange(poc, y, mn, d, tp)

compare_demand <- demand %>%
  left_join(
    demand_meter %>% 
      select(-dttm) %>% 
      rename(MWh_meter = MWh),
    by = c("poc", "y", "mn", "d", "tp")
  ) %>%
  mutate(diff = MWh - MWh_meter)

biggest_diff <- compare_demand %>%
  group_by(poc) %>%
  summarise(max_diff = max(diff)) %>%
  top_n(12, max_diff)

compare_demand %>%
  filter(poc %in% biggest_diff$poc) %>%
  ggplot(aes(ymd_hm(dttm), diff)) + 
  geom_line() +
  geom_smooth(se = FALSE) +
  facet_wrap(~poc)

demand_meter %>%
  filter(poc %in% c("BEN2201", "HAY2201", "TPN00NI", "TPN00SI")) %>%
  ggplot(aes(ymd_hm(dttm), MWh)) +
  geom_line() +
  facet_wrap(~poc)

compare_demand %>%
  filter(poc == "GLN0332") %>%
  ggplot(aes(ymd_hm(dttm), MWh)) +
  geom_line() +
  geom_line(aes(y = MWh_meter), colour = "red")

