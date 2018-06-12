### Note: Not currently used. This code was originally part of the code to generate the
### GEM demand GDX file. It was decided to leave the conversion of TPs out.


# Identify the first Sunday in April (i.e. where TP = 50)
# and last Sunday in Sept (i.e. where TP = 46)

TP_count_by_day <- demand %>%
  group_by(y, mn, d, poc) %>%
  count() %>%
  ungroup() %>%
  select(-poc) %>%
  distinct()

first_sunday_april <- TP_count_by_day %>%
  filter(n == 50)

last_sunday_sept <- TP_count_by_day %>%
  filter(n == 46)

# Modify TPs for daylight saving
demand_ds <- demand %>%
  mutate(
    tp = case_when(
      
      #Change TPs for April
      mn == 4 & d == first_sunday_april$d & tp == 5 ~ 4.5
      ,mn == 4 & d == first_sunday_april$d & tp == 6 ~ 5
      ,mn == 4 & d == first_sunday_april$d & tp == 7 ~ 5.5
      ,mn == 4 & d == first_sunday_april$d & tp >= 8 ~ tp - 2
      
      #Change TPs for September
      ,mn == 9 & d == last_sunday_sept$d & tp >= 5 ~ tp + 2
      
      #Everything else remains the same
      ,TRUE ~ as.numeric(tp)
    )
  )

# Check that the max TP value is now 48
if(max(demand_ds$tp) > 48) {
  warning("The max TP value is greater than 48. Double-check the daylight savings adjustment.")
}