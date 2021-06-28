# starter functions for reading in data

evaluate_annual <- function(in_file) {
  
  # these are "raw" data not reduced to a single obs 
  # per seg-day
  dat <- readRDS(in_file)
  
  # now we can summarize by segment ID, which is the unit we should be measuring data availability
  # so, for example, we can calculate how many unique days of measurement a site has per year:
  annual <- dat %>%
    # make new year column for grouping
    mutate(year = lubridate::year(date)) %>%
    group_by(seg_id_nat, year) %>%
    # group by and summarize reduced data frame down to one row per seg_id_nat-year combo
    # we count unique dates because some stream segments will have multiple sites, and 
    # we don't want to double count since we'll have to evaluate a single value per day
    summarize(n_per_year = length(unique(date))) %>%
    group_by(seg_id_nat) %>%
    # group by and mutate will still do evaluation by group, but won't reduce to one value per seg_id_nat
    # this will produce repeated values per seg_id_nat
    mutate(n_all_time = sum(n_per_year)) %>% ungroup()
  
  return(annual)
  
  
}