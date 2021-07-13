## ---------------------------
##
## Script name: time summaries
##
## Purpose of script: grouping by year and month
##
## Author: Jack Holbrook (USGS)
##
## ---------------------------
## Notes:
##     ~
## ---------------------------


evaluate_annual <- function(in_file) 
{
  # these are "raw" data not reduced to a single obs per seg-day
  dat <- readRDS(in_file)
  # now we can summarize by segment ID, which is the unit we should be measuring data availability
  # so, for example, we can calculate how many unique days of measurement a site has per year:
  annual <- dat %>%
    # make new year column for grouping
    mutate(year = lubridate::year(date)) %>%
    group_by(seg_id_nat, year) %>%
    drop_na(seg_id_nat) %>%
    # group by and summarize reduced data frame down to one row per seg_id_nat-year combo
    # we count unique dates because some stream segments will have multiple sites, and 
    # we don't want to double count since we'll have to evaluate a single value per day
    summarize(n_per_year = length(unique(date))) %>%
    group_by(seg_id_nat) %>%
    # group by and mutate will still do evaluation by group, but won't reduce to one value per seg_id_nat
    # this will produce repeated values per seg_id_nat
    mutate(n_all_time = sum(n_per_year)) %>% ungroup()
  # returns dataframe
  return(annual)
}


evaluate_monthly <- function(in_file)
{
  dat <- readRDS(in_file)
  monthly <- dat %>% 
    mutate(month_year = lubridate::floor_date(date, "month")) %>% #lapply(date, (format(as.Date(date), "%Y-%m")))
    #mutate(month_year = lubridate::ym(date)) %>% 
    group_by(seg_id_nat, month_year) %>% 
    drop_na(seg_id_nat) %>% 
    summarize(n_per_month = length(unique(date))) %>%
    group_by(seg_id_nat) %>% 
    ungroup()
  monthly$month_year <- format(as.POSIXct(monthly$month_year,format='%m/%d/%Y %H:%M:%S'),format='%m/%Y')
  
  # readr::write_csv(monthly, 'out/monthly_Table.csv')
  # return('out/monthly_Table.csv')
  
  return(monthly) 
}


clean_monthly <- function(in_file)
{
  dat <- readRDS(in_file)
  dat <- dat %>% 
    drop_na(seg_id_nat) %>% 
    mutate(month_year = lubridate::floor_date(date, "month"))
  dat$month_year <- format(as.POSIXct(dat$month_year,
                    format='%m/%d/%Y %H:%M:%S'),format='%m/%Y')
  # add the count per month, this will be used to examine only months with full records
  dat <- dat %>%
    group_by(seg_id_nat, month_year) %>% 
    mutate(n_per_month = n_distinct(date)) %>% 
    ungroup()
  
  # readr::write_rds(dat, 'out/data_with_months.rds')
  # return('out/data_with_months.rds')
  
  # returns dataframe
  return(dat)
}

add_days_in_month <- function(in_data)
{
  dat <- in_data %>%
    mutate(month_year = as.Date(date)) %>% 
    mutate(days_in_month = lubridate::days_in_month(date)) %>% 
    mutate(meets_criteria = n_per_month >= (days_in_month)) %>%
    filter(meets_criteria)
    
  # readr::write_rds(dat, 'out/data_with_months.rds')
  # return('out/data_with_months.rds')
    
  return(dat)
}