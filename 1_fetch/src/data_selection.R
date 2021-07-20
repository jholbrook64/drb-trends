# data selectionn script

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
  
  # filters criteria needed for having so many days in a month. 
  dat <- dat %>%
    mutate(month_year = as.Date(date)) %>% 
    mutate(days_in_month = lubridate::days_in_month(date)) %>% 
    mutate(meets_criteria = n_per_month >= (days_in_month-2)) %>%
    filter(meets_criteria)
  
  
  dat <-  dat %>% 
    mutate(n_years = length(unique(month_year))) %>% 
    filter(n_years < 12)
  # readr::write_rds(dat, 'out/data_with_months.rds')
  # return('out/data_with_months.rds')
  
  # returns dataframe
  return(dat)
}

group_time <- function(clean_monthly)  # this will be the data preparing function!
{
  data_for_trend_analysis <- clean_monthly %>% 
    group_by(seg_id_nat) %>% 
    mutate(start_date_id = min(date)) %>% 
    mutate(end_date_id = max(date)) %>% 
    filter(start_date_id <= as.Date("1995-01-01")) %>%
    filter(end_date_id >= as.Date("2010-01-01")) %>% 
    ungroup()
  # this will add mean values for each month from highest,
  # lowest and mean recorded daily temperatures
  data_for_trend_analysis <- data_for_trend_analysis %>% 
    mutate(month = lubridate::month(date)) %>% 
    mutate(year = lubridate::year(date)) %>% 
    group_by(month, year) %>% 
    mutate(month_mean = mean(mean_temp_c, na.rm = TRUE)) %>%
    mutate(month_meanOfMax = mean(max_temp_c, na.rm = TRUE)) %>%
    mutate(month_meanOfMin = mean(min_temp_c, na.rm = TRUE))
  
  return(data_for_trend_analysis)
}