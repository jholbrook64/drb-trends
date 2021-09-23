## ---------------------------
##
## Script name: data_selection.R
##
## Purpose of script: selecting and filtering data from original data-set.
##
## Author: Jack Holbrook (USGS)
##
## ---------------------------
## Notes:
##     ~
## ---------------------------

filter_monthly_data <- function(in_file){

  # all seg_id_nat were changed to site_id
  dat <- readRDS(in_file)
  dat <- dat %>%
    drop_na(site_id) %>%
    mutate(month_year = lubridate::floor_date(date, "month"))
  dat$month_year <- format(as.POSIXct(dat$month_year,
                                      format='%m/%d/%Y %H:%M:%S'),format='%m/%Y')
  # filter out months that have more than two missing days of observations
  # make sure sites have at least 15 years of data
  dat <- dat %>%
    group_by(site_id, month_year) %>%
    mutate(n_per_month = n_distinct(date)) %>%
    mutate(days_in_month = lubridate::days_in_month(date)) %>%
    mutate(month = lubridate::month(date)) %>%
    mutate(month_criteria = n_per_month >= (days_in_month-2)) %>%
    filter(month_criteria) %>%
    group_by(site_id, month) %>%
    mutate(n_year = n_distinct(month_year)) %>%
    #mutate(n_year = floor(n_distinct(month_year)/12)) %>% # remove floor() if it gives problems
    ungroup() %>%
    filter(n_year >= 15)

  # now look for sites that have at least 15 years of data,
  # with no more than a 3 year gap (so >=4)
  year_dat <- ungroup(dat) %>%
    mutate(year = lubridate::year(date)) %>%
    select(site_id, month, year) %>%
    distinct() %>%
    arrange(site_id, month, year) %>%
    group_by(site_id, month) %>%
    mutate(year_diff = c(NA, diff(year))) %>% ungroup()

  # Let's exclude data after big gaps (>10 years)
  # and if a site still has >15 years of data, let's just drop
  # the most recent data

  # first, keep sites with no gaps >4
  no_gaps_keep <- group_by(year_dat, site_id, month) %>%
    filter(max(year_diff, na.rm = TRUE) <= 4) %>%
    select(site_id, month, year) %>%
    distinct() %>%
    mutate(series = 'whole')

  # next, check for data around gaps
  # if a site-month is missing 4-10 years,
  # keep if there are >10 years before/after the gap
  gaps_keep <- group_by(year_dat, site_id, month) %>%
    filter(max(year_diff, na.rm = TRUE) > 4 & max(year_diff, na.rm = TRUE) <= 10) %>%
    filter((sum(year >= year[which.max(year_diff)]) >= 10) & (sum(year < year[which.max(year_diff)]) >= 10)) %>% ungroup() %>%
    mutate(series = 'whole')

  # next, for sites that have >10 year gap
  # break the data up into two chunks
  # if there are >15 years of data, analyze the chunks seperately
  big_gaps_test <- group_by(year_dat, site_id, month) %>%
    filter(max(year_diff, na.rm = TRUE) > 10) %>%
    mutate(series = if_else(year < year[which.max(year_diff)], 'early', 'late')) %>%
    group_by(site_id, series, month) %>%
    mutate(n_diff2 = c(NA, diff(year))) %>%
    mutate(n_per_series = n(),
           max_gap_series = max(n_diff2, na.rm = TRUE)) %>%
    ungroup()

  # keep those that meet our 15 year & gap criteria
  big_gaps_keep1 <- big_gaps_test %>%
    filter(n_per_series >= 15 & max_gap_series <= 4)
  # keep those that meet 5-10 year gap + 10 years after gap criteria
  big_gaps_keep2 <- group_by(big_gaps_test, site_id, series, month) %>%
    filter(n_per_series >= 15 & max_gap_series > 4 & max_gap_series <= 10) %>%
    filter((sum(year >= year[which.max(n_diff2)]) >= 10) & (sum(year < year[which.max(n_diff2)]) >= 10)) %>% ungroup()
  # turns out no sites meet this criteria, so we can drop this test in the future

  # now split series again and do the same tests over
  # do the same thing again, split series and see if series meet criteria
  big_gaps_test2 <- big_gaps_test %>%
    filter(n_per_series >= 15 & max_gap_series > 4) %>%
    group_by(site_id, series, month) %>%
    mutate(series = if_else(year < year[which.max(n_diff2)], paste0(series, '1'), paste0(series, '2'))) %>%
    mutate(n_diff3 = c(NA, diff(year)),
           n_per_series = n(),
           max_gap_series = max(n_diff3, na.rm = TRUE)) %>%
    ungroup()

  big_gaps_keep3 <- big_gaps_test2 %>%
    filter(n_per_series >= 15 & max_gap_series <= 4)

  # are there any left that can be split more?
  # this would be
  big_gaps_test3 <- big_gaps_test2 %>%
    filter(n_per_series >= 15 & max_gap_series > 4) %>%
    group_by(site_id, series, month) %>%
    mutate(series = if_else(year < year[which.max(n_diff3)], paste0(series, '1'), paste0(series, '2'))) %>%
    mutate(n_diff4 = c(NA, diff(year)),
           n_per_series = n(),
           max_gap_series = max(n_diff4, na.rm = TRUE)) %>%
    ungroup()

  big_gaps_keep4 <- big_gaps_test3 %>%
    filter(n_per_series >= 15 & max_gap_series <= 4)

  # no more potential keeps
  # let's bind all the keeps together
  keeps <- bind_rows(no_gaps_keep, gaps_keep, big_gaps_keep1, big_gaps_keep2, big_gaps_keep3, big_gaps_keep4) %>%
    ungroup() %>%
    select(site_id, month, year, series) %>%
    mutate(series_id = paste(site_id, series, sep = '_')) %>%
    distinct()

  dat_out <- dat %>%
    mutate(year = lubridate::year(date)) %>%
    left_join(keeps) %>%
    filter(!is.na(series_id))

  return(dat_out)
}
filter_annual_data <- function(in_file){
  # all seg_id_nat were changed to site_id
  dat <- readRDS(in_file)
  dat <- dat %>%
    drop_na(site_id) %>%
    drop_na(mean_temp_degC) %>%
    mutate(year = lubridate::year(date),
           month = lubridate::month(date))


  # filter out months that have more than two missing days of observations
  # make sure sites have at least 15 years of data
  dat <- dat %>%
    group_by(site_id, year, month) %>%
    mutate(n_per_month = n_distinct(date)) %>%
    mutate(days_in_month = lubridate::days_in_month(date)) %>%
    filter(n_per_month >= (days_in_month-4)) %>% ungroup() %>%
    group_by(site_id, year) %>%
    filter(n_distinct(month) == 12) %>% ungroup() %>%
    group_by(site_id) %>%
    mutate(n_year = n_distinct(year)) %>%
    ungroup() %>%
    filter(n_year >= 15)

  # now look for sites that have at least 15 years of data,
  # with no more than a 3 year gap (so >=4)
  year_dat <- ungroup(dat) %>%
    select(site_id, year) %>%
    distinct() %>%
    arrange(site_id, year) %>%
    group_by(site_id) %>%
    mutate(year_diff = c(NA, diff(year))) %>% ungroup()

  # Let's exclude data after big gaps (>10 years)
  # and if a site still has >15 years of data, let's just drop
  # the most recent data

  # first, keep sites with no gaps >4
  no_gaps_keep <- group_by(year_dat, site_id) %>%
    filter(max(year_diff, na.rm = TRUE) <= 4) %>%
    select(site_id, year) %>%
    distinct() %>%
    mutate(series = 'whole')

  # next, check for data around gaps
  # if a site-month is missing 4-10 years,
  # keep if there are >10 years before/after the gap
  gaps_keep <- group_by(year_dat, site_id) %>%
    filter(max(year_diff, na.rm = TRUE) > 4 & max(year_diff, na.rm = TRUE) <= 10) %>%
    filter((sum(year >= year[which.max(year_diff)]) >= 10) & (sum(year < year[which.max(year_diff)]) >= 10)) %>% ungroup() %>%
    mutate(series = 'whole')

  # next, for sites that have >10 year gap
  # break the data up into two chunks
  # if there are >15 years of data, analyze the chunks seperately
  big_gaps_test <- group_by(year_dat, site_id) %>%
    filter(max(year_diff, na.rm = TRUE) > 10) %>%
    mutate(series = if_else(year < year[which.max(year_diff)], 'early', 'late')) %>%
    group_by(site_id, series) %>%
    mutate(n_diff2 = c(NA, diff(year))) %>%
    mutate(n_per_series = n(),
           max_gap_series = max(n_diff2, na.rm = TRUE)) %>%
    ungroup()

  # keep those that meet our 15 year & gap criteria
  big_gaps_keep1 <- big_gaps_test %>%
    filter(n_per_series >= 15 & max_gap_series <= 4)

  # turns out no sites meet this criteria, so we can drop this test in the future and stop looking for splits

  # no more potential keeps
  # let's bind all the keeps together
  keeps <- bind_rows(no_gaps_keep, gaps_keep, big_gaps_keep1) %>%
    ungroup() %>%
    select(site_id, year, series) %>%
    mutate(series_id = paste(site_id, series, sep = '_')) %>%
    distinct()

  dat_out <- dat %>%
    left_join(keeps) %>%
    filter(!is.na(series_id))

  return(dat_out)
}

filter_data <- function(clean_monthly)
{
  select_data <- clean_monthly %>%
    group_by(site_id) %>%
    mutate(start_date_id = min(date)) %>%
    mutate(end_date_id = max(date)) %>%
    filter(start_date_id <= as.Date("1990-01-01")) %>%  # this is taken away to look at data temporal distribution 8/30/21
    #filter(end_date_id >= as.Date("2010-01-01")) %>%
    filter(end_date_id >= as.Date("2020-12-31")) %>%
    ungroup()

  return(select_data)
}

group_time <- function(select_data)
{
  data_for_trend_analysis <- select_data %>%
    mutate(year = lubridate::year(date)) %>%
    group_by(site_id, series, series_id, year, month) %>%
    summarize(month_mean = mean(mean_temp_degC, na.rm = TRUE),
              month_meanOfMax = mean(max_temp_degC , na.rm = TRUE),
              month_meanOfMin = mean(min_temp_degC, na.rm = TRUE))

  return(data_for_trend_analysis)
}

# function to remove time lags of greater than 3 years
remove_temporal_disconnect <- function(data_for_trend_analysis)
{
  obs_date <- lubridate::date(data_for_trend_analysis$date)

  # use dplyr::mutate to clean data if tempiral disconnect 
  data_removed_lags <- data_for_trend_analysis %>% 
    mutate(year_step = year - lag(year)) %>% 
    filter(year_step > 3)
    # this takes in either month or year data? should be able to do both
  return(data_removed_lags)
}

group_year <- function(select_data)
{

  year_trend_analysis <- select_data %>%
    group_by(site_id, series, year) %>%
    summarize(annual_mean = mean(mean_temp_degC, na.rm = TRUE),
              annual_meanOfMax = mean(max_temp_degC, na.rm = TRUE),
              annual_meanOfMin = mean(min_temp_degC, na.rm = TRUE),
              month = NA) %>%
    drop_na(annual_mean, annual_meanOfMax, annual_meanOfMin)

  return(year_trend_analysis)
}

clean_monthly_mean <- function(all_data)
{
  noNA_data <- all_data %>%
    drop_na(month_mean)
  return(noNA_data)
}

tile_plot_func <- function(data_for_trend_analysis)
{

  # select_data <- data_for_trend_analysis %>%
  #   group_by(site_id) %>%
  #   mutate(start_date_id = min(date)) %>%
  #   mutate(end_date_id = max(date)) %>%
  #   filter(start_date_id <= as.Date("1990-01-01")) %>%  # this is taken away to look at data temporal distribution 8/30/21
  #   #filter(end_date_id >= as.Date("2010-01-01")) %>%
  #   filter(end_date_id >= as.Date("2020-12-31")) %>%
  #   ungroup()

  data_for_trend_analysis <-
    data_for_trend_analysis %>%
    group_by(month, site_id) %>%
    mutate(instance = 1) %>%
    ungroup()

  count_sites <-
    data_for_trend_analysis %>%
      group_by(month, site_id) %>%
      filter(nyear > 27) %>%
      #mutate(sparse = sum(filter(nyear >22))) %>%
      mutate(instance = 1) %>%
      ungroup()

  #code sam provided:
  all_sites <- data_for_trend_analysis %>%  mutate(selected = site_id %in% unique(count_sites$site_id))

  #data_for_trend_analysis$month_mean <- as.numeric(data_for_trend_analysis$month_mean)

  tile_p <- ggplot(all_sites, aes(x = year, y = site_id)) +
              geom_tile(mapping = aes(fill = selected))


  #                    list(geom_tile(mapping = aes(fill = count(instance))),#count(data_for_trend_analysis, vars = year, month))) +
  #                              scale_fill_distiller(palette = "YlGnBu"),
  #                              labs(title = "count of records",
  #                                   x = "year",
  #                                   y = "month")))

    this_filename <- file.path('1_fetch', 'out', 'tile_plot_monthly_.png')
    ggsave(filename = this_filename, tile_p, height = 7, width = 12)
    return(this_filename)
}
