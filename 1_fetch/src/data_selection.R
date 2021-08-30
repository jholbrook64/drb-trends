## ---------------------------
##
## Script name: data_map.R
##
## Purpose of script: selecting and filtering data from original data-set.
##
## Author: Jack Holbrook (USGS)
##
## ---------------------------
## Notes:
##     ~
## ---------------------------

clean_monthly <- function(in_file)
{
  # keep this one unchanged since daily values need to retained for the summaries
  
  # all seg_id_nat were changed to site_id
  dat <- readRDS(in_file)
  dat <- dat %>% 
    drop_na(site_id) %>% 
    mutate(month_year = lubridate::floor_date(date, "month"))
  dat$month_year <- format(as.POSIXct(dat$month_year,
                                      format='%m/%d/%Y %H:%M:%S'),format='%m/%Y')
  # add the count per month, this will be used to examine only months with full records
  dat <- dat %>%
    group_by(site_id, month_year) %>% 
    mutate(n_per_month = n_distinct(date)) #%>% 
    #ungroup()
  
  # filters criteria needed for having so many days in a month. 
  dat <- dat %>%
    mutate(days_in_month = lubridate::days_in_month(date)) %>% 
    mutate(month = lubridate::month(date)) %>% 
    mutate(meets_criteria = n_per_month >= (days_in_month-2)) %>%
    filter(meets_criteria) %>% 
    group_by(site_id, month) %>% 
    mutate(n_year = n_distinct(month_year)) %>% 
    #mutate(n_year = floor(n_distinct(month_year)/12)) %>% # remove floor() if it gives problems
    ungroup() %>% 
    filter(n_year >= 15) 
  
  return(dat)
}

filter_data <- function(clean_monthly)
{
  select_data <- clean_monthly %>% 
    group_by(site_id) %>% 
    mutate(start_date_id = min(date)) %>% 
    mutate(end_date_id = max(date)) %>% 
    #filter(start_date_id <= as.Date("1995-01-01")) %>%  # this is taken away to look at data temporal distribution 8/30/21
    #filter(end_date_id >= as.Date("2010-01-01")) %>% 
    filter(end_date_id >= as.Date("2020-12-31")) %>% 
    ungroup()
  
  return(select_data)
}

group_time <- function(select_data)
{
  browser()
  
  data_for_trend_analysis <- select_data %>% 
    mutate(year = lubridate::year(date)) %>% 
    group_by(month, year, site_id) %>% 
    summarize(month_mean = mean(mean_temp_degC, na.rm = TRUE),
              month_meanOfMax = mean(max_temp_degC , na.rm = TRUE),
              month_meanOfMin = mean(min_temp_degC, na.rm = TRUE))
    # summarize(month_meanOfMax = mean(max_temp_degC , na.rm = TRUE)) %>%
    # summarize(month_meanOfMin = mean(min_temp_degC, na.rm = TRUE))
    
    # data_for_trend_analysis <- select_data %>% 
    # mutate(year = lubridate::year(date)) %>% 
    # group_by(month, year, site_id) %>% 
    # summarize(month_meanOfMax = mean(max_temp_degC , na.rm = TRUE))
    
    # mutate(month_mean = mean(mean_temp_degC, na.rm = TRUE)) %>%
    # mutate(month_meanOfMax = mean(max_temp_degC, na.rm = TRUE)) %>%
    # mutate(month_meanOfMin = mean(min_temp_degC, na.rm = TRUE)) %>% 
    # group_by(month, site_id)
  
    # mutate(month_mean = mean(mean_temp_c, na.rm = TRUE)) %>%
    # mutate(month_meanOfMax = mean(max_temp_c, na.rm = TRUE)) %>%
    # mutate(month_meanOfMin = mean(min_temp_c, na.rm = TRUE))
  
  return(data_for_trend_analysis)
}

group_year <- function(select_data)
{
  year_trend_analysis <- select_data %>% 
    mutate(year = lubridate::year(date)) %>%
    group_by(year) %>% 
    mutate(annual_mean = mean(mean_temp_degC, na.rm = TRUE)) %>%
    mutate(annual_meanOfMax = mean(max_temp_degC, na.rm = TRUE)) %>%
    mutate(annual_meanOfMin = mean(min_temp_degC, na.rm = TRUE)) %>% 
    drop_na(annual_mean, annual_meanOfMax, annual_meanOfMin)
  
  return(year_trend_analysis)
}

tile_plot_func <- function(data_for_trend_analysis)
{
  browser()
  
  count_sites <-  
    data_for_trend_analysis %>% 
      group_by(month, site_id) %>% 
      mutate(instance = 1) %>% 
      ungroup()
  
  data_for_trend_analysis$month_mean <- as.numeric(data_for_trend_analysis$month_mean)
  
  tile_p <- ggplot(count_sites, aes(x = year, y = site_id)) +
              geom_tile(mapping = aes(fill = sum(instance)))
  #                    list(geom_tile(mapping = aes(fill = count(instance))),#count(data_for_trend_analysis, vars = year, month))) +
  #                              scale_fill_distiller(palette = "YlGnBu"),
  #                              labs(title = "count of records",
  #                                   x = "year",
  #                                   y = "month")))
    
    this_filename <- file.path('1_fetch', 'out', 'tile_plot_monthly_.png')
    ggsave(filename = this_filename, boxp, height = 7, width = 12)
    return(this_filename)
}
