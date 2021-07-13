## ---------------------------
##
## Script name: site_selection_code
##
## Purpose of script: selecting sites
##
## Author: Jack Holbrook (USGS)
##
## ---------------------------
## Notes:
##     ~
## ---------------------------

meta_data <- function(annual) # this will be the function that does an eda of the data here:
{
  minimum_sampled_days <-  121 # four months
  table <- annual %>% group_by(seg_id_nat) 
  table <- subset(table, table[, 3] > minimum_sampled_days)
  table <- table %>% group_by(seg_id_nat) %>% summarise(
    n_per_year = mean(n_per_year),
    n_all_time = mean(n_all_time)
  )
  readr::write_csv(table, 'out/groupedTable.csv')
  return('out/groupedTable.csv')
}

# this one will be useful for plotting later on 
split_df <- function(in_file)
{
  dat <- readRDS(in_file)
  split_data <- split(dat, f= annual$seg_id_nat) #split(annual, seg_id_nat)
  split_data <- lapply(split_data, get_dateRange)
  # site_summary <- vector()
  # for (site_id in split_data) 
  # {
  #   # can call the group time function here as well
  #   #for each segment, create a list the holds segment id, date at head, date at tail
  #   new_vect <- split_data[site_id, seg_id_nat]   
  # }
  # readr::write_csv(table, 'out/splitData.csv')
  # return('out/splitData.csv')
  print(split_data)  # printing gives the correct number of tibbles. (1 per group) 
  return(split_data)
}

group_time <- function(add_days)  # this will be the data preparing function!
{                                 # this si the same data as env var "data with months"
  time_series_plots <- add_days %>% 
    group_by(seg_id_nat) %>% 
    mutate(start_date_id = min(date)) %>% 
    mutate(end_date_id = max(date)) %>% 
    filter(start_date_id <= as.Date("1995-01-01")) %>%
    filter(end_date_id >= as.Date("2010-01-01")) %>% 
    ungroup()
  return(time_series_plots)
}

monthly_trend_analysis <- function(time_series_plots)
{
  # summarize(month_mean = mean(mean_temp_c),
  #           month_meanOfMax = mean(max_temp_c),
  #           month_meanOfMin = mean(min_temp_c)) %>% 
  
  add_month <- time_series_plots %>% 
    mutate(month = lubridate::month(date)) %>% 
    mutate(year = lubridate::year(date)) %>% 
    group_by(month, year) %>% 
    mutate(month_mean = mean(mean_temp_c, na.rm = TRUE)) %>%
    mutate(month_meanOfMax = mean(max_temp_c, na.rm = TRUE)) %>%
    mutate(month_meanOfMin = mean(min_temp_c, na.rm = TRUE))#%>%
    #ungroup()
  
  print(add_month)
  return(add_month)  # this is taken in as the parameter, regressionData in subsequent functions. 
}

regression_for_each_site_monthly <- function(regressionData, fileout)
{
  # take return from monthly_trend_analysis
  
  #this is still WIP
  lr_list = list()
  for (i in unique(regressionData$month)) 
  {
    sites <- regressionData %>% 
      group_by(month)
  }
  readr::write_csv(sites, fileout)
  return('2_process/out/DRB_site_regress_Values.csv')
}


DRB_regression_monthly <-  function(regressionData, fileout)
{
  months = c("January", "February", "March", "April", "May", "June", "July", "August", 
             "September", "October", "November", "December")
  # take return from monthly_trend_analysis
  lr_list = list()
  for (i in unique(regressionData$month)) 
  {
    unique_month <- regressionData %>% 
      filter(month == i)  
    lr <- lm(month_mean ~ year, data = unique_month)
    lr_list[[i]] = lr
  }
  pdf(fileout)
  for (i in unique(regressionData$month)) 
    {
    print(lr_list[[i]])
    unique_month <- regressionData %>% 
      filter(month == i) 
    print(plot(lr_list[[i]]))  #difficult to interpret
    #print(plot(unique_month$year, unique_month$month_mean, main = months[i],
    #           xlab = "year", ylab = "mean temperature on the month in celsius"))
    #print(lines(predict(lr), col = 'green'))
    }
  return(fileout)
  # readr::write_lines(lr_list, fileout)
  # return('2_process/out/DRB_regress_Values.txt')
}

write_up <- function(annual)
{
  # get number of unique segment id's.
  no_unique <- sapply(annual, function(x) length(unique(x))) # the n_all_time will be 
  print(no_unique)  # will only print once, for reference, below is the return:
  
  #seg_id_nat year n_per_year n_all_time 
  #332         85        343        245
  return(no_unique)
}
