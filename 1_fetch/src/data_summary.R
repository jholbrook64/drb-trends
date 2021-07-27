## ---------------------------
##
## Script name: data_map.R
##
## Purpose of script: mapping reaches $ their annual observations
##
## Author: Jack Holbrook (USGS)
##
## ---------------------------
## Notes:
##     ~ this is the file that runs linear regressions
## ---------------------------

DRB_monthly <- function(regressionData, fileout)
{
  months = c("January", "February", "March", "April", "May", "June", "July", "August", 
             "September", "October", "November", "December")
  # take return from monthly_trend_analysis
  lr_list = list()
  month_order <- c(1:12)
  #order dataframe by increasing months
  regressionData <- regressionData[match(month_order, regressionData$month),]
  for (i in unique(regressionData$month)) 
  {
    unique_month <- regressionData %>% 
      filter(month == i) %>% 
      drop_na(month_mean)
    lr <- lm(month_mean ~ year, data = unique_month)
    lr_list[[i]] = lr
  }

  pdf(fileout)
  for (i in unique(regressionData$month)) 
  {
    print(lr_list[[i]])
    unique_month <- regressionData %>% 
      filter(month == i) 
    #print(plot(lr_list[[i]]))  #difficult to interpret
    print(plot(unique_month$year, unique_month$month_mean, main = months[i],
              xlab = "year", ylab = "mean temperature on the month in celsius"))
    print(lines(predict(lr), col = 'green'))
  }
  return(fileout)
}

regress_site <- function(sites)
{
  lr <- lm(month_mean ~ year, data = sites)
  sum_lr <- summary(lr)
  # build a stats list
  stats <- c(sum_lr$coefficients[[1]],
             sum_lr$coefficients[[2]])
  #descrip_site <- sites %>% 
    #group_by(seg_id_nat) %>% 
    #mutate(date_range = lubridate::as.interval(start = date[[1]], date[[nrow(sites)]])) #%>% 
    #mutate(max_temp_observed = max(max_temp_c)) %>% 
    #mutate(min_temp_observed = min(min_temp_c)) 
    
    # ^^^  this needs to be ironed out tonight so i can present stuff to sam tomorrow  ^^^
  
  dfstats <- data.frame("seg_id_nat" = sites$seg_id_nat[[1]], 
                        "Month" = sites$month[[1]],
                        "max_temp_observed" = max(sites$max_temp_c, na.rm = TRUE),
                        "mean_monthly_temp" = mean(sites$mean_temp_c, na.rm = TRUE),
                        "min_temp_observed" = min(sites$min_temp_c, na.rm = TRUE),
                        "Date Range" = lubridate::as.interval(start = sites$date[[1]], sites$date[[nrow(sites)]]),
                        #"Intercept" = stats[1], 
                        "Slope" = stats[2] 
                        # "Date Range" = descrip_site$date_range 
                        # "Max Temp Observed" = descrip_site$max_temp_observed,
                        # "Min Temp Observed" = descrip_site$min_temp_observed
                        #"r-squared" = stats[3])
                        )
  return(dfstats)
}

flexible_linear_regression <- function(sites, type)
{
  #where sites is each branched target, and type is the type of linear regression that is be run 
  if (type == 1)
  {
    lr <- lm(month_mean ~ year, data = sites)
    sum_lr <- summary(lr)
  }
  else if (type == 2)
  {
    #2 will be the mean of max's. 
    lr <- lm(month_meanOfMax ~ year, data = sites)
    sum_lr <- summary(lr)
  }
  else if (type == 3)
  {
    lr <- lm(month_meanOfMin ~ year, data = sites)
    sum_lr <- summary(lr)
  }
  else if (type == 4)
  {
    lr <- lm(annual_mean ~ year, data = sites)
    sum_lr <- summary(lr)
  }
  stats <- c(sum_lr$coefficients[[1]],
             sum_lr$coefficients[[2]])
  
  dfstats <- data.frame("seg_id_nat" = sites$seg_id_nat[[1]], 
                        "Month" = sites$month[[1]],
                        "max_temp_observed" = max(sites$max_temp_c, na.rm = TRUE),
                        "mean_monthly_temp" = mean(sites$mean_temp_c, na.rm = TRUE),
                        "min_temp_observed" = min(sites$min_temp_c, na.rm = TRUE),
                        "Date Range" = lubridate::as.interval(start = sites$date[[1]], sites$date[[nrow(sites)]]),
                        "Slope" = stats[2] 
                        )
}

build_statistics <- function(sum_lr)
{
  # takes each branch summary and returns an vector in correct format
  stats <- c(sum_lr$coefficients[[1]],
             sum_lr$coefficients[[2]], .name_spec = "{outer}_{inner}")
  return(stats)
}

write_summary_positive <- function(dfstats)
{
#These will only include metrics from the month of July since that is the hottest month on record
  month_table <- dfstats %>% 
    filter(Month == 7) %>% 
    filter(Slope > 0.0)

  df_positive <-  data.frame(
   # "Positive values" = NULL,
    "median_temp" = median(month_table$mean_monthly_temp),
    "greatest_change" = max(month_table$Slope),
    "median_change" = median(month_table$Slope),
    "least_change" = min(month_table$Slope)
  )
  df_positive_transpose <- as.data.frame(t(as.matrix(df_positive)))
  # seg_id_vect <- c(which(median(month_table$mean_monthly_temp)), which(max(month_table$Slope)),
  #                  which(median(month_table$Slope)), which(min(month_table$Slope)))
  seg_id_vect <- c(match(median(month_table$mean_monthly_temp),month_table), match(max(month_table$Slope),month_table),
                   match(median(month_table$Slope),month_table), match(min(month_table$Slope),month_table))
  df_positive_transpose <- cbind(df_positive_transpose, seg_id_vect)
  return(df_positive_transpose)
}

write_summary_negative <- function(dfstats)
{
  month_table <- dfstats %>% 
    filter(Month == 7) %>% 
    filter(Slope < 0.0)
  
  df_negative <-  data.frame(
  #"Negative values" = NULL,
  "median_temp" = median(month_table$mean_monthly_temp),
  "greatest_change" = max(month_table$Slope),
  "median_change" = median(month_table$Slope), 
  "least_change" = min(month_table$Slope)
  )
  
  df_negative_transpose <- as.data.frame(t(as.matrix(df_negative)))
  seg_id_vect <- c(match(median(month_table$mean_monthly_temp),month_table), match(max(month_table$Slope),month_table),
                   match(median(month_table$Slope),month_table), match(min(month_table$Slope),month_table))
  df_negative_transpose <- cbind(df_negative_transpose, seg_id_vect)
  return(df_negative_transpose)
}

bind_transposed <- function(df_positive_transpose, df_negative_transpose)
{
final_df <- bind_rows(df_positive_transpose, df_negative_transpose)
return(final_df)
}
