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

flexible_linear_regression <- function(sites, type)
{
  
  #browser()
  
  #where sites is each branched target, and type is the type of linear regression that is be run 
  if (type == 1)
  {
    lr <- lm(month_mean ~ year, data = sites)
    sum_lr <- summary(lr)
    rs <- sum_lr$r.squared
    p_vlaue <- sum_lr$coefficients[,4][2]
    max_temp <-  max(sites$month_mean, na.rm = TRUE)
    mean_temp <- mean(sites$month_mean, na.rm = TRUE)
    min_temp <-  min(sites$month_mean, na.rm = TRUE)
  }
  else if (type == 2)
  {
    #2 will be the mean of max's. 
    lr <- lm(month_meanOfMax ~ year, data = sites)
    sum_lr <- summary(lr)
    rs <- sum_lr$r.squared
    p_vlaue <- sum_lr$coefficients[,4][2]
    max_temp <-  max(sites$month_meanOfMax, na.rm = TRUE)
    mean_temp <- mean(sites$month_meanOfMax, na.rm = TRUE)
    min_temp <-  min(sites$month_meanOfMax, na.rm = TRUE)
  }
  else if (type == 3)
  {
    lr <- lm(month_meanOfMin ~ year, data = sites)
    sum_lr <- summary(lr)
    rs <- sum_lr$r.squared 
    p_vlaue <- sum_lr$coefficients[,4][2]
    max_temp <-  max(sites$month_meanOfMin, na.rm = TRUE)
    mean_temp <- mean(sites$month_meanOfMin, na.rm = TRUE)
    min_temp <-  min(sites$month_meanOfMin, na.rm = TRUE)
  }
  else if (type == 4)
  {
    # kept as of 12:34 pm on monday 8-2
    # here there is a problem of one of the targets having all NA values
    if(all(is.nan(sites$annual_mean)))
    {
      return(  dfstats <- data.frame("site id" = sites$site_id[[1]], 
                                     "Month" = sites$month[[1]],
                                     "max_temp_observed" = 0,
                                     "mean_monthly_temp" = 0,
                                     "min_temp_observed" = 0,
                                     "Date Range" = lubridate::as.interval(start = sites$date[[1]], sites$date[[nrow(sites)]]),
                                     "years" = sites$n_year[[1]],
                                     "Slope" = 0,
                                     "p_value" = 0,
                                     "is_significant" = 0,
                                     "r2" = 0
      ))
    }
    else{
    lr <- lm(annual_mean ~ year, data = sites)
    sum_lr <- summary(lr)
    rs <- sum_lr$r.squared
    p_vlaue <- sum_lr$coefficients[,4][2]
    max_temp <-  max(sites$annual_mean, na.rm = TRUE)
    mean_temp <- mean(sites$annual_mean, na.rm = TRUE)
    min_temp <-  min(sites$annual_mean, na.rm = TRUE)
    }
  }
  stats <- c(sum_lr$coefficients[[1]],
             sum_lr$coefficients[[2]])
  
  dfstats <- data.frame("site id" = sites$site_id[[1]], 
                        "Month" = sites$month[[1]],
                        "max_temp_observed" = max_temp,
                        "mean_monthly_temp" = mean_temp,
                        "min_temp_observed" = min_temp,
                        "Date Range" = lubridate::as.interval(start = sites$date[[1]], sites$date[[nrow(sites)]]),
                        "years" = sites$n_year[[1]],
                        "Slope" = stats[2],
                        #"r" = rs^(1/2),
                        "p_value" = p_vlaue,
                        "is_significant" = p_vlaue < 0.01,
                        "r2" = rs
                        )
  return(dfstats)
}

summarize_table <- function(regression_table)
{
  summaryT <- regression_table %>% 
    group_by(Month) %>% 
    summarise(Min_slope = min(Slope), 
              Max_slope = max(Slope),
              strongest_correlation = max(r2),
              max_temp_observed = max(max_temp_observed),
              min_temp_observed = min(min_temp_observed),
              sd_across_branches = sd(mean_monthly_temp),
              Quartile_range_across_branches = IQR(mean_monthly_temp),
              longest_observation = max(years))
  
  return(summaryT)
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
