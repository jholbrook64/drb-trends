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


#' Creates linear regressions per each branch fed into the function
#'
#' @param sites A data frame of daily water temperature observation values
#' @param type A categorical value for the type of regression ran, type can be either 1, 2, 3, or 4
#' @return data frame of trend values from each regression of the branch
#' @examples
#' line_plot_function(data, 3)
flexible_linear_regression <- function(sites, type)
{
#browser()
  #where sites is each branched target, and type is the type of linear regression that is be run
  if (type == 1) {



    if(all(is.nan(sites$month_mean))){
      dfstats <- data.frame("site id" = sites$site_id[[1]],
                            "series" = sites$series[1],
                            "Month" = sites$month[[1]],
                            "max_temp_observed" = NA,
                            "mean_monthly_temp" = NA,
                            "min_temp_observed" = NA,
                            #"Date Range" = lubridate::as.interval(start = sites$date[[1]], sites$date[[nrow(sites)]]),
                            "years" = NA,
                            "Slope" = NA,
                            "p_value" = NA,
                            "is_significant" = NA,
                            "r2" = NA)
      return(dfstats)
    } else {
      sites <- sites %>%
        drop_na(month_mean)
      lr <- lm(month_mean ~ year, data = sites)
      sum_lr <- summary(lr)
      rs <- sum_lr$r.squared
      p_vlaue <- sum_lr$coefficients[,4][2]
      max_temp <-  max(sites$month_mean, na.rm = TRUE)
      mean_temp <- mean(sites$month_mean, na.rm = TRUE)
      min_temp <-  min(sites$month_mean, na.rm = TRUE)

    }
  } else if (type == 2) {
    sites <- sites %>%
      drop_na(month_meanOfMax)

    lr <- lm(month_meanOfMax ~ year, data = sites)
    sum_lr <- summary(lr)
    rs <- sum_lr$r.squared
    p_vlaue <- sum_lr$coefficients[,4][2]
    max_temp <-  max(sites$month_meanOfMax, na.rm = TRUE)
    mean_temp <- mean(sites$month_meanOfMax, na.rm = TRUE)
    min_temp <-  min(sites$month_meanOfMax, na.rm = TRUE)

  } else if (type == 3) {

    sites <- sites %>%
      drop_na(month_meanOfMin)

    lr <- lm(month_meanOfMin ~ year, data = sites)
    sum_lr <- summary(lr)
    rs <- sum_lr$r.squared
    p_vlaue <- sum_lr$coefficients[,4][2]
    max_temp <-  max(sites$month_meanOfMin, na.rm = TRUE)
    mean_temp <- mean(sites$month_meanOfMin, na.rm = TRUE)
    min_temp <-  min(sites$month_meanOfMin, na.rm = TRUE)
  } else if (type == 4) {
    if(all(is.nan(sites$annual_mean)))
    {
      return(
        dfstats <- data.frame("site id" = sites$site_id[[1]],
                              "Month" = sites$month[[1]],
                              "series" = sites$series[1],
                              "max_temp_observed" = NA,
                              "mean_monthly_temp" = NA,
                              "min_temp_observed" = NA,
                              #"Date Range" = lubridate::as.interval(start = sites$date[[1]], sites$date[[nrow(sites)]]),
                              "years" = NA,
                              "Slope" = NA,
                              "p_value" = NA,
                              "is_significant" = NA,
                              "r2" = NA)
      )
    } else {
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
                        "series" = sites$series[1],
                        "max_temp_observed" = max_temp,
                        "mean_monthly_temp" = mean_temp,
                        "min_temp_observed" = min_temp,
                        #"Date Range" = lubridate::as.interval(start = sites$year[[1]], sites$year[[nrow(sites)]]),
                        "years" = length(unique(sites$year)),
                        "Slope" = stats[2],
                        #"r" = rs^(1/2),
                        "p_value" = p_vlaue,
                        "is_significant" = p_vlaue < 0.05,
                        "r2" = rs)
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

#' Creates a single density plot that shows the distribution of trends of the annual data regressions
#'
#' @param sites A data frame of daily water temperature observation values
#' @param type A categorical value for the type of regression ran, type can be either 1, 2, 3, or 4
#' @return data frame of trend values from each regression of the branch
#' @examples
#' line_plot_function(data, 3)
density_plot <- function(annual_data)
{
  p <- ggplot(data = annual_data, aes(x=Slope)) +
    geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
    theme_bw() +
    scale_color_brewer(palette="Dark2") +
    ggtitle("Distribution of Warming Trends Across Selected Sites") + 
    xlab("warming trend in degrees Celsius / year") +
    ylab("Number of Observed Sites") 
  
  this_filename <-  file.path('3_summarize/', 'out/', 'denisty_plot_annual', '.png', fsep = "")
  ggsave(filename = this_filename, p, height = 7, width = 12)
  return(this_filename)
}
