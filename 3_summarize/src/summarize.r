## ---------------------------
##
## Script name: summarize.R
##
## Purpose of script: mapping reaches $ their annual observations
##
## Author: Jack Holbrook (USGS)
##
## ---------------------------
## Notes: implementation file for data summaries
##     ~
## ---------------------------

overlap_density <-  function(regression_data_all)
{ 
  p <- ggplot(aes(x=Slope, fill = regression_type, group = regression_type), data = regression_data_all) +
    geom_density(alpha = 0.6)+
    theme_bw() +
    scale_color_brewer() +
    ggtitle("Distribution of Warming Trends Across Selected Sites") + 
    xlab("Slope of Trends in Degrees Celsius per Year") +
    ylab("Density") 
  
  this_filename <-  file.path('3_summarize/', 'out/', 'max_mean_min_denisty_plot', '.png', fsep = "")
  ggsave(filename = this_filename, p, height = 7, width = 12)
  return(this_filename)
  
}

line_plot_function <- function(temp_data)
{
  browser()
  #making a date vector
  temp_data <- temp_data %>% filter(month ==7)
  temp_data$Date <- with(temp_data, sprintf("%d-%02d", year, month))
  temp_data$Date <- as.Date(temp_data$Date, format='%m/%d')
  #temp_data$Date <- zoo::as.yearmon(paste(df$year, df$month), "%Y %m")
  p <- ggplot(data = temp_data, aes(x=Date, y= month_mean), group =1) +
    geom_point()+
    theme_bw() +
    geom_smooth(method = "lm")+
    ggtitle("Trend in Temperature values for Site below Cannonsville") + 
    #xlab("Date") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
    ylab("Mean Monthly temperature in Degrees Celsius") 
    
    #geom_abline()
  
  this_filename <-  file.path('3_summarize/', 'out/', 'line_plot', '.png', fsep = "")
  ggsave(filename = this_filename, p, height = 7, width = 12)
  return(this_filename)
}

ANOVA <- function(regression_data, crosswalk)
{
  # join the regression data to the crosswalk data
  
}