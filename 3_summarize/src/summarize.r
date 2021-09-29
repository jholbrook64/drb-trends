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
  browser()
  regression_data_all <- regression_data_all %>% 
    group_by(regression_type) %>% 
    mutate(group_mean = mean(Slope, na.rm = TRUE)) %>% 
    ungroup()
                        
  p <- ggplot(aes(x=Slope, fill = regression_type), data = regression_data_all) +
    geom_vline(data = regression_data_all, aes(xintercept = group_mean, size = 1,
                                               color = regression_type, group = regression_type, na.rm = FALSE, show.legend = FALSE)) +
    geom_density(alpha = 0.6)+
    scale_color_identity() +
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
  #browser()
  p <- ggplot(data = subset(temp_data, !is.na(month_mean)), aes(x=year, y= month_mean), group =1) +
    geom_point()+
    geom_smooth(formula = y ~ x, method = "lm")+
    theme_bw() +
    ggtitle("Trend in Temperature values for Site below Cannonsville") + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    ylab("Mean Monthly temperature in Degrees Celsius") 
    
    
  
  this_filename <-  file.path('3_summarize/', 'out/', 'line_plot', '.png', fsep = "")
  ggsave(filename = this_filename, p, height = 7, width = 12, scale = 0.6)
  return(this_filename)
}

ANOVA <- function(regression_data, crosswalk)
{
  # join the regression data to the crosswalk data
  
}