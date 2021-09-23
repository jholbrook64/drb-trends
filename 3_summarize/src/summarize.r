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
{ # datat come in as all regression binded
  # then melt data in joined df
  #d_plot <- melt(regression_data_all, id = c("mean_monthly_temp", "max_temp_observed", "min_temp_observed"))
  
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