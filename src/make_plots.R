## ---------------------------
##
## Script name: make_plots
##
## Purpose of script: selecting sites
##
## Author: Jack Holbrook (USGS)
##
## ---------------------------
## Notes:
##     ~
## ---------------------------

plot_tot <- function(add_days, fileout)
{
  time_series <- add_days %>% 
    group_by(seg_id_nat) %>%
    drop_na(seg_id_nat) %>% 
    summarize(start_date = min(date),
              end_date = max(date)) %>% 
    filter(start_date <= as.Date("1995-01-01")) %>% 
    mutate(day_range = end_date - start_date) %>% 
    mutate(day_count = as.numeric(day_range)) %>% 
    mutate(date_range = lubridate::time_length(day_range, unit = "year"))
  
  sampleVariance <- CV(time_series$day_count)
  chart_subtitle <- substitute(paste("Coefficient of variance of sample length is: ",
                                     sampleVariance))
  ggplot(data = time_series, aes(x = as.factor(seg_id_nat), 
                                 y = date_range, fill = day_count)) +
    geom_bar(stat='identity', position = position_dodge(), colour="black") + 
    # geom_errorbar(aes(ymin = day_count-sd, ymax = day_count+sd), width =.2,
    #               position = position_dodge(.9)) 
    labs(title = "Years of observations for sites prior to 1995",
       subtitle = chart_subtitle, 
       x = "DRB Observation Site ID", y = "Amount of Daily Observations in Years") +
    theme_minimal() +
    ggsave(fileout, width = 12, height = 8)
  
  return(fileout)
}

plot_current <- function(add_days, fileout)
{
  # filters data so that the 7 sites that don't 
  # have samples to the present date are cut
  recent_data <- add_days %>%
    group_by(seg_id_nat) %>%
    drop_na(seg_id_nat) %>% 
    summarize(start_date = min(date),end_date = max(date)) %>% 
    filter(start_date <= as.Date("1995-01-01")) %>% 
    filter(end_date == as.Date("2021-03-31")) %>% 
    mutate(day_range = end_date - start_date) %>% 
    mutate(day_count = as.numeric(day_range)) %>% 
    mutate(date_range = lubridate::time_length(day_range, unit = "year"))
  
  sampleVariance <- CV(recent_data$day_count)
  chart_subtitle <- substitute(paste("Coefficient of variance of sample length is: ",
                                     sampleVariance))
  ggplot(data = recent_data, aes(x = as.factor(seg_id_nat), 
                                 y = date_range, fill = day_count)) +
    geom_bar(stat='identity', position = position_dodge(), colour="black") + 
    # geom_errorbar(aes(ymin = day_count-sd, ymax = day_count+sd), width =.2,
    #               position = position_dodge(.9)) 
    labs(title = "Years of observations for sites prior to 1995 to march 2021",
         subtitle = chart_subtitle, 
         x = "DRB Observation Site ID", y = "Amount of Daily Observations in Years") +
    theme_minimal() +
    ggsave(fileout, width = 12, height = 8)
  
  return(fileout)
}

plot_fig <- function(in_file, add_days)
{
  dat <- readRDS(in_file)

  first_obs <- add_days %>% 
    group_by(seg_id_nat) %>% 
    mutate(date == min(date))

  # used for the text output
  cleanedData <- add_days
    
   long_term_obs_sites <- add_days %>%
     group_by(seg_id_nat) %>%
     drop_na(seg_id_nat) %>% 
     summarize(start_date = min(date)) %>% 
     filter(start_date <= as.Date("1995-01-01"))

  #long_term_obs_sites <- length(long_term_sites)
    
   c("the original size of the data ingested in the file ", nrow(dat),
     " the new size of the data to be worked with is ", nrow(cleanedData),
     "The number of sites with data that goes back further than 25 years is ",
      nrow(long_term_obs_sites))%>% write_lines("out/summary.txt")

   return("out/summary.txt")
}

# extra coefficient of variation function
CV <- function(x) { sd(x) / mean(x) }
