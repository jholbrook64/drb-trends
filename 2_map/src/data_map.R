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
##     ~
## ---------------------------

map_sites <- function(data_for_trend_analysis_month, in_network) 
  {
  net <- readRDS(in_network)[[1]]
  # joins attribute w/ spatial data
  net_d <- left_join(net, select(data_for_trend_analysis_month, seg_id_nat = seg_id_nat, Slope))
  # reassign variable name for a better legend
  names(net_d)[names(net_d) == 'Slope'] <- 'Warming Trend degC Year'
  #  assigns file name based off date month, is same value for each branch
  month <- unique((data_for_trend_analysis_month$Month))
  month_list <- c("January", "February", "March", "April", "May", "June", "July", "August",
                     "September", "October", "November", "December")
  monthname <- month_list[month]
  
  browser()
  
  title <- "DRB wide warming trend for the month of "
  background <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
  DRB_states <- c("new jersey", "pennsylvania", "delaware", "maryland",  "new york")
  background <- background[background$ID=="new jersey" | background$ID=="pennsylvania" | background$ID=="delaware" | background$ID=="maryland" | background$ID=="new york",]  # this should turn background data into a subset
  do.call(rbind.data.frame, background)
  # aoi <- st_bbox(c(xmin = min(network[2]$vertices$geometry[[1]]), xmax = max(network[2]$vertices$geometry[[1]]),
  #                  ymax = max(network[2]$vertices$geometry[[2]]), ymin = min(network[2]$vertices$geometry[[2]])), 
  #                crs = st_crs(4326))
  p <- ggplot(net_d) +
    
    # filter to sites with a n_all_time value, which indicates at least some data
   
     # color by how much data a segment has
    
    geom_sf(data = background, fill="white") +
    geom_sf(color = 'grey') +
    geom_sf(data = filter(net_d, !is.na(`Warming Trend degC Year`)), aes(color =`Warming Trend degC Year`)) +
    # geom_polygon(data = background, aes(x=long, y=lat, group=group),
    #             color="black", fill="gray") +
    # coord_sf(xlim = c(min(in_network[2]$vertices$geometry[[1]]), max(in_network[2]$vertices$geometry[[1]])),
    #          ylim = c(max(in_network[2]$vertices$geometry[[2]]), min(in_network[2]$vertices$geometry[[2]])), 
    #          expand = FALSE) +
    scale_color_viridis_c(direction = -1, option = 'plasma', end = 1) +
    theme_bw() +
    ggtitle(paste(title, monthname, sep = ""))+ 
    xlab(expression(paste(Longitude^o,~'N'))) +
    ylab(expression(paste(Latitude^o,~'W')))

  #saving file
  this_filename <-  file.path('2_map/', 'out/', monthname, '.png', fsep = "")
  ggsave(filename = this_filename, p, height = 7, width = 5)
  return(this_filename)
}

boxplot_func <- function(regression_data, type)
{
  if (type == 2) 
  {
    regression_data$Month <- as.factor(regression_data$Month)
    #regression_data$Slope <- as.factor(regression_data$Slope)
    # group_colors <- ifelse(levels(regression_data$Slope)>0.0, rgb(0.1,0.1,0.7,0.5),
    #                 ifelse(levels(regression_data$Slope)<0.0, rgb(0.8,0.1,0.3,0.6),
    #                             "grey90"  ))
    month_list <- c("January", "February", "March", "April", "May", "June", "July", "August",
                    "September", "October", "November", "December")
    regression_data$Month <- month_list[regression_data$Month]
    boxp <- ggplot(regression_data, aes(x = Month, y = Slope))+
      #geom_boxplot(fill = as.numeric(range(cut(regression_data$Month, breaks = -6:6, label = FALSE)))) + 
      geom_boxplot(fill = "blue") +
      # aes(fill = after_scale()),size =1)+
      # theme(legend.position="none") +
      theme(axis.text.x = element_text(angle = 90)) +
      scale_fill_distiller(palette = "RdYlGn", guide = "none") + 
      ggtitle("Distribution of Stream Segment Trends for Each Month")+ 
      xlab("Individual Segments") +
      ylab("site Trends") 
      
    
    this_filename <- file.path('2_map', 'out', 'boxplots_monthly.png')
    ggsave(filename = this_filename, boxp, height = 7, width = 5)
    return(this_filename)
  }
  else if (type == 1)
  {
    regression_data$seg_id_nat <- as.factor(regression_data$seg_id_nat)
    boxp <- ggplot(regression_data, aes(x = seg_id_nat, y = Slope))+
      geom_boxplot()+
      ggtitle("Distribution of Stream Segment Trends for Each Segment")+ 
      xlab("Individual Segments") +
      ylab("Yearly mean temperatures") +
      theme(axis.text.x = element_text(angle = 90))
    
    this_filename <-  file.path('2_map', 'out', 'boxplots_annual.png')
    ggsave(filename = this_filename, boxp, height = 7, width = 5)
    return(this_filename)
  }
}

hist_func <- function(regression_data)
{
  # takes annual data since the boxplots no longer works for annual data:
  regression_data$seg_id_nat <- as.factor(regression_data$seg_id_nat)
  boxp <- ggplot(regression_data, aes(x = Slope))+  #x = seg_id_nat, y = Slope
    geom_histogram()+
    ggtitle("Distribution of Stream Segment Trends for Each Segment")+ 
    xlab("Individual Segments") +
    ylab("Yearly mean temperatures") +
    theme(axis.text.x = element_text(angle = 90))
  
  this_filename <-  file.path('2_map', 'out', 'boxplots_annual.png')
  ggsave(filename = this_filename, boxp, height = 7, width = 5)
  return(this_filename)
}