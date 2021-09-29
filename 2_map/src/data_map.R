## ---------------------------
##
## Script name: data_map.R
##
## Purpose of script: mapping reaches and their annual observations
##
## Author: Jack Holbrook (USGS)
##
## ---------------------------
## Notes:
##     ~  
## ---------------------------

boxplot_func <- function(regression_data, type, site_info)
{
  #browser()
  # this is new, used to join data
  site_info <- read_csv(site_info)
  regression_data <- regression_data %>% left_join(site_info, by = 'site.id')

  if (type == 3) 
  {
    browser()
    regression_data$Month <- as.factor(regression_data$Month)
    regression_data$Slope <- as.numeric(regression_data$Slope)
    regression_data$reservoir_code <- as.factor(regression_data$reservoir_code)
    month_list <- c("January", "February", "March", "April", "May", "June", "July", "August",
                    "September", "October", "November", "December")
    regression_data$Month <- month_list[regression_data$Month]
    
    labs <- levels(cut(regression_data$Slope, 12))
    binned_slope <- cut(regression_data$Slope, 12, inlcude.lowest = TRUE, labels = labs)
    
    names(regression_data)[names(regression_data) == 'reservoir_code'] <- 'Reservoir Code'
    names(regression_data)[names(regression_data) == 'is_significant'] <- 'Significant above a P of 0.01'
    
    regression_data$Month <- factor(regression_data$Month, levels = month.name)
    regression_data <- filter(regression_data, !is.na(`Reservoir Code`))
    boxp <- ggplot(regression_data, aes(x = Month, y = Slope, group = Month, show.legend = TRUE))+
      geom_hline(yintercept=0, linetype="dashed", color = "black") +
      geom_violin() +
      stat_summary(fun = median, fun.min = median, fun.max = median,
                   geom = "crossbar",
                   width = 0.25, color = 'black') +
      geom_jitter(aes(shape=`Significant above a P of 0.05`, size = 4), position=position_jitter(0.2), alpha = 0.4) +
      scale_shape_manual(values = c(1,16)) +
      theme_bw() +
      scale_color_brewer(palette="Dark2") +
      ggtitle("Distribution of Stream Segment Trends for Each Month from Daily Temperature Minimums") + 
      ylab("site Trends") 
    
    this_filename <- file.path('2_map', 'out', 'boxplots_monthly_min.png')
    ggsave(filename = this_filename, boxp, height = 7, width = 12)
    return(this_filename)
  }
  if (type == 2) 
  {
    browser()
    regression_data$Month <- as.factor(regression_data$Month)
    regression_data$Slope <- as.numeric(regression_data$Slope)
    regression_data$reservoir_code <- as.factor(regression_data$reservoir_code)
    month_list <- c("January", "February", "March", "April", "May", "June", "July", "August",
                    "September", "October", "November", "December")
    regression_data$Month <- month_list[regression_data$Month]

    labs <- levels(cut(regression_data$Slope, 12))
    binned_slope <- cut(regression_data$Slope, 12, inlcude.lowest = TRUE, labels = labs)
  
    names(regression_data)[names(regression_data) == 'reservoir_code'] <- 'Reservoir Code'
    names(regression_data)[names(regression_data) == 'is_significant'] <- 'Significant above a P of 0.01'
    
    regression_data$Month <- factor(regression_data$Month, levels = month.name)
    regression_data <- filter(regression_data, !is.na(`Reservoir Code`))
    boxp <- ggplot(regression_data, aes(x = Month, y = Slope, group = Month, show.legend = TRUE))+
      geom_hline(yintercept=0, linetype="dashed", color = "black") +
      geom_violin() +
      stat_summary(fun = median, fun.min = median, fun.max = median,
                   geom = "crossbar",
                   width = 0.25, color = 'black') +
      geom_jitter(aes(shape=`Significant above a P of 0.05`, size = 4), position=position_jitter(0.2), alpha = 0.4) +
      scale_shape_manual(values = c(1,16)) +
      theme_bw() +
      scale_color_brewer(palette="Dark2") +
      ggtitle("Distribution of Stream Segment Trends for Each Month from Daily Temperature Means") + 
      ylab("site Trends") 
    
    this_filename <- file.path('2_map', 'out', 'boxplots_monthly_mean.png')
    ggsave(filename = this_filename, boxp, height = 7, width = 12)
    return(this_filename)
  }
  else if (type == 1)
  {
    browser()
    regression_data$Month <- as.factor(regression_data$Month)
    regression_data$Slope <- as.numeric(regression_data$Slope)
    regression_data$reservoir_code <- as.factor(regression_data$reservoir_code)
    month_list <- c("January", "February", "March", "April", "May", "June", "July", "August",
                    "September", "October", "November", "December")
    regression_data$Month <- month_list[regression_data$Month]
    
    labs <- levels(cut(regression_data$Slope, 12))
    binned_slope <- cut(regression_data$Slope, 12, inlcude.lowest = TRUE, labels = labs)
    
    names(regression_data)[names(regression_data) == 'reservoir_code'] <- 'Reservoir Code'
    names(regression_data)[names(regression_data) == 'is_significant'] <- 'Significant above a P of 0.01'
    
    regression_data$Month <- factor(regression_data$Month, levels = month.name)
    regression_data <- filter(regression_data, !is.na(`Reservoir Code`))
    boxp <- ggplot(regression_data, aes(x = Month, y = Slope, group = Month, show.legend = TRUE))+
      geom_hline(yintercept=0, linetype="dashed", color = "black") +
      geom_violin() +
      stat_summary(fun = median, fun.min = median, fun.max = median,
                   geom = "crossbar",
                   width = 0.25, color = 'black') +
      geom_jitter(aes(shape=`Significant above a P of 0.05`, size = 4), position=position_jitter(0.2), alpha = 0.4) +
      scale_shape_manual(values = c(1,16)) +
      theme_bw() +
      scale_color_brewer(palette="Dark2") +
      ggtitle("Distribution of Stream Segment Trends for Each Month from Daily Temperature Maximums") + 
      ylab("site Trends") 
    
    this_filename <- file.path('2_map', 'out', 'boxplots_monthly_max.png')
    ggsave(filename = this_filename, boxp, height = 7, width = 12)
    return(this_filename)
  }
}

map_sites <- function(data_for_trend_analysis_month, in_network, in_crosswalk) 
{
  #browser()

  points <- as.data.frame(readRDS(in_crosswalk))
  site_v <- data_for_trend_analysis_month$site.id
  names(points)[names(points) == 'site_id'] <- 'site.id'
  select_points <- filter(points, points$site.id %in% site_v)
  net <- readRDS(in_network)[[1]]
  
  # joins attribute w/ spatial data from attribute to points
  net_d <- net %>% left_join(points, by = 'seg_id_nat')  # this works!
  
  # joins attribute w/ spatial data from attribute & points to network lines
  names(net_d)[names(net_d) == 'site_id'] <- 'site.id'
  net_d <- net_d %>% left_join(data_for_trend_analysis_month, by = 'site.id') 
  
  points_p <- st_as_sf(select_points, coords = c("longitude", "latitude"), 
                       crs = 4326)
  points_data <- points_p %>% left_join(data_for_trend_analysis_month, by = 'site.id')
  names(points_data)[names(points_data) == 'Slope'] <- 'Warming Trend degC Year'
  
  # reassign variable name for a better legend
  names(net_d)[names(net_d) == 'Slope'] <- 'Warming Trend degC Year'
  
  #  assigns file name based off date month, is same value for each branch
  month <- unique((data_for_trend_analysis_month$Month))
  month_list <- c("January", "February", "March", "April", "May", "June", "July", "August",
                  "September", "October", "November", "December")
  monthname <- month_list[month]
  title <- "DRB wide warming trend for the month of "
  background <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
  # this should turn background data into a subset
  background <- background[background$ID=="new jersey" | background$ID=="pennsylvania" |
                             background$ID=="delaware" | background$ID=="maryland" | background$ID=="new york",]  
  aoi <- st_crop(points_p, xmin = min(points$longitude), xmax = max(points$longitude),
                            ymin = min(points$latitude), ymax = max(points$latitude),
                            crs = 4326)
  p <- ggplot(net_d) +
    geom_sf(data = background, fill="white") +
    geom_sf(color = 'grey') +
    geom_sf(data = filter(net_d, !is.na(`Warming Trend degC Year`))) +
    geom_sf(data = points_data, aes(color =`Warming Trend degC Year`)) +   
    theme_bw() +
    coord_sf(xlim = c(min(points$longitude), xmax = max(points$longitude)),
             ylim = c(ymin = min(points$latitude), ymax = max(points$latitude)),
             crs = 4326) +
    scale_color_viridis_c(direction = -1, option = 'plasma', end = 1) +
    ggtitle(paste(title, monthname, sep = ""))+ 
    xlab(expression(paste(Longitude^o,~'N'))) +
    ylab(expression(paste(Latitude^o,~'W')))
  
  # saving file
  this_filename <-  file.path('2_map/', 'out/', monthname, '.png', fsep = "")
  ggsave(filename = this_filename, p, height = 7, width = 5)
  return(this_filename)
}


map_tiles <-function(data_for_trend_analysis_month, in_network, in_crosswalk)   
{
  browser()
  # simply trying to get an interactive tile map of the same static map
  points <- as.data.frame(readRDS(in_crosswalk))
  site_v <- data_for_trend_analysis_month$site.id
  names(points)[names(points) == 'site_id'] <- 'site.id'
  select_points <- filter(points, points$site.id %in% site_v)
  net <- readRDS(in_network)[[1]]
  
  #running unique joins across datas
  net_d <- net %>% left_join(points, by = 'seg_id_nat')
  
  # joins attribute w/ spatial data from attribute & points to netowrk lines
  names(net_d)[names(net_d) == 'site_id'] <- 'site.id'
  net_d <- net_d %>% left_join(data_for_trend_analysis_month, by = 'site.id') 
  
  # change anme
  names(net_d)[names(net_d) == 'Slope'] <- 'Warming Trend degC Year'
  
  # create points data set as spatial reference 
  points_p <- st_as_sf(select_points, coords = c("longitude", "latitude"), 
                       crs = 4326)
  
  # use for plotting number of observations
  month <- unique((data_for_trend_analysis_month$Month))
  month_list <- c("January", "February", "March", "April", "May", "June", "July", "August",
                  "September", "October", "November", "December")
  monthname <- month_list[month]
  
  #net_d <- left_join(net, select(data_for_trend_analysis_month, site_id = seg_id_nat, Slope))
  #net_d <- net_d[!rowSums(is.na(net_d[`Warming Trend degC Year`])), ] 
  #pal = mapviewpalette("")
  mapviewGetOption("basemaps")
  mapviewOptions(basemaps = c("Esri.WorldImagery", "OpenStreetMap"),
                 raster.palette = grey.colors,
                 vector.palette = colorRampPalette(c("snow", "cornflowerblue", "grey10")),
                 na.color = "grey",
                 layers.control.pos = "topright")
  map_view <- mapview(net_d, zcol =  'Warming Trend degC Year') + mapview(points_p, zcol = 'ID')
                      col.regions = c("yellow", "orange", "pink", "red", "purple")
  map_shot <- mapshot(map_view, url = paste0(getwd(), '/2_map/out/satMap.html')
                      #,file = paste0(getwd(), "/map.png")
  )
  return('2_map/out/satMap.html')  # hooray! this works!
}


# began modifiying this funciton to do a different thing 9/21/2021
hist_func <- function(regression_data)
{
  browser()
  
  # takes annual data since the boxplots no longer works for annual data:
 # regression_data$seg_id_nat <- as.factor(regression_data$seg_id_nat)
  boxp <- ggplot(regression_data, aes(x = r2))+  #x = seg_id_nat, y = Slope
    geom_density()+
    ggtitle("Distribution of Annual Stream Segment Trends for Each Segment")+ 
    xlab("Trend Value") +
    ylab("Count") +
    theme(axis.text.x = element_text(angle = 90))
  
  this_filename <-  file.path('2_map', 'out', 'trends_hist.png')
  ggsave(filename = this_filename, boxp, height = 7, width = 12)
  return(this_filename)
}
