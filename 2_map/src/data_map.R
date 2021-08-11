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

map_sites <- function(data_for_trend_analysis_month, in_network) {
  # read the in_network piece
  net <- readRDS(in_network)[[1]]
  
  # join the network with the data so you can use things like
  # the number of observations for plotting
  
  net_d <- left_join(net, select(data_for_trend_analysis_month, seg_id_nat = seg_id_nat, Slope))
  net_d$Slope = as.numeric(levels(net_d$Slope))[[net_d$Slope]]
  names(net_d)[names(net_d) == 'Slope'] <- 'Warming Trend degC Year'

  #  assigns file name based off date month, is same value for each branch
  month <- unique((data_for_trend_analysis_month$Month))
  month_list <- c("January", "February", "March", "April", "May", "June", "July", "August",
                     "September", "October", "November", "December")
  monthname <- month_list[month]
  title <- "DRB wide warming trend for the month of "
  # device` must be NULL, a string or a function.
  out_filename <- paste("2_map/out/", month, sep = "")
  out_filename_ext <- paste0(substitute(out_filename))

  # look at all sites with data                              # below makes the geom object, the thing is different bc I eliminated aes
  background <- map_data("state")
  p <- ggplot(net_d) +
    geom_sf(color = 'gray') +
    # filter to sites with a n_all_time value, which indicates at least some data
   
     # color by how much data a segment has
  
    
    
    geom_sf(data = filter(net_d, !is.na('Warming Trend degC Year')), aes(color = 'Warming Trend degC Year')) +
    geom_polygon(data = background, aes(x=long, y=lat, group=group),
                 color="black", fill="gray") +
    #coord_sf(xlim = c(-77, -75), ylim = c(38.8, 43), expand = FALSE) +
    scale_color_viridis_c(direction = -1, option = 'plasma', end = 1) +
    theme_bw() +
    ggtitle(paste(title, monthname, sep = ""))+ 
    xlab(expression(paste(Longitude^o,~'N'))) +
    ylab(expression(paste(Latitude^o,~'W')))

  #saving file
  this_filename <-  file.path('2_map', 'out', paste0(monthname, '.png'))
  ggsave(filename = this_filename, p, device = "png", path = "2_map/out/", height = 7, width = 5)
  return(this_filename)
}