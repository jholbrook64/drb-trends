map_sites <- function(in_dat, in_network) {
  
  net <- readRDS(in_network)[[1]]
  
  # join the network with the data so you can use things like
  # the number of observations for plotting
  net_d <- left_join(net, distinct(select(in_dat, seg_id_nat, n_all_time)))
  
  # look at all sites with data
  p <- ggplot(net_d) +
    geom_sf(color = 'gray') +
    # filter to sites with a n_all_time value, which indicates at least some data
    # color by how much data a segment has
    geom_sf(data = filter(net_d, !is.na(n_all_time)), aes(color = n_all_time)) +
    # change color scale
    scale_color_viridis_c(direction = -1, option = 'plasma', end = 0.95) +
    theme_bw()
  
  ggsave('out/example_plot.png', p, height = 7, width = 5)
  return('out/example_plot.png')
  
}

map_tiles <-function(in_dat, in_network)   # takes in the sample spatial data as other function
{
  # simply trying to get an interactive tile map of the same static map
  net <- readRDS(in_network)[[1]]
  # use for plotting number of observations
  net_d <- left_join(net, distinct(select(in_dat, seg_id_nat, n_all_time)))
  net_d <- net_d[!rowSums(is.na(net_d["n_all_time"])), ] 
  #pal = mapviewpalette("")
  map_view <- mapview(net_d, zcol =  "n_all_time",
                      col.regions = c("yellow", "orange", "pink", "red", "purple"))

  map_shot <- mapshot(map_view, url = paste0(getwd(), '/out/map.html')
                      #,file = paste0(getwd(), "/map.png")
                      )
  return('out/map.html')  # hooray! this works!
}