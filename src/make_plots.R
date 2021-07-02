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

plot_tot <- function(in_monthDat, fileout)
{
  #convert((num(n_per_month)))
  print(in_monthDat)
  
  in_monthDat <- readr::read_csv(in_monthDat)
  #in_monthDat <- as.numeric(in_monthDat$n_per_month)
  
  months <- in_monthDat %>% 
    # uses simple val 27 in absence of costly date check function
    filter(n_per_month > 27) %>% 
    subset(month_year < "1995-01-01") %>% 
    group_by(seg_id_nat) %>% 
    mutate(n_all_time= sum(n_per_month)) #%>% 
   # mutate(months = count(month_year))
  
  v <- 0 
  for (i in 1:nrow(months)) {  # nots ure why this is computing out the way it is. 
    if (months[i, 4] > 10000)  #this is the check the amount of data avaliable. 
    {
      v <- v + 1
    }
  }
  print(v)
  
  ggplot(data = months, aes(x = seg_id_nat, y = n_per_month)) +
  geom_bar(stat='identity') + theme_bw()
  ggsave(fileout, width = 12, height = 8)

  return(fileout)
}

# plot_tot2 <- function(annualData, fileout)
# {
#   #convert((num(n_per_month)))
#   #print(in_monthDat)
#   
#   annual <- readr::read_csv(in_monthDat)
#   #in_monthDat <- as.numeric(in_monthDat$n_per_month)
#   
#   months <- in_monthDat %>% 
#     # uses simple val 27 in absence of costly date check function
#     filter(n_per_month > 27) %>% 
#     group_by(seg_id_nat) %>% 
#     mutate(n_all_time= sum(n_per_month)) #%>% 
#   # mutate(months = count(month_year))
#   
#   ggplot(data = months, aes(x = seg_id_nat, y = n_all_time)) +
#     geom_bar(stat='identity') + theme_bw()
#   ggsave(fileout, width = 12, height = 8)
#   
#   return(fileout)
# }
