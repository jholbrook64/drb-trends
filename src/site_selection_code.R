## ---------------------------
##
## Script name: site_selection_code
##
## Purpose of script: selecting sites
##
## Author: Jack Holbrook (USGS)
##
## ---------------------------
## Notes:
##     ~
## ---------------------------

meta_data <- function(annual) # this will be the function that does an eda of the data here:
{
  minimum_sampled_days <-  121 # four months
  table <- annual %>% group_by(seg_id_nat) 
  table <- subset(table, table[, 3] > minimum_sampled_days)
  table <- table %>% group_by(seg_id_nat) %>% summarise(
    n_per_year = mean(n_per_year),
    n_all_time = mean(n_all_time)
  )
  readr::write_csv(table, 'out/groupedTable.csv')
  return('out/groupedTable.csv')
}

# this one will be useful for plotting later on 
split_df <- function(in_file)
{
  dat <- readRDS(in_file)
  split_data <- split(dat, f= annual$seg_id_nat) #split(annual, seg_id_nat)
  split_data <- lapply(split_data, get_dateRange)
  # site_summary <- vector()
  # for (site_id in split_data) 
  # {
  #   # can call the group time function here as well
  #   #for each segment, create a list the holds segment id, date at head, date at tail
  #   new_vect <- split_data[site_id, seg_id_nat]   
  # }
  # readr::write_csv(table, 'out/splitData.csv')
  # return('out/splitData.csv')
  print(split_data)  # printing gives the correct number of tibbles. (1 per group) 
  return(split_data)
}

# old one: (7/8/2021) 
# group_time <- function(add_days)  # this will be the data preparing function!
# {
#   annual <- add_days %>%
#     mutate(year = lubridate::year(date)) %>%
#     group_by(seg_id_nat, year) %>%
#     drop_na(seg_id_nat) %>%
#     summarize(n_per_year = length(unique(date))) %>%
#     group_by(seg_id_nat) %>%
#     mutate(n_all_time = sum(n_per_year)) %>% ungroup()
#   
#   print(head(annual))
#   return(annual)
# }

group_time <- function(add_days)  # this will be the data preparing function!
{
  time_series_plots <- add_days %>% 
    group_by(seg_id_nat) %>% 
    mutate(start_date_id = min(date)) %>% 
    mutate(end_date_id = max(date)) %>% 
    filter(start_date_id <= as.Date("1995-01-01")) %>%
    filter(end_date_id >= as.Date("2010-01-01")) %>% 
    ungroup()
  
  return(time_series_plots)
}

write_up <- function(annual)
{
  # get number of unique segment id's.
  no_unique <- sapply(annual, function(x) length(unique(x))) # the n_all_time will be 
  print(no_unique)  # will only print once, for reference, below is the return:
  
  #seg_id_nat year n_per_year n_all_time 
  #332         85        343        245
  return(no_unique)
}
