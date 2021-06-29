# starter functions for reading in data

evaluate_annual <- function(in_file) {
  
  # these are "raw" data not reduced to a single obs 
  # per seg-day
  dat <- readRDS(in_file)
  
  # now we can summarize by segment ID, which is the unit we should be measuring data availability
  # so, for example, we can calculate how many unique days of measurement a site has per year:
  annual <- dat %>%
    # make new year column for grouping
    mutate(year = lubridate::year(date)) %>%
    group_by(seg_id_nat, year) %>%
    # group by and summarize reduced data frame down to one row per seg_id_nat-year combo
    # we count unique dates because some stream segments will have multiple sites, and 
    # we don't want to double count since we'll have to evaluate a single value per day
    summarize(n_per_year = length(unique(date))) %>%
    group_by(seg_id_nat) %>%
    # group by and mutate will still do evaluation by group, but won't reduce to one value per seg_id_nat
    # this will produce repeated values per seg_id_nat
    mutate(n_all_time = sum(n_per_year)) %>% ungroup()
  
  # returns dataframe
  return(annual)
}

meta_data <- function(annual) # this will be the function that does an eda of the data here:
{
  minimum_sampled_days <-  121 # four months
  table <- annual %>% group_by(seg_id_nat) 
  table <- subset(table, table[, 3] > minimum_sampled_days)
  
  #table <- subset(table, table[, 1] == if_nan(table$seg_id_nat))
  # %>% annual[annual[, 3(annual)]>= minimum_sampled_days] 
  
  table <- table %>% group_by(seg_id_nat) %>% summarise(
    n_per_year = mean(n_per_year),
    n_all_time = mean(n_all_time)
  )
  
  # we will begin an operation to remove years with low sampling
  #table <- table %>% summarise()
  
  # for (year in vector) {
  #   
  # }
  
  readr::write_csv(table, 'out/groupedTable.csv')
  return('out/groupedTable.csv')
}

if_nan <- function(x)
{
  is.nan(x)
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