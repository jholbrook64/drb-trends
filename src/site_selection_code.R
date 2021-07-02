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
  
  #table <- subset(table, table[, 1] == if_nan(table$seg_id_nat))
  # %>% annual[annual[, 3(annual)]>= minimum_sampled_days] 
  
  table <- table %>% group_by(seg_id_nat) %>% summarise(
    n_per_year = mean(n_per_year),
    n_all_time = mean(n_all_time)
  )
  readr::write_csv(table, 'out/groupedTable.csv')
  return('out/groupedTable.csv')
}


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

group_time <- function(dirty_months)
{
  # set removal vector to 0
  removeVect <- 0
  #creates vector of indeces to be removed
  for (var in 1:nrow(dirty_months)) 
    {
    x <- as.POSIXlt(dirty_months[var, 3], tz = "", format,
                    tryFormats = c("%Y-%m-%d",
                                   "%Y/%m/%d"))
    # compare n_per_month to the date
    if (dirty_months[var, 12] != lubridate::days_in_month(x)) {
      removeVect <- c(removeVect, var)
    }
  }
  # removes those indices
  clean_months <- dirty_months[-removeVect,]
  
  # watch out, im keeping this the same namespace
  readr::write_rds(dat, 'out/data_with_months.rds')  
  return('out/data_with_months.rds')
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

# get_dateRange <- function(alist)   leave this one, found different way to solve the problem this addresses
# {
#   for (variable in alist) 
#   {
#     
#   }
# }

numberOfDays <- function(date) # use this function to compare month to day # in month
  {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  # change return signature to account for allowed missed days
  return(as.integer(format(date - 1, format="%d")))
}

if_nan <- function(x)
{
  is.nan(x)
}