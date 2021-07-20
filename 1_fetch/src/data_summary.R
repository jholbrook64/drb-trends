# this is "1_fetch/src/data_summary"
# this file will be used for summarizing outputs of linear regressions

DRB_monthly <- function(regressionData, fileout)
{
  months = c("January", "February", "March", "April", "May", "June", "July", "August", 
             "September", "October", "November", "December")
  # take return from monthly_trend_analysis
  lr_list = list()
  month_order <- c(1:12)
  #order dataframe by increasing months
  regressionData <- regressionData[match(month_order, regressionData$month),]
  for (i in unique(regressionData$month)) 
  {
    unique_month <- regressionData %>% 
      filter(month == i) %>% 
      drop_na(month_mean)
    lr <- lm(month_mean ~ year, data = unique_month)
    lr_list[[i]] = lr
  }

  pdf(fileout)
  for (i in unique(regressionData$month)) 
  {
    print(lr_list[[i]])
    unique_month <- regressionData %>% 
      filter(month == i) 
    #print(plot(lr_list[[i]]))  #difficult to interpret
    print(plot(unique_month$year, unique_month$month_mean, main = months[i],
              xlab = "year", ylab = "mean temperature on the month in celsius"))
    print(lines(predict(lr), col = 'green'))
  }
  return(fileout)
}

regress_site <- function(sites)
{
  # This should be all I need if my targets are dynamically branching
  lr <- lm(month_mean ~ year, data = sites)
  return(lr)
}

