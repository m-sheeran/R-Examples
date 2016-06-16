# Order Level is the amount of ordered units for each SKU by months, Forecast is the forecasted demand for each SKU by month. 
# skulist is list of SKU currently in permanent caseflow locations. Use query for list, load into
# dataframe in R workspace. 

gamma_revis = function(order_level_csv, forecast_csv, skulist)
{
  # read in order level, setkey by sku, and set NA = 0
  orderLevel = fread(order_level_csv)
  setkey(orderLevel, SKU)
  orderLevel[is.na(orderLevel)] = 0
  
  # read in forecast, setkey by sku, and set NA = 0
  forecast = fread(forecast_csv)
  setkey(forecast, SKU)
  forecast[is.na(forecast)] = 0
  
  setkey(skulist, SKU)
  
  # subset data with join on list of skus in caseflow locations
  # create empty data frame to hold list of locations to exhaust

  sub1 = merge(orderLevel, forecast)
  sub2 = merge(sub1, skulist)
  
  list = data.table(rep(0 , nrow(sub2)))

  # create new columns for totalopen, shipped, threshold, and %shipped
  # set NA's = 0
  sub2[ , totalOpen := MayOpen + JuneOpen]
  sub2[ , totalShipped := MayShipped + JuneShipped]
  sub2[ , threshold := rep(.75, nrow(sub2))][ , percentShipped := totalShipped / (totalOpen + totalShipped)]
  sub2[is.na(sub2)] = 0
  
  # loop through sub to find if %shipped > threshold.
  # if so, set decision to 1.
  # return list of skus with decision == 1
  for(i in 1: nrow(sub2))
  {
    if((sub2[i, percentShipped] > sub2[i , threshold]) & ( sub2[i ,totalOpen] < 100))
    {
      sub2[i , decision := 1]
    }
    else if (sub2[i , totalOpen ==0])
    {
      sub2[i, decision := 1]
    }
    else
    {
      sub2[i , decision := 0]
    }
  }
  list = sub2[decision == 1]
}