# filename1 is for Order Level, filename2 is for Forecast. skulist is list of
# skus currently in permanent caseflow locations. Use query for list, load into
# dataframe in R workspace.

beta = function(filename1, filename2, skulist)
{
  # read in order level, setkey by sku, and set NA = 0
  orderLevel = fread(filename1)
  setkey(orderLevel, SKU)
  orderLevel[is.na(orderLevel)] = 0
  
  # read in forecast, setkey by sku, and set NA = 0
  forecast = fread(filename2)
  setkey(forecast, SKU)
  forecast[is.na(forecast)] = 0
  
  # subset data with join on list of skus in caseflow locations
  # create empty data frame to hold list of locations to exhaust
  sub = orderLevel[forecast][skulist]
  list = data.table(rep(0 , nrow(sub)))
  
  #sub[is.na(sub)] = 0
  # create new columns for totalopen, shipped, threshold, and %shipped
  # set NA's = 0
  sub [ , totalOpen := MayOpen + JuneOpen]
  sub[ , totalShipped := MayShipped + JuneShipped]
  sub[ , threshold := rep(.75, nrow(sub))][ , percentShipped := totalShipped / (totalOpen + totalShipped)]
  sub[is.na(sub)] = 0
  
  # loop through sub to find if %shipped > threshold.
  # if so, set decision to 1.
  # return list of skus with decision == 1
  for(i in 1: nrow(sub))
  {
    if(sub[i, percentShipped] > sub[i , threshold])
    {
      sub[i , decision := 1]
    }
    else
    {
      sub[i , decision := 0]
    }
  }
  list = sub[decision == 1]
}