# filename1 is for Order Level, filename2 is for Forecast. skulist is list of
# skus currently in permanent caseflow locations. Use query for list, load into
# dataframe in R workspace.

delta = function(filename1, filename2, skulist)
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
  sub1 = merge(orderLevel, forecast)
  sub2 = merge(sub1, skulist)
  
  #sub = orderLevel[forecast][skulist]
  
  list = data.table(rep(0 , nrow(sub2)))
  
  #sub[is.na(sub)] = 0
  # create new columns for totalopen, shipped, threshold, and %shipped
  # set NA's = 0
  sub2[ , totalOpen := JuneOpen + JulyOpen]
  sub2[ , totalShipped := JuneShipped + JulyShipped]
  #sub2[ , threshold := rep(.75, nrow(sub2))][ , percentShipped := totalShipped / (totalOpen + totalShipped)]
  sub2[is.na(sub2)] = 0
  
  # loop through sub to find if %shipped > threshold.
  # if so, set decision to 1.
  # return list of skus with decision == 1
 # for(i in 1: nrow(sub2))
  #{
   # if(sub2[i, percentShipped] > sub2[i , threshold])
    #{
     # sub2[i , decision := 1]
    #}
    #else
    #{
     # sub2[i , decision := 0]
    #}
  #}
  #list = sub2[decision == 1]
 sub2
}