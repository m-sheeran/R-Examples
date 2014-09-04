## Function to calculate the mean observed value of a pollutant across ##
## a specified list of monitors                                       ##

pollutantmean = function (directory, pollutant, id = 1:332)
{
   
  dat = list.files(directory, full.names = TRUE)
  data = data.frame()
    
  ## Fill empty data frame with data from id selection
  for (i in id)
  {
    data = rbind(data, read.csv(dat[i]))
              
  }         
      
  # Subset data for specified pollutant
   pol_subset = data[ , pollutant]
   
 
  ## Calculate pollutant mean from data subset
  pol_mean = mean(pol_subset, na.rm = TRUE)
  
  ## Return pollutant mean
  pol_mean
  
}
