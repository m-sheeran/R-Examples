# Function to compare number of complete cases in monitor id
# vs a min threshold. If >= threshold, compute correlation
# between Nitrate and Sulfate readings for monitor id
# Requries the use of complete.R found in the pollutant set
# folder


corr = function(directory, threshold = 0)
  {
    # Enumerate files and create empty data structures
    dat = list.files(directory, full.names = TRUE)
    data = data.frame()
    cor_data = vector()
    
    # Get complete cases from data files
    comp_cases = complete(directory, 1:332)
   
    # Compare number of complete observations against min threshold 
    # If greater than threshold, compute correlation between
    # Nitrate and Sulfate readings
    for( i in 1:332)
    {
      if(comp_cases$nobs[i] >= threshold)
      {
        data = read.csv(dat[i])
        cor_data = c(cor_data, cor(data[ ,"nitrate"], data[ , "sulfate"], 
                      use = "complete"))
      }
        
    }
    # Return correlation data
    cor_data
  
  }
