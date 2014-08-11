# Function to list number of complete cases and 
# associated monitor id in Specdata


complete = function(directory, id = 1:332)
  {
    # Enumerate files and create empty data frames
    dat = list.files(directory, full.names = TRUE)
    data = data.frame()
    nobs.frame = data.frame()
    id.frame = data.frame()
    complete.frame = data.frame()
    
    for (i in id)
      {
        data = read.csv(dat[i])
    
        # Fill id.frame with associated file id's
        monitor.id = i
        id.frame = rbind(id.frame, monitor.id)
        colnames(id.frame) = "id"
      
        # Sum total complete cases per monitor id
        comp= complete.cases(data)
        nobs = sum(comp)
        nobs.frame = rbind(nobs.frame, nobs)
        colnames(nobs.frame) = "nobs"
      }
    
    # Column merge both id and observation dataframes
    complete.frame = cbind(id.frame, nobs.frame)
    complete.frame
    
  }