# Function to read in list of reserve SKUS and relevant summaries for Outside Storage.
# Need allocation detail list in order to compute percentage of inventory that is "excess"
# 
move_list = function(reserve_list, allocation_detail)
  
{
  # Require data.table for fread function and fast select of columns
  require(data.table)
  
  # Create data.tables
  reserve = fread(reserve_list)
  alloc_detail = fread(allocation_detail)
  
  # Select only relevant columns since exporting data from xlsx to csv may add in additional empty columns
  # .() in data.table is an alias to list
  # DESCRIP is SKU description, WALNUT_RESERVE is number of units in reserve, PALLETS is # of pallets in reserve
  reserve = reserve[ , .(SKU, DESCRIP, WALNUT_RESERVE, PALLETS)]
  
  # Set key by SKU then left join on reserve list since SKU in Reserve may not have allocations yet
  setkey(reserve, SKU)
  setkey(alloc_detail, SKU)
  
  moves_list = merge(reserve, alloc_detail, all.x = TRUE)
  
  # Set NAs to 0 (mostly entries in allocation columns)
  
  moves_list[is.na(moves_list)] = 0
  return(moves_list)
  # Write to csv
}