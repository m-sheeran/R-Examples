# Determine the total SKU fragmentation in Active Pick Mod. We define fragmentation as the same SKU being in multiple Active Pick 
# locations. Sometimes this is by design, e.g., slotting the same backpack or shoe into multiple carton flow locations in order to 
# absorb the impact on the pick mod. More often than not, we run into instances of the same SKU being in 15 different shelving locs.

# Additionally, we break out the fragmentation measure to account for two scenarios - With multi-SKU locations included, and without
# including multi-SKU locations, by filtering off the determination zones.

sku_frag = function(activeLocations)
{
  
  require(data.table)
  require(dplyr)
  
  #read in all Active Pick locations
  totalLocations = fread(activeLocations)
  
  #select in only data we want from CSV
  totalLocations = totalLocations %>% select(SKU:DET_ZONE)
  
  #Excluding multi-SKU locations
  nomulti = totalLocations %>% filter(DET_ZONE != 'RTN') %>% summarise(total.locations = n(), distinct.sku = n_distinct(SKU))
  
  
  
  
  multi = totalLocations %>% summarise(total.locations = n(), distinct.sku = n_distinct(SKU))
  
  
  #First row is without multi-SKU locations, second row is with multi-SKU locations
  totalFrag = rbind(nomulti, multi)  
  
  totalFrag = totalFrag %>% mutate(fragmentation = 1 - (distinct.sku / total.locations))
  
  frag_type = data.table(type = c("No multi-SKU", "Multi-SKU"))
  
  totalFrag = cbind(frag_type, totalFrag)
  
  return(totalFrag)
}