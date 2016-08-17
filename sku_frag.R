# Determine the total SKU fragmentation in Active Pick Mod. We define fragmentation as the same SKU being in multiple Active Pick 
# locations. Sometimes this is by design, e.g., slotting the same backpack or shoe into multiple carton flow locations in order to 
# absorb the impact on the pick mod. This is to measure impact of fragmentation in pick mod.

# Additionally, we break out the fragmentation measure to account for two scenarios - With multi-SKU locations included, and without
# including multi-SKU locations, by filtering off the determination zones.

sku_frag = function(activeLocations)
{
  
  require(data.table)
  require(dplyr)
  
  # Read in all Active Pick locations
  totalLocations = fread(activeLocations)
  
  # Select in only data we want from CSV
  totalLocations = totalLocations %>% 
                   select(SKU:DET_ZONE)
  
  #Excluding multi-SKU locations
  nomulti = totalLocations %>% 
            filter(DET_ZONE != 'RTN') %>% 
            summarise(total.locations = n(), distinct.sku = n_distinct(SKU))
  
  
  
  # Reduce Multi-SKU locations to only those with non-zero inventory, combine with non-Multi-SKU locations
  # then summarize for total fragmentation
  
  multi_non_zero = totalLocations %>%
                   filter(DET_ZONE == 'RTN' & UNITS != 0)
  
  shf_locs = totalLocations %>%
             filter(DET_ZONE != 'RTN')
  
  multi = bind_rows(multi_non_zero, shf_locs) 
  
  multi = multi %>%
          summarise(total.locations = n(), distinct.sku = n_distinct(SKU))

  
  
  # First row is without multi-SKU locations, second row is with multi-SKU locations
  totalFrag = bind_rows(nomulti, multi)
  
  totalFrag = totalFrag %>% 
              mutate(fragmentation = 1 - (distinct.sku / total.locations))
  
  frag_type = data.table(type = c("No multi-SKU", "Multi-SKU"))
  
  totalFrag = bind_cols(frag_type, totalFrag)
  
  return(totalFrag)
}
