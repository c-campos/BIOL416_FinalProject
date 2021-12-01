# Campos Final Project

setwd('/Users/carloscampos/Desktop/HoneycreeperProjects/Data')

# use 'lapply' to load all of the packages I will need at once

X <- c("tidyverse", "measurements", "rgdal", "rgeos", "sp", "pbapply", "data.table", "scales", "ggsn", "ggrepel", "readxl")
invisible(lapply(X, library, character.only = TRUE)) # use 'invisible' to hide outcome - its long

### Importing data about the samples obtained----

infection_data <- as.data.frame(read_xlsx(path = 'infection_data.xlsx', sheet = 'infection_data'))
  head(infection_data)

# convert 'infection_status' to binary 0 = negative and 1 = positive using an 'if...else' within a forloop
  infection_binary <- matrix(nrow = 129, ncol =1)
  
  for(i in 1:length(infection_data$infection_status)){
    if(infection_data$infection_status[i] == 'negative'){
      infection_binary[i] <- print(0)} else if(infection_data$infection_status[i] == 'positive'){
        infection_binary[i] <- print(1) }
  }
  
# use 'cbind' to merge the two data storage objects together
  infection_data <- cbind(infection_data, infection_binary)
  
# create a function to change the format of 'Date Sampled' to a more accepted format
  date <- function(date){
    y <- gsub(pattern = '\\.', replacement = '/', x = date)
    
    new.date <- format(as.Date(x = y, format = '%m/%d/%y'), format = '%d-%b-%Y')
    
    return(new.date)
  }
# use 'lapply' to change the values in 'Date Sampled' to the corrected format
  infection_data$`Date Sampled` <- lapply(X = infection_data$`Date Sampled`, FUN = date)
  
# sort the data by Island because i like it better that way
  infection_data <- infection_data[order(infection_data$Island), ]

# Transform the dataset from long to wide by 'Island' to see if this is better for visualization of site occurrences
  infection_data_w <- spread(data = infection_data, key = Island, value = Site)
    
    # not quite what i thought so I will try dcast()
  infection_data_site_counts <- dcast(data = infection_data, formula = Island~Site)
    # this looks pretty cool for visualizing sample counts by site

# I like the way the site counts look in the wide format so I will export
  write_csv(x = infection_data_site_counts, file = 'sample_site.counts.csv')

  ##### Now try creating a sample map of rthe big Island
  
# import the data set that contains the coordinate info for all sites on all islands
amak_data <- as.data.frame(read_xlsx(path = 
                                       'amakihi_samplelist_RNADNA_spatial_allislands-2.xlsx', sheet = 'sites'))

head(amak_data)
str(amak_data) # looks good!

# Index the columns that I need
amak_lat_long <- amak_data[ , c('Island', 'Site', 'Latitude', 'Longitude')]

# Subset the data by island
HAW_dat <- subset(x = amak_lat_long, subset = Island == 'Hawaii')

MAU_dat <- subset(x = amak_lat_long, subset = Island == 'Maui') 
  # there are repeat coordinates in this grouping so I removed them
    MAU_dat <- MAU_dat[MAU_dat$Site == 'KulaFR' | MAU_dat$Site == 'Nakula' | MAU_dat$Site == 'WAIK' | MAU_dat$Site == 'GardenOfEden' | MAU_dat$Site == 'Waihee', ] 

KAU_dat <- subset(x = amak_lat_long, subset = Island == 'Kauai')  

# now all of the data for each island is sorted

# read in the underlying shapefile that I will use as the base of my map

hawaii_full <- rgdal::readOGR(dsn = 'Elevation_Ranges/')
buf <- 0.15

# plot the Hawaii data points ----

jpeg('HAAM_sampleMap.jpeg', units = 'cm', width = 30, height = 30, res = 300 )

ggplot() +
  
  geom_polygon(data = hawaii_full, aes(x = long, y = lat, group = group), color = 'black', fill = 'grey88', size = 0.25) +
  
  geom_point(data = HAW_dat, aes(x = Longitude, y = Latitude), color = 'red', size = 2.5, fill = 'red') +
  
  coord_fixed(ratio = 1, xlim = c(min(HAW_dat$Longitude) - buf, max(HAW_dat$Longitude) + buf), ylim = c(min(HAW_dat$Latitude) - buf, max(HAW_dat$Latitude) + buf)) +
  
  geom_label_repel(data = HAW_dat, aes(x = Longitude, y = Latitude, label = Site), size = 5) +
  
  theme(panel.background = element_rect('white'), panel.grid.major = element_line('gray')) +
  
  scalebar(data = NULL, dist = 10, dist_unit = "km", transform = TRUE, height = 0.3, st.dist = 0.3, st.bottom = TRUE, st.size = 5, model = "WGS84", x.min = -154.9, x.max = -154.8 , y.min = 20.0 , y.max = 20.1) +
  
  north(data = NULL, scale = 1.25 , symbol = 3, x.min = -154.92, x.max = -154.73 , y.min = 20.1 , y.max = 20.2) +
  
  xlab('Longitude') + ylab('Latitude')

dev.off()
