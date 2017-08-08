####loading in packages and data####
library(ggmap)
library(geosphere)
#setting working directory 
setwd("C:/Users/Ianpe/Downloads/DResSup/GGMAP/")
#loading in data 
tourist_dest<-read.csv("tourist_destinations.csv")
sdata<-read.csv("sampledata1.csv")

####creating a addresses from tourist list####
infile <- "input"
addresses<-as.character(tourist_dest$address)#making sure that addresses are treated as strings or as characters

####actual geocoding####
geocode ("Young Research Library UCLA")#example of how the geocode command works in the GGMAP package

#define a function that will process Google's server responses for us.
getGeoDetails <- function(address){   
  #use the gecode function to query Google servers
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}

#initialize a dataframe to hold the results
geocoded <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 1
#if a temp file exists - load it up and count the rows!
tempfilename <- paste0(infile, '_temp_geocoded_attorney.rds')
if (file.exists(tempfilename)){
  print("Found temp file - resuming from index:")
  geocoded <- readRDS(tempfilename)
  startindex <- nrow(geocoded)
  print(startindex)
}

# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
for (ii in seq(startindex, length(addresses))){
  print(paste("Working on index", ii, "of", length(addresses)))
  #query the google geocoder - this will pause here if we are over the limit.
  result = getGeoDetails(addresses[ii]) 
  print(result$status)     
  result$index <- ii
  #append the answer to the results file.
  geocoded <- rbind(geocoded, result)
  #save temporary results as we are going along
  saveRDS(geocoded, tempfilename)
}

# Checking out what our results

View(geocoded)
print(geocoded)

#merging back together
tourist_dest2 <- merge(tourist_dest, geocoded, by =("index"), all=F)

#quick maps
##making map based on Google map of Los Angeles
map_LA<- get_map(location='Los Angeles', zoom=10, maptype = "terrain", source='google',color='color') #might have to alter zoom 
ggmap(map_LA)+ geom_point(aes(x=Long, y=Lat, color =Type), data=sdata, na.rm = T)#plotting points from Sdata 
ggmap(map_LA)+ geom_point(aes(x=long, y=lat, size =1, color =cost ), data=tourist_dest2, na.rm = T) #plotting points from tourist data 

####coming up with distance matrix###
set1 <- subset(sdata, select = c(Name, Lat, Long) )#sub-setting sample data w/addresses
set2 <- subset(tourist_dest2, select = c(tourist_site,index, lat, long) )#sub-setting new tourist destination data
mat <- distm(set1[,c('Long','Lat')], set2[,c('long','lat')], fun=distVincentyEllipsoid)#creating distance matrix
set1$nearest_site<- set2$tourist_site[max.col(-mat)]#adding name of nearest distance tourist site to 
names(set2)<-c("tourist_site","index","lat_ts", "long_ts")#renaming lat and long to have indicators for being tourist sites
match <- merge(set1, set2, by.x=c("nearest_site"), by.y=c("tourist_site"), all=F)
match$begin_location<-paste0(as.character(match$Lat),"  ,  ", as.character(match$Long)) #changing long and lattitude to character for start location
match$end_location<-paste0(as.character(match$lat_ts),"  ,  ", as.character(match$long_ts))#changing long and lat to character for destination

####Using Google Maps to get driving distance between places####
#(to me this was more practical than thinking in terms of long and lat coordinates or cartesian space etc)

#testing out mapdist function
mapdist ("1311 South 550 East Springville Ut 84663", "Young Research Library UCLA", mode= "driving")

distance_google_maps<-mapdist(match$begin_location,match$end_location,mode = 'driving')#calculating for all addresses
match_distance<-merge(match, distance_google_maps, by.x=c("begin_location", "end_location"), by.y=c("from","to"), all=F)

####creating other maps  ####
final <- merge(sdata, match_distance, by =c("Name", "Lat", "Long"), all=F)#merging everything
final<-final[!duplicated(final), ]#getting rid of duplicates

ggmap(map_LA)+ geom_point(aes(x=Long, y=Lat, size=miles, color =Type), 
                          data=final, na.rm = T) #making map colored by type w/size that is proportional to distance to nearest tourist attraction

####saving data frame as a csv####
write.csv(final, file = "GGMAP_example_data_set.csv")
