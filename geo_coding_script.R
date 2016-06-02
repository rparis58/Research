# Google geocoding API expects address in format as defined by the national post office
# for example see http://www.bitboost.com/ref/international-address-formats/new-zealand/
# from what i can see it doesnt make use of the post code.
#
# example input.csv contents
# Address
# 173 Park Road Johnsonville Wellington
# 5 Lawry St Ellerslie Auckland
#
# NOTE: 
# 1. this script expects 'data' subdirectory for it to write out RData and CSV version of the geocoded data
# 2. for non-existent addresses it absracts out to region etc.
# 3. seems to return a result for non existent street number (assuming street is correct etc)

options(stringsAsFactors = FALSE)
setwd("~/Projects/R/Research")

#load up the ggmap library
library(ggmap)
# get the input data
infile <- "input"
data <- read.csv(paste0('./', infile, '.csv'), header = TRUE)

# get the address list, and append "New Zealand" to the end to increase accuracy 
# (change or remove this if your address already include a country etc.)
addresses = data$Address
addresses = paste0(addresses, ", New Zealand")

#define a function that will process googles server responses for us.
getGeoDetails <- function(address){   
  #use the gecode function to query google servers
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

#### MAIN PART OF THE SCRIPT ####
#initialise a dataframe to hold the results
geocoded <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 1
#if a temp file exists - load it up and count the rows!
tempfilename <- paste0(infile, '_temp_geocoded.rds')
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

# the full results in in 'geocoded' data.frame.  Copy lat/long and accuracy into the main 'data' frame...
data$lat <- geocoded$lat
data$long <- geocoded$long
data$accuracy <- geocoded$accuracy

#finally write the 'data' frame to the output files
saveRDS(data, paste0("data/", infile ,"_geocoded.rds"))
write.table(data, file=paste0("data/", infile ,"_geocoded.csv"), sep=",", row.names=FALSE)

#now we have finished...remove the temp file
if (file.exists(tempfilename)) file.remove(tempfilename)
