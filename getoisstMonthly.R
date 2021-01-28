######################################################################################################
# Get monthly oisst using daily data saved in annual netcdfs
# https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html
######################################################################################################

getoisstMonthly <- function(myYr, myMo, myBounds, myPath) {
  # Month must be 2 characters
  # Open the correct netcdf. These were downloaded from the website above, and named 
  # as below. There's one file for each year, as they are large
  # Domain extracted is 0-60N and 140-250E, which is reflected in the filenames.
  dataPath <- paste0(myPath, "./oisst_060N_140250E_", myYr, ".nc")
  sstFile <- nc_open(dataPath)
  sstLon <- ncvar_get(sstFile, "lon")
  sstLat <- ncvar_get(sstFile, "lat")
  sstTime <- ncvar_get(sstFile, "time")
  sstTimDate <- as.Date(as.POSIXct(sstTime*86400, origin = "1800-01-01"))
  sstMoYr <- as.data.frame(cbind(mo = month(sstTimDate), yr = year(sstTimDate)))
  sstIndex <- which(sstMoYr$mo == myMo & sstMoYr$yr== myYr) 
  sst <- ncvar_get(sstFile, "sst", start = c(1, 1, min(sstIndex)), 
                   count = c(length(sstLon), length(sstLat), length(sstIndex))) # lon, lat, time
  nc_close(sstFile)
  # Convert to long format
  dimnames(sst) <- list(lon = sstLon, lat = sstLat, date = as.Date(sstTimDate[sstIndex]))
  sstLong <- melt(sst, value.name = "sst")
  sstLong$date <- as.Date(sstLong$date, origin = "1970-01-01") # melt forces dates into integers
  # Subset based on myBounds. Lon is lon360 in OISST
  sstLongSub <- subset(sstLong, sstLong$lon >= myBounds[1] & sstLong$lon <= myBounds[2] & 
                         sstLong$lat >= myBounds[3] & sstLong$lat <= myBounds[4])
  sstLongSub$lonrd <- sstLongSub$lon # leaving at native 0.25 res here, but could change
  sstLongSub$latrd <- sstLongSub$lat
  sstAgg <- aggregate(sst ~ lonrd + latrd, sstLongSub, FUN = mean, na.rm = TRUE)
  sstAgg$month <- myMo
  sstAgg$year <- myYr
  return(sstAgg)
}
