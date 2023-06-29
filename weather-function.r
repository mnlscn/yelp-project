# Function to extract weather data
extractweather = function(dataset, mindate = min(dataset$date), maxdate = max(dataset$date),
                          latrange = range(dataset$business_lat), longrange = range(dataset$business_long),
                          resol = 0.5, getdata = FALSE,
                          wear = ifelse("weatherPRCPSNWDSNOWTMAXTMINTOBS.RData" %in% list.files(), "available", "navailable")) {
  # Check if weather data is already available
  wdatacond = wear == "navailable"
  if (getdata | wdatacond) {
    require("doParallel")
    
    # Set up parallel processing
    cl <- makeCluster(detectCores(), outfile = "log1.txt")
    registerDoParallel(cl)
    
    # Read station names and coordinates from NOAA dataset
    stations = read.delim(url("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt"),
                          header = FALSE, quote = "", sep = "")[, 1:3]
    colnames(stations) = c("Station", "lat", "long")
    stations = stations[strtrim(stations$Station, 2) == "US", ]
    stations$lat = as.numeric(stations$lat)
    stations$long = as.numeric(stations$long)
    stations = stations[!is.na(stations$lat) | !is.na(stations$long), ]
    
    # Generate latitude and longitude sequences based on specified resolution
    latseq = c(seq(latrange[1], latrange[2], by = resol), latrange[2])
    longseq = c(seq(longrange[1], longrange[2], by = resol), longrange[2])
    
    wear = NULL
    k = 0
    torunlist = NULL
    for (lat in 1:(length(latseq) - 1)) {
      for (lon in 1:(length(longseq) - 1)) {
        k = k + 1
        torunlist = rbind(torunlist, c(lat, lon))
      }
    }
    
    # Parallel loop to extract weather data for each grid cell
    wear = foreach(i = 1:k, .noexport = ls(), .export = c("latseq", "longseq", "stations", "torunlist", "mindate", "maxdate")) %dopar% {
      lat = torunlist[i, 1]
      lon = torunlist[i, 2]
      
      # Determine latitude and longitude ranges for the grid cell
      rangelat = c(latseq[lat + 1], latseq[lat])
      rangelong = c(longseq[lon], longseq[lon + 1])
      
      # Find stations within the grid cell's range
      indx = (stations$lat > rangelat[2]) & (stations$lat < rangelat[1]) &
             (stations$long > rangelong[1]) & (stations$long < rangelong[2])
      stations_temp = stations[indx, ]
      stations_t = paste(stations_temp$Station, collapse = ",")
      
      # Build the API query URL to retrieve weather data
      temp = paste0("dataset=daily-summaries&dataTypes=PRCP,SNWD,SNOW,TMAX,TMIN,TOBS",
                    "&stations=", stations_t,
                    "&startDate=", mindate, "",
                    "&endDate=", maxdate)
      
      # Check if the URL is valid and retrieve the weather data
      valid_url <- TRUE
      a = tryCatch(read.csv(url(paste0("https://www.ncei.noaa.gov/access/services/data/v1?", temp))),
                   error = function(e) { valid_url <<- FALSE })
      toreturn = NULL
      if (valid_url)
        toreturn = list(range = cbind(rangelat, rangelong), data = read.csv(url(paste0("https://www.ncei.noaa.gov/access/services/data/v1?", temp))))
      
      print(c(lat, lon, valid_url))
      return(toreturn)
    }
    
    stopCluster(cl)
    
    # Save the extracted weather data to a file
    save(file = "weatherPRCPSNWDSNOWTMAXTMINTOBS.RData", list = c("wear"))
  } else {
    # If weather data is already available, load it from file
    if (wear == "available") {
      load("weatherPRCPSNWDSNOWTMAXTMINTOBS.RData")
    }
  }
  
  return(wear)
}

# Function to convert extracted weather data into daily level data
weardailyavg = function(wear) {
  if ("weather_data.RData" %in% list.files()) {
    load(file = "weather_data.RData")
  } else {
    require("doParallel")
    
    # Set up parallel processing
    cl <- makeCluster(detectCores())
    registerDoParallel(cl)
    clusterCall(cl, function(x) { library(dplyr) })
    
    wear_avg = NULL
    k = 0
    
    # Parallel loop to calculate daily averages for each weather data grid cell
    wear_avg = foreach(i = 1:length(wear), .noexport = ls(), .export = c("wear"), .packages = c("dplyr")) %dopar% {
      if (is.null(wear[[i]])) {
        temp = NULL
      } else {
        temp = wear[[i]]$data %>%
          group_by(DATE) %>%
          summarize(PRCP = mean(PRCP, na.rm = TRUE),
                    SNOW = mean(SNOW, na.rm = TRUE),
                    SNWD = mean(SNWD, na.rm = TRUE),
                    TMAX = mean(TMAX, na.rm = TRUE),
                    TMIN = mean(TMIN, na.rm = TRUE),
                    TOBS = mean(TOBS, na.rm = TRUE))
        temp = list(range = wear[[i]]$range, data = temp)
      }
      return(temp)
    }
    
    stopCluster(cl)
    
    weather = NULL
    k = 0
    
    # Process the results and store weather data for each day
    for (i in 1:length(wear_avg)) {
      if (is.null(wear[[i]]))
        next
      
      k = k + 1
      weather[[k]] = wear_avg[[i]]
      weather[[k]]$data$DATE = as.Date(weather[[k]]$data$DATE)
    }
    
    # Save the processed weather data to a file
    save(file = "weather_data.RData", list = c("weather"))
  }
  
  return(weather)
}
