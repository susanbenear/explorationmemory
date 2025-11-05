#### Noise removal ####
# Takes input of raw GPS datafiles and outputs noise removed and per minute files with and without interpolation

##############################################################################
########### run strip_spaces_infilenames.py before running this #############
##############################################################################

library(fields)
library(plyr)
library(dplyr)
library(tidyverse)
library(hms)
library(fs)     

# Define the directories

# sample input dir
#input_dir <- "/Users/susanbenear/Library/CloudStorage/GoogleDrive-susanbenear@gmail.com/My Drive/NYU/MemGeoVal Project/MemGeo Study/Data/GPS Data/sample_files"
# real input dir
input_dir <- "/Users/susanbenear/Library/CloudStorage/GoogleDrive-susanbenear@gmail.com/My Drive/NYU/MemGeoVal Project/MemGeo Study/Data/GPS Data/gps_csv_files_7.16.25"
empty_file_dir <- "/Users/susanbenear/Library/CloudStorage/GoogleDrive-susanbenear@gmail.com/My Drive/NYU/MemGeoVal Project/MemGeo Study/Data/GPS Data/empty_files"

# Create the output directory dynamically
today_date <- format(Sys.Date(), "%Y-%m-%d")  # Get today's date in YYYY-MM-DD format
out_dir_permin <- file.path(input_dir, paste0("gps_permin_", today_date))
out_dir_filtered <- file.path(input_dir, paste0("gps_filtered_", today_date))

# Get the list of all CSV files in the input directory
file_list_full <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

# get rid of any files with no data in them
# Loop through each file
for (file in file_list_full) {
  # Check if the file exists and its size
  file_info <- file.info(file)
  
  if (file_info$size == 0) {
    # Move completely empty files to the empty files directory
    file_move(file, file.path(empty_file_dir, basename(file)))
    message(paste("Moved completely empty file:", file))
    next
  }
  
  # Try to read the file
  data <- tryCatch({
    read.csv(file, nrows = 5)  # Read only the first few rows to minimize memory usage
  }, error = function(e) {
    # Move unreadable file to the output directory
    message(paste("Unreadable file:", file, "-", e$message))
    file_move(file, file.path(empty_file_dir, basename(file)))
    NULL
  })
  
  # If the file was moved because it was unreadable, continue to the next one
  if (is.null(data)) next
  
  # Check if the file contains only headers
  if (nrow(data) == 0 && ncol(data) > 0) {
    # Move files with only headers to the output directory
    file_move(file, file.path(empty_file_dir, basename(file)))
    message(paste("Moved file with only headers:", file))
  }
}


# Make a list of files only with the file name rather than the full path to the file
file_list <- list.files(input_dir, full.names = FALSE)
# Print the length of the file list (# of files)
#print(length(file_list))

date_list <- substr(file_list,10,19)
date_list = date_list[order(as.Date(date_list, format="%Y-%m-%d"))]
subj_list <- substr(file_list,1,8)
subj_list <- subj_list[!duplicated(subj_list)]
start_date <- min(date_list)
end_date <- max(date_list)
DaysVec <- seq(as.Date(start_date), as.Date(end_date), by="days")


#### Noise removal parameters ####

decimal <- 4
minimum_distance <- .05
largest_angle <- .4
mins_between <- 5

#### Noise refilmoval loop for each file in file_list ####

for(a in 1:length(file_list)){
  date1 <- substr(file_list[a],10,19)
  #date1 <- date_list[a]
  subj <- substr(file_list[a],1,8)
  
  ftable <- read.csv(file_list_full[a]) # Read in file for participant (single day's file).
  if (ftable[1,1] == "No data returned for your query" || ftable[1,1] == "DeviceID not valid for your user name. Or your device is in free license and API does not work with free license" || nrow(ftable)==0){
    next()
  }
  else{
    
  pointtable = ftable[,2:3] # just lat, lon
  time = ftable[,1]
  time = gsub(" ", "T", time) 
  time = sub(".*T", "", time)
  time = sub("-.*", "", time)
  time = as_hms(time)
  
  
  #### Remove location points at small angles ####
  
  b <- 2 #start at 2nd row of location data
  while(b<(nrow(ftable))){
    p12 <- rdist.earth(cbind(pointtable[b-1,2], pointtable[b-1,1]),cbind(pointtable[b,2], pointtable[b,1])) #distance between this location and previous location ()
    p23 <- rdist.earth(cbind(pointtable[b,2], pointtable[b,1]),cbind(pointtable[b+1,2], pointtable[b+1,1])) #distance between this location and next location ()
    p13 <- rdist.earth(cbind(pointtable[b-1,2], pointtable[b-1,1]),cbind(pointtable[b+1,2], pointtable[b+1,1])) #distance between previous and next location (A)
    angle <- (p12^2+p23^2-p13^2)/(2*p12*p23) #angle at b location (angle between prev, current, next location)
    if(is.na(angle)){
      b <- b+1
      next()
    }
    if(angle > 1){
      angle <- 1}
    if(angle < -1){
      angle <- -1}
    if(acos(angle)<largest_angle && difftime(time[b+1], time[b-1], units='mins') < mins_between){ #if the angle is smaller than .4 radians (22.91 degrees), in radians because curvature of earth & < 5 minutes between prev and next point
      ftable <- ftable[-(b),] #remove this location point 
      pointtable <- pointtable[-(b),] 
    }
    else {
      b <- b+1
    }
  }

  
  #### Instead of removing close points as before, remove consecutive duplicates, keeping first instance ----
  
  #lat_lon = paste(pointtable[,'Data.Latitude'], pointtable[,'Data.Longitude'])
  lat_lon = paste(pointtable[,'Latitude'], pointtable[,'Longitude'])
  dup <- rle(lat_lon)
  pointtable = pointtable[cumsum(c(1, dup$lengths[-length(dup$lengths)])),] 
  ftable = ftable[cumsum(c(1, dup$lengths[-length(dup$lengths)])),]
  
  ftable[,'Time'] = substr(ftable[,1],12,16)
  pointtable = pointtable[!duplicated(ftable[,c('Time')]),]
  ftable = ftable[!duplicated(ftable[,'Time']),]
  
  
  #### Round to decimal ####
  
  LatVec <- round(as.numeric(pointtable[,1]), digits = decimal) #round to decimal stated at beginning of script
  LonVec <- round(as.numeric(pointtable[,2]), digits = decimal) 
  DateVec <- substr(ftable[,1],1,10) #get just date
  TimeVec <- substr(ftable[,1],12,19) #get just time
  DateTimeVec <- substr(ftable[,1],1,19) #time and date

  positionMatrix = matrix(
    c(LatVec,LonVec),
    nrow=length(LatVec),
    ncol=2) #new positionMatrix with rounded lat/lon
  
   if (a == 1) { # Initialize filtered data frame on first iteration
     all_gps_filtered <- data.frame(matrix(ncol = 5, nrow = 0))
     colnames(all_gps_filtered) <- c("date", "time", "lat", "lon", "device_id")
   }

   # Capture the filtered data after noise removal and duplicate removal
   filtered_data <- data.frame(
     date = substr(ftable[, 1], 1, 10),
     time = substr(ftable[, 1], 12, 19),
     lat = round(as.numeric(pointtable[, 1]), digits = decimal),
     lon = round(as.numeric(pointtable[, 2]), digits = decimal),
     device_id = subj
   )

   # Append the filtered data to the all_gps_filtered data frame
   all_gps_filtered <- rbind(all_gps_filtered, filtered_data)

  
   
   
   
   
   
  #### Get location per minute and interpolate over gaps ####
  
  # Count the number of data points for the current date ----
  # Check for daylight savings.
  DateTimeVec = gsub(" ", "T", DateTimeVec) # need this for the weekly downloaded files
  dataLocation <- character(2000) # Preallocating for max possible number of trackpoints in a day.
  f <- 1
  for (t in 1:length(DateVec)){
    if(DateVec[t]==date1){
      dataLocation[f] <- t
      f <- f +1
    }
    if(DateVec[t]>date1)
      break
  } # count of the number of location points for the current date
  dataLocation <- dataLocation[!dataLocation %in% ""] #get rid of blanks
  beginDate <- as.POSIXct(as.character(date1), tz = "EST")
  
  # Create blank matrix with a row for every minute of the day ----
  
  dmsize = 1440 # number of minutes in a day (unless daylight savings)
  dayMatrix = matrix(
    nrow=dmsize,
    ncol=3) #create empty matrix with number of minutes in a day

# removed this chunk and edited below  
  # if daylight savings, need 1500 rows
#  if(difftime(as.POSIXct(DateTimeVec[as.numeric(dataLocation[length(dataLocation)])],format = "%Y-%m-%dT%H:%M:%S", tz = "EST"),
 #             as.POSIXct(as.character(date1), tz = "EST"),units = "mins")>1440){
  #  dmsize <- 1500 # If daylight savings, add on 60 more minutes.
   # dayMatrix = matrix(
    #  nrow=dmsize,
     # ncol=3
  #  )
#  }
 
  ##changed the above chunk to the below chunk to more gracefully handle these issues 
  # - it will skip over the files and print an error message instead of halting the whole script
  if (length(dataLocation) > 0) {
    last_index <- as.numeric(dataLocation[length(dataLocation)])
    
    if (!is.na(last_index) && last_index <= length(DateTimeVec)) {
      time_diff <- difftime(
        as.POSIXct(DateTimeVec[last_index], format = "%Y-%m-%dT%H:%M:%S", tz = "EST"),
        as.POSIXct(as.character(date1), tz = "EST"),
        units = "mins"
      )
      
      if (time_diff > 1440) {
        dmsize <- 1500 # If daylight savings, add on 60 more minutes.
        dayMatrix <- matrix(
          nrow = dmsize,
          ncol = 3
        )
      }
    } else {
      message(paste("Invalid index in dataLocation or insufficient DateTimeVec for file:", file_list_full[a]))
      next  # Skip the problematic file
    }
  } else {
    message(paste("dataLocation is empty for file:", file_list_full[a], "Skipping this file."))
    next  # Skip the problematic file
  }
  
  
  # Place coordinates in proper timeline (each row of a matrix represents a minute of the timeframe of collected data).
  if(length(dataLocation) >= 1){
    
    # Interpolation decision tree ----
    startPoint <- 1
    for(u in 1:length(dataLocation)){
      tTest <- as.POSIXct(DateTimeVec[as.numeric(dataLocation[u])],format = "%Y-%m-%dT%H:%M:%S", tz = "EST") # datetime of this point
      timeInMinutes <- floor(as.numeric(difftime(tTest,beginDate,units = "mins")))+1 #time in number of mins since start of day

      #if first point, fill in any timepoints before that with the same location coordinates
      if(startPoint==1){ 
        for(v in 1:(timeInMinutes-1)){
          dayMatrix[v,] <- c(positionMatrix[as.numeric(dataLocation[u]),],NA)
        }
      }

      #for the final data point fill in the rest of the day with that location coordinates
      if(u==length(dataLocation)&&startPoint<=dmsize){ 
        for(w in startPoint:dmsize){
          dayMatrix[w,] <- c(positionMatrix[as.numeric(dataLocation[u]),],NA)
        }
      }

      #if not first or last point
      if(timeInMinutes-startPoint>0&&u!=1&&u!=length(dataLocation)){ 
        
        # Get previous lat,lon, current lat,lon and difference in time and distance
        previous_lat <- as.numeric(positionMatrix[as.numeric(dataLocation[u])-1,1])
        current_lat <- as.numeric(positionMatrix[as.numeric(dataLocation[u]),1])
        previous_lon <- as.numeric(positionMatrix[as.numeric(dataLocation[u])-1,2])
        current_lon <- as.numeric(positionMatrix[as.numeric(dataLocation[u]),2])
        previous_time <- startPoint-1
        current_time <- timeInMinutes
        time_diff = current_time - previous_time
        dist = rdist.earth(cbind(previous_lon, previous_lat),cbind(current_lon, current_lat))
        
        # does it have the potential to be interpolated? Gap data
        if (time_diff > 10 & dist > .05 & dist < 1 & u<(length(dataLocation)-2)) {
          
          # get next 2 locations and their times 
          next_lats = as.numeric(positionMatrix[c((as.numeric(dataLocation[u])+1):(as.numeric(dataLocation[u])+2)),1])
          next_lons = as.numeric(positionMatrix[c((as.numeric(dataLocation[u])+1):(as.numeric(dataLocation[u])+2)),2])
          next_times = as.POSIXct(DateTimeVec[c((as.numeric(dataLocation[u])+1):(as.numeric(dataLocation[u])+2))],format = "%Y-%m-%dT%H:%M:%S", tz = "EST") # datetime of this point
          nextT_mins <- floor(as.numeric(difftime(next_times,beginDate,units = "mins")))+1

          # calculate average velocity between current point and next two datapoints
          max_time = max(nextT_mins[1]-current_time, nextT_mins[2]-nextT_mins[1])
          velocitylat_1 = abs((next_lats[1]-current_lat)/(nextT_mins[1]-current_time))
          velocitylat_2 = abs((next_lats[2]-next_lats[1])/(nextT_mins[2]-nextT_mins[1]))
          velocitylon_1 = abs((next_lons[1]-current_lon)/(nextT_mins[1]-current_time))
          velocitylon_2 = abs((next_lons[2]-next_lons[1])/(nextT_mins[2]-nextT_mins[1]))
          avg_lat_velocity = mean(c(velocitylat_1, velocitylat_2))
          avg_lon_velocity = mean(c(velocitylon_1, velocitylon_2))
          
          # Get velocity if travelling constantly between previous coord and current coord 
          vel_cp_lat = abs((current_lat-previous_lat)/(current_time-previous_time))
          vel_cp_lon = abs((current_lon-previous_lon)/(current_time-previous_time))
          
          #conditions under which we cannot interpolate - if the distance per minute is longer than the distance interpolating over (interpolation will overshoot) OR if we don't have enough points to calculate velocity - there is another gap in the next datapoints
          if ((avg_lat_velocity > abs(current_lat - previous_lat) & avg_lon_velocity > abs(current_lon - previous_lon))| max_time > 10){ 
            for(z in (current_time-1):startPoint){
              dayMatrix[z,] = c(previous_lat,previous_lon,'bothnone-overshootorgap')
            }
            
          #otherwise, if we can interpolate in some way
          } else{
            
          # if the post-gap velocity is slower than the speed it would take to travel the distance in both, interpolate in the old way
          if (avg_lat_velocity < vel_cp_lat & avg_lon_velocity < vel_cp_lon){
            avg_lat_velocity <- (current_lat-previous_lat)/(current_time-previous_time) # get signed velocity so it is drawn in the correct direction
            avg_lon_velocity <- (current_lon-previous_lon)/(current_time-previous_time)
            for(z in startPoint:(timeInMinutes-1)){ #fill in points between previous and current location, according to assumed velocity between locations
              dayMatrix[z,1] <- round((avg_lat_velocity*(z-previous_time)+previous_lat), digits = decimal)
              dayMatrix[z,2] <- round((avg_lon_velocity*(z-previous_time)+previous_lon), digits = decimal)
              dayMatrix[z,3] = 'old-slow'
            }
            
            
          # if ave lat speed is greater than constant movement between previous and current, lat should be interpolated in new way but lon in old way
          } else if (avg_lat_velocity >= vel_cp_lat & avg_lon_velocity < vel_cp_lon){ 
            for(z in (current_time-1):startPoint){ #fill in points between current BACKWARDS, according to assumed velocity until you get to the previous location
              lat_back = ifelse(current_lat>previous_lat,round((current_lat-avg_lat_velocity*(current_time-z)), digits = decimal), 
                                round((current_lat+avg_lat_velocity*(current_time-z)), digits = decimal))
              
              if (abs(current_lat-lat_back) < abs(current_lat-previous_lat)){ # keep filling in with the new calculated latitude while the distance is smaller than the total distance to interpolate over
                dayMatrix[z,] <- c(lat_back,NA,'latint-lonold')
              } else if (abs(current_lat - lat_back) >= abs(current_lat-previous_lat)) { # otherwise just fill in with the previous lat
                dayMatrix[z,] = c(previous_lat,NA,'latint-lonold')
              }
            }
            avg_lon_velocity <- (current_lon-previous_lon)/(current_time-previous_time) # for the lon, use the old way
            for(z in startPoint:(timeInMinutes-1)){ #fill in points between previous and current location, according to assumed velocity between locations
              dayMatrix[z,2] <- round((avg_lon_velocity*(z-previous_time)+previous_lon), digits = decimal)
            }
            
          # if lon should be interpolated in new way but lat in old way
          }else if (avg_lat_velocity < vel_cp_lat & avg_lon_velocity >= vel_cp_lon){ 
            for(z in (current_time-1):startPoint){ #fill in points between current BACKWARDS, according to assumed velocity until you get to the previous location
              lon_back = ifelse(current_lon>previous_lat,round((current_lon+avg_lon_velocity*(current_time-z)), digits = decimal),
                                round((current_lon-avg_lon_velocity*(current_time-z)), digits = decimal))
              
              if (abs(current_lon-lon_back) < abs(current_lon-previous_lon)){
                dayMatrix[z,] <- c(NA,lon_back,'latold-lonint')
              } else if (abs(current_lon-lon_back) >= abs(current_lon-previous_lon)) {
                dayMatrix[z,] = c(NA,previous_lon,'latold-lonint')
              }
            }
            avg_lat_velocity <- (current_lat-previous_lat)/(current_time-previous_time)
            for(z in startPoint:(timeInMinutes-1)){ #fill in points between previous and current location, according to assumed velocity between locations
              dayMatrix[z,1] <- round((avg_lat_velocity*(z-previous_time)+previous_lat), digits = decimal)
            }
            
          # if both lat and lon speeds are faster than if you went at a constant speed between previous and current locations, interpolate both in new way
          } else if (avg_lat_velocity >= vel_cp_lat & avg_lon_velocity >= vel_cp_lon){ 
            
            for(z in (current_time-1):startPoint){ #fill in points between current BACKWARDS, according to assumed velocity until you get to the previous location
              lat_back = ifelse(current_lat>previous_lat,round((current_lat-avg_lat_velocity*(current_time-z)), digits = decimal), 
                                round((current_lat+avg_lat_velocity*(current_time-z)), digits = decimal))
              lon_back = ifelse(current_lon>previous_lat,round((current_lon+avg_lon_velocity*(current_time-z)), digits = decimal),
                                round((current_lon-avg_lon_velocity*(current_time-z)), digits = decimal))
              
              if (abs(current_lat-lat_back) < abs(current_lat-previous_lat) & abs(current_lon-lon_back) < abs(current_lon-previous_lon)){
                dayMatrix[z,] <- c(lat_back,lon_back,'bothnew')
              } else if ((abs(current_lat - lat_back) < abs(current_lat-previous_lat) & abs(current_lon-lon_back) >= abs(current_lon-previous_lon))) {
                dayMatrix[z,] = c(lat_back,previous_lon,'bothnew')
              } else if (abs(current_lat - lat_back) >= abs(current_lat-previous_lat) & abs(current_lon-lon_back) < abs(current_lon-previous_lon)) {
                dayMatrix[z,] = c(previous_lat,lon_back,'bothnew')
              } else if (abs(current_lat - lat_back) >= abs(current_lat-previous_lat) & abs(current_lon-lon_back) >= abs(current_lon-previous_lon)){
                dayMatrix[z,] = c(previous_lat,previous_lon,'bothnew')
              }
            }
          } 
          }
        }else if (time_diff > 10 & dist < .05) { # if the distance over the gap is short, then don't interpolate 
          
          for(z in (current_time-1):startPoint){
            dayMatrix[z,] = c(as.numeric(positionMatrix[as.numeric(dataLocation[u])-1,1]),as.numeric(positionMatrix[as.numeric(dataLocation[u])-1,2]),'none-shortdist')
          }
          
        }else{ # with time differences < 10 minutes, or distances > 1 mile, do the old interpolation 
          
          previous_lat <- as.numeric(positionMatrix[as.numeric(dataLocation[u])-1,1])
          current_lat <- as.numeric(positionMatrix[as.numeric(dataLocation[u]),1])
          previous_lon <- as.numeric(positionMatrix[as.numeric(dataLocation[u])-1,2])
          current_lon <- as.numeric(positionMatrix[as.numeric(dataLocation[u]),2])
          previous_time <- startPoint-1
          current_time <- timeInMinutes
          
          avg_lat_velocity <- (current_lat-previous_lat)/(current_time-previous_time)
          avg_lon_velocity <- (current_lon-previous_lon)/(current_time-previous_time)
  
          for(z in startPoint:(timeInMinutes-1)){ #fill in points between previous and current location, according to assumed velocity between locations
            dayMatrix[z,1] <- round((avg_lat_velocity*(z-previous_time)+previous_lat), digits = decimal)
            dayMatrix[z,2] <- round((avg_lon_velocity*(z-previous_time)+previous_lon), digits = decimal)
            dayMatrix[z,3] = 0
          }
        }

      }
      
      # for the current data point, where we have actual data, just fill in with the actual data
      dayMatrix[timeInMinutes,] <- c(positionMatrix[as.numeric(dataLocation[u]),],NA)
      startPoint <- timeInMinutes+1
    }
  }

  # Convert matrix to dataframe.
  # Daylight savings
  if(difftime(as.POSIXct(DateTimeVec[as.numeric(dataLocation[length(dataLocation)])],format = "%Y-%m-%dT%H:%M:%S", tz = "EST"),
              as.POSIXct(as.character(date1), tz = "EST"),units = "mins")>1440){
    time = read.csv('/Users/mpayne/Dropbox/MRC DTP/Bogue fellowship/Data/GPS/Minutes_per_day_ds.csv')
    date = rep(DateVec[1],1500)
  }else{
    time = format( seq.POSIXt(as.POSIXct(Sys.Date(), tz = "EST"), as.POSIXct(Sys.Date()+1, tz = "EST"), by = "1 min"),
          "%H:%M", tz="GMT")
    time = head(time,-1)
    date = rep(DateVec[1],1440)
  }

  # #### Create data frame for all data permin ####
  pt3 <- as.data.frame(c(as.data.frame(date),as.data.frame(time),as.data.frame(dayMatrix)))
  pt3$id = subj
  colnames(pt3) <- c("date","time","lat","lon", "int_type","device_id")

  if (a == 1){
    all_gps_permin <- data.frame(matrix(ncol = 5, nrow = 0))
    colnames(all_gps_permin) <- c("date","time","lat","lon", "device_id")
  }

  all_gps_permin = rbind(all_gps_permin,pt3)

  print(file_list[a])
  print(paste(as.character(a), 'of', as.character(length(file_list)), 'complete'))

}
}


#### Create individual files per participant and save as csv ####

all_gps_filtered = all_gps_filtered[order(all_gps_filtered$device_id),]
all_gps_permin = all_gps_permin[order(all_gps_permin$device_id),]

# remove repeats
# this is possible because we are using weekly data, so there may be some overlap with the daily downloader
# note from Susan: i don't think this is possible with our downloading structure but no harm in keeping it
all_gps_permin <- all_gps_permin[!duplicated(all_gps_permin), ]

devices = unique(all_gps_permin$device_id)

# Initialize a vector to store error messages
error_messages <- c()

for (s in 1:length(devices)) {
  # Filter data for the current participant
  gpsp_by_pp <- all_gps_permin[all_gps_permin$device_id == devices[s], ]
  gpsp_by_pp <- gpsp_by_pp[order(gpsp_by_pp$date), ]
  gpsf_by_pp <- all_gps_filtered[all_gps_filtered$device_id == devices[s], ]
  gpsf_by_pp <- gpsf_by_pp[order(gpsf_by_pp$date), ]
  
  # Ensure the output directories exist
  if (!dir.exists(out_dir_permin)) {
    dir.create(out_dir_permin, recursive = TRUE)
    message(paste("Created missing directory:", out_dir_permin))
  }
  
  if (!dir.exists(out_dir_filtered)) {
    dir.create(out_dir_filtered, recursive = TRUE)
    message(paste("Created missing directory:", out_dir_filtered))
  }
  
  # Generate file paths for the participant
  file_path_permin <- file.path(out_dir_permin, paste0(as.character(devices[s]), "_gps_permin.csv"))
  file_path_filtered <- file.path(out_dir_filtered, paste0(as.character(devices[s]), "_gps_noiseremoved.csv"))
  
  # Try to write the PER MIN file
  tryCatch({
    write.csv(gpsp_by_pp, file = file_path_permin, row.names = FALSE)
  }, error = function(e) {
    error_messages <- c(error_messages, paste("Failed to create PER MIN file for participant:", devices[s], "-", e$message))
  })
  
  # Try to write the FILTERED file
  tryCatch({
    write.csv(gpsf_by_pp, file = file_path_filtered, row.names = FALSE)
  }, error = function(e) {
    error_messages <- c(error_messages, paste("Failed to create FILTERED file for participant:", devices[s], "-", e$message))
  })
}

# Print all error messages at the end
if (length(error_messages) > 0) {
  cat("Summary of Errors:\n")
  cat(paste(error_messages, collapse = "\n"))
  
  # Save errors to a log file
  log_file <- file.path(out_dir_permin, "error_log.txt")
  writeLines(error_messages, log_file)
  message(paste("Error log saved to:", log_file))
} else {
  message("No errors encountered.")
}