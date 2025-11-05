#### Calculate GPS parameters ####

#########################################################################
##### run noiseremoval and get_pp_ids... R files before running this ####
#########################################################################
##### also run check_onlyonedate.py and add any to excluded IDs ########
#### (these usually overlap with the above, but good to check) ########
#########################################################################

# The following R code calculates various GPS parameters - Novelty, RE, Distance, and PE. It does so by reading in two different types of location data, filtered (noise removed) and permin (per minute).
# The filtered location data is used to calculate Novelty, while the permin location data is used to calculate RE and Distance. PE is calculated using RE.
# The results are saved in a csv file in long form.

# Must run file 'noise_removal.R' before this to get filtered GPS data as input 
# This script calculates daily Novelty, RE, Distance and PE
# Reads filtered location data - Calculate Novelty 
# Reads permin location data - Calculate RE and Distance
# Uses RE to calculate PE
# Save all data as csv in longform

library(geosphere)
library(measurements)
library(dplyr)
library(plyr)

#in_dir_filtered = "/Users/mpayne/Library/CloudStorage/Box-Box/HartleyLab_SHARED/1_STUDIES/Transitions_Maddy/data/gps/gps_noiseremoved/"
#in_dir_permin = "/Users/mpayne/Library/CloudStorage/Box-Box/HartleyLab_SHARED/1_STUDIES/Transitions_Maddy/data/gps/gps_permin_new/"
#in_dir_permin_woint = "/Users/mpayne/Library/CloudStorage/Box-Box/HartleyLab_SHARED/1_STUDIES/Transitions_Maddy/data/gps/gps_permin_woint/"

############################################################
######## CHANGE THESE FILE PATHS (TWO MORE BELOW) ######### 
############################################################

in_dir_filtered = "/Users/susanbenear/Library/CloudStorage/GoogleDrive-susanbenear@gmail.com/My Drive/NYU/MemGeoVal Project/MemGeo Study/Data/GPS Data/gps_csv_files_7.16.25/gps_filtered_2025-07-16"
in_dir_permin = "/Users/susanbenear/Library/CloudStorage/GoogleDrive-susanbenear@gmail.com/My Drive/NYU/MemGeoVal Project/MemGeo Study/Data/GPS Data/gps_csv_files_7.16.25/gps_permin_2025-07-16"


#### Novelty ####

filtered_files = list.files(in_dir_filtered, full.names = TRUE)
files_short = list.files(in_dir_filtered, full.names = FALSE)
device_IDs <- substr(files_short, 1, 8)

# Define the IDs to exclude
#excluded_ids <- c("12777488","12777838","12779212","12779441","12779483","12779634","12779694","12796405","12797825","12798715","12798828","12799135","12799649","12800686","12801421","12806695","12825300","12826231","12831086","12832285","12834028","12835304","12835248", "12837861","12837840","12835219")
excluded_ids <- c("12777488","12777838","12779212","12779441","12779483","12779634","12779694","12796405","12797825","12798715","12798828","12799135","12799649","12800686","12801421","12806695","12825300","12826231","12831086","12832285","12832503","12832574","12832758","12834028","12835015","12835219","12835247","12835304","12836413","12836859","12838400","12840340","12844680","12835248")
  
# Define device_IDs, excluding the specified IDs
#device_IDs <- device_IDs[!device_IDs %in% excluded_ids]  # Exclude specific IDs

# Apply exclusion to both device_IDs and filtered_files
keep_index <- !device_IDs %in% excluded_ids
# Filter both vectors accordingly
filtered_files <- filtered_files[keep_index]
files_short <- files_short[keep_index]
device_IDs <- device_IDs[keep_index]

for (p in 1:length(filtered_files)) {
  tryCatch({
    
    # === Start of your existing code ===
    filename <- filtered_files[p]
    p_master_file <- read.csv(filename, stringsAsFactors = FALSE)
    
    p_master_file <- p_master_file %>%
      filter(lat >= -90 & lat <= 90 & lon >= -180 & lon <= 180)
    
    if (nrow(p_master_file) == 0) {
      warning(paste("Skipping file due to invalid coordinates:", filename))
      next
    }
    
    device_id <- p_master_file$device_id[1]
    
    # Convert columns and compute datetime/coord
    p_master_file$datetime <- as.POSIXct(strptime(paste(p_master_file$date, p_master_file$time), "%Y-%m-%d %H:%M:%S"))
    p_master_file$lat <- as.numeric(p_master_file$lat)
    p_master_file$lon <- as.numeric(p_master_file$lon)
    p_master_file$coord <- paste(p_master_file$lat, p_master_file$lon, sep = ",")
    
    # Order and initialize
    p_master_file <- p_master_file[order(p_master_file$datetime), ]
    p_master_file$LastVisited <- character(nrow(p_master_file))
    p_master_file$coordNovelty <- rep(0, nrow(p_master_file))
    
    p_master_file$LastVisited[1] <- as.character(p_master_file$datetime[1])
    
    for (r in 2:nrow(p_master_file)) {
      found <- 0
      for (s in (r - 1):1) {
        if (p_master_file$coord[r] == p_master_file$coord[s]) {
          p_master_file$LastVisited[r] <- as.character(p_master_file$datetime[s])
          found <- 1
          break
        }
      }
      if (found == 0) {
        p_master_file$LastVisited[r] <- as.character(p_master_file$datetime[r])
      }
    }
    
    p_master_file$LastVisited <- as.POSIXct(p_master_file$LastVisited, format = "%Y-%m-%d %H:%M:%S")
    p_master_file$coordNovelty <- difftime(p_master_file$datetime, p_master_file$LastVisited, units = "mins")
    
    # Daily novelty summary
    pp_dates <- unique(p_master_file$date)
    pp_novelty <- data.frame(matrix(ncol = 3, nrow = 0))
    for (d in 1:length(pp_dates)) {
      pp_novelty[d, ] <- c(device_id, pp_dates[d], sum(p_master_file$coordNovelty[p_master_file$date == pp_dates[d]] == 0))
    }
    colnames(pp_novelty) <- c("device_id", "date", "novelty")
    
    if (p == 1) {
      novelty <- data.frame(matrix(ncol = 3, nrow = 0))
      colnames(novelty) <- c("device_id", "date", "novelty")
    }
    
    novelty <- rbind(novelty, pp_novelty)
    
    print(filename)
    print(paste(p, "of", length(filtered_files), "complete"))
    
    # === End of your existing code ===
    
  }, error = function(e) {
    warning(paste("Skipping due to error in file", filtered_files[p], ":", e$message))
  })
}



#### Roaming entropy & Distance ####

permin_files = list.files(in_dir_permin, full.names = TRUE)
pm_files_short = list.files(in_dir_permin, full.names = FALSE)


# Filter both vectors accordingly
permin_files <- permin_files[keep_index]
pm_files_short <- pm_files_short[keep_index]


subj_list <- substr(pm_files_short,1,8)
subj_list <- subj_list[!duplicated(subj_list)]
start_date <- min(novelty$date)
end_date <- max(novelty$date)
DaysVec <- seq(as.Date(start_date), as.Date(end_date), by="days")

for (a in 1:length(permin_files)) {
  subj <- substr(pm_files_short[a], 1, 8)
  permin_dat <- read.csv(permin_files[a]) # Read in file for participant (every day's file)
  
  # Validate and filter invalid coordinates
  permin_dat <- permin_dat %>%
    filter(lat >= -90 & lat <= 90 & lon >= -180 & lon <= 180)
  
  if (nrow(permin_dat) == 0) {
    warning(paste("Skipping file due to invalid coordinates:", permin_files[a]))
    next
  }
  
  # Process each day
  pp_dates <- unique(permin_dat$date)
  pp_dates <- pp_dates[order(pp_dates)]
  
  for (d in 1:length(pp_dates)) {
    singleday_dat <- permin_dat[permin_dat$date == pp_dates[d], ]
    
    if (nrow(singleday_dat) > 1500) { # Check for daylight savings issue
      warning(paste("Skipping date due to excessive rows (>1500):", pp_dates[d]))
      next
    }
    
    singleday_dat <- singleday_dat[!duplicated(singleday_dat), ]
    
    if (d == 1) {
      REdist_pp <- data.frame(matrix(ncol = 4, nrow = 0))
      colnames(REdist_pp) <- c("device_id", "date", "RE", "distance_miles")
    }
    
    if (nrow(singleday_dat) == 0) { # Handle empty day
      REdist_pp[d, ] <- c(subj, pp_dates[d], NA, NA)
      next
    }
    
    singleday_latlon <- singleday_dat[, c("lat", "lon")]
    
    # Filter invalid coordinates again for safety
    singleday_latlon <- singleday_latlon %>%
      filter(lat >= -90 & lat <= 90 & lon >= -180 & lon <= 180)
    
    if (nrow(singleday_latlon) < 2) { # Skip if insufficient data for distance calculation
      REdist_pp[d, ] <- c(subj, pp_dates[d], NA, 0)
      next
    }
    
    # Calculate roaming entropy for that date
    # uniquePositions <- ddply(singleday_latlon, .(lat, lon), nrow)
    # uniquePositions$p <- uniquePositions$V1 / nrow(singleday_latlon)
    # uniquePositions$plogp <- uniquePositions$p * log(uniquePositions$p)
    # sampleRE <- (-sum(uniquePositions$plogp)) / log(648000000)
    # REdist_pp[d, ] <- c(subj, pp_dates[d], sampleRE, NA)
    
    # Calculate roaming entropy for that date
    uniquePositions <- ddply(singleday_latlon, .(lat, lon), nrow)
    
    if (nrow(uniquePositions) == 1) {
      # If all coordinates are the same, set RE to zero
      sampleRE <- 0
    } else {
      # Standard RE calculation
      uniquePositions$p <- uniquePositions$V1 / nrow(singleday_latlon)
      uniquePositions$plogp <- uniquePositions$p * log(uniquePositions$p)
      sampleRE <- (-sum(uniquePositions$plogp)) / log(648000000)
    }
    REdist_pp[d, ] <- c(subj, pp_dates[d], sampleRE, NA)
    
    
    # Calculate distance for that date
    distance <- 0
    for (timepoint in 1:(nrow(singleday_latlon) - 1)) {
      distance <- distance + distHaversine(
        c(singleday_latlon$lon[timepoint], singleday_latlon$lat[timepoint]),
        c(singleday_latlon$lon[timepoint + 1], singleday_latlon$lat[timepoint + 1])
      )
    }
    REdist_pp$distance_miles[d] <- conv_unit(distance, "m", "mi")
  }
  
  # Combine with the main REdist data frame
  if (a == 1) {
    REdist <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(REdist) <- c("device_id", "date", "RE", "distance_miles")
  }
  
  REdist <- rbind(REdist_pp, REdist)
  
  print(permin_files[a])
  print(paste(as.character(a), "of", as.character(length(permin_files)), "complete"))
}


# Convert columns to appropriate types
REdist$device_id = as.numeric(REdist$device_id)
REdist$RE = as.numeric(REdist$RE)
REdist$distance_miles = as.numeric(REdist$distance_miles)

# Remove rows where device_id is NA
REdist <- REdist[!is.na(REdist$device_id), ]

# Calculate z-scores
# REdist$RE_z = ave(REdist$RE, REdist$device_id, FUN = scale, center = TRUE, scale = FALSE)
# REdist$distance_miles_z = ave(REdist$distance_miles, REdist$device_id, FUN = scale, center = TRUE, scale = FALSE)

# Calculate z-scores - updated to handle values of 0 for RE or distance so we don't end up with NaNs in REdist
# Calculate z-scores for RE
REdist$RE_z <- ave(REdist$RE, REdist$device_id, FUN = function(x) {
  if (sd(x, na.rm = TRUE) == 0) {
    # If standard deviation is 0, assign 0 to z-scores
    rep(0, length(x))
  } else {
    scale(x, center = TRUE, scale = FALSE)
  }
})

# Calculate z-scores for distance_miles
REdist$distance_miles_z <- ave(REdist$distance_miles, REdist$device_id, FUN = function(x) {
  if (sd(x, na.rm = TRUE) == 0) {
    # If standard deviation is 0, assign 0 to z-scores
    rep(0, length(x))
  } else {
    scale(x, center = TRUE, scale = FALSE)
  }
})


# Convert date column to Date type
REdist$date = as.Date(REdist$date)


# #### Roaming entropy & Distance (without interpolation) ####
# 
# permin_files_woint = list.files(in_dir_permin_woint, full.names = TRUE)
# pm_files_short_woint = list.files(in_dir_permin_woint, full.names = FALSE)
# 
# subj_list <- substr(pm_files_short_woint,1,8)
# subj_list <- subj_list[!duplicated(subj_list)]
# start_date <- min(novelty$date)
# end_date <- max(novelty$date)
# DaysVec <- seq(as.Date(start_date), as.Date(end_date), by="days")
# 
# for(a in 1:length(permin_files)){
#   subj <- substr(pm_files_short_woint[a],1,8)
#   permin_dat <- read.csv(permin_files_woint[a]) # Read in file for participant (every days file).
#   
#   #for each day, take part of file needed, calculate roaming entropy, enter into longform matrix
#   pp_dates = unique(permin_dat$date)
#   pp_dates = pp_dates[order(pp_dates)]
#   
#   for (d in 1:length(pp_dates)){
#     singleday_dat = permin_dat[permin_dat$date == pp_dates[d],]
#     if (nrow(singleday_dat)>1500){ # if there are more than 25 (because daylight savings) hours in a day then throw an error
#       stop
#     }
#     singleday_dat = singleday_dat[!duplicated(singleday_dat),]
#     
#     if (d==1){
#       REdist_pp_woint = data.frame(matrix(ncol = 4, nrow = 0))
#       colnames(REdist_pp_woint) <- c("device_id","date","RE_woint","distance_miles_woint")
#     }
#     
#     if(nrow(singleday_dat)==0){ #if there's no data for this date, put NA for RE and dist
#       REdist_pp_woint[d,] <- c(subj,pp_dates[d],NA,NA)
#       next
#     }
#     
#     singleday_latlon = singleday_dat[,3:4]
#     
#     # Calculate roaming entropy for that date
#     uniquePositions <- ddply(singleday_latlon,.(lat,lon),nrow) #  number of time points (i.e. no. mins) recorded for a given position.
#     uniquePositions$p <- uniquePositions$V1/nrow(singleday_latlon) # P is the  number of time points recorded for a given position divided by the total number of minutes in the day. 
#     uniquePositions$plogp <- uniquePositions$p*log(uniquePositions$p)
#     sampleRE_woint = (-sum(uniquePositions$plogp))/log(648000000)
#     REdist_pp_woint[d,] = c(subj,pp_dates[d],sampleRE_woint,NA)
#     
#     # Calculate distance for that date (used to use uniquePositions MovesDev but I think singleday_latlon makes more sense to account going to a location and back again)
#     distance = 0;
#     if (nrow(singleday_latlon)>1){
#       for(timepoint in 1:(length(singleday_latlon$lat)-1)){
#         # This calculates distance in meters.
#         distance = distance + distHaversine(c(singleday_latlon$lon[timepoint], singleday_latlon$lat[timepoint]), c(singleday_latlon$lon[timepoint+1], singleday_latlon$lat[timepoint+1]))
#       }
#       REdist_pp_woint$distance_miles_woint[d] = conv_unit(distance, "m", "mi")
#     } else{
#       REdist_pp_woint$distance_miles_woint[d] = 0;
#     }
#     
#     
#   }
#   
#   #### Bind together different participants ####
#   
#   if (a == 1){
#     REdist_woint = data.frame(matrix(ncol = 4, nrow = 0))
#     colnames(REdist_woint) <- c("device_id","date","RE_woint", "distance_miles_woint")
#   }
#   
#   REdist_woint = rbind(REdist_pp_woint,REdist_woint)
#   
#   print(permin_files[a])
#   print(paste(as.character(a), 'of', as.character(length(permin_files)), 'complete'))
# }

# REdist_woint$device_id = as.numeric(REdist_woint$device_id)
# REdist_woint$RE_woint = as.numeric(REdist_woint$RE_woint)
# REdist_woint$distance_miles_woint = as.numeric(REdist_woint$distance_miles_woint)
# REdist_woint$RE_woint_z = ave(REdist_woint$RE_woint, REdist_woint$device_id, FUN=scale, center = TRUE, scale = FALSE)
# REdist_woint$distance_miles_woint_z = ave(REdist_woint$distance_miles_woint, REdist_woint$device_id, FUN=scale, center = TRUE, scale = FALSE)
# 
# novelty$date = as.Date(novelty$date)
# REdist = merge(REdist,novelty)
# #REdist = merge(REdist, REdist_woint)

# # Remove days that occur before the initial questionnaire has been completed ----
# q_dir = "/Users/mpayne/Library/CloudStorage/Box-Box/HartleyLab_SHARED/1_STUDIES/Transitions_Maddy/data/questionnaires/"
# init_q = read.csv(paste0(q_dir,"Transitions - Questionnaire battery_September 29, 2022_14.05.csv"))
# init_q = subset(init_q, !is.na(spark_id) & spark_id>2000 & spark_id<3500)
# init_q$StartDate = strptime(init_q$StartDate, "%m/%d/%y %H:%M", tz = "America/Denver") #for some reason qualtrics outputted Denver timezone
# init_q$StartDate <- lubridate::with_tz(init_q$StartDate, "America/New_York") #convert to NY
# init_q$date <- as.Date(init_q$StartDate)

#merge with IDs to get device id 
# id_dir = "/Users/susanbenear/Library/CloudStorage/GoogleDrive-susanbenear@gmail.com/My Drive/NYU/MemGeoVal Project/MemGeo Study/Data/GPS Data/"
# ids = read.csv(paste0(id_dir,"memgeo_IDs_1.22.25.csv"), header = TRUE)
# init_q = merge(init_q,ids, by = "part_id")
# pp_startdates = init_q[,c("device_id","date")]
# pp_startdates$device_id = as.character(pp_startdates$device_id)
# 
# REdist1 = REdist
# REdist$device_id = as.character(REdist$device_id)
# REdist <- REdist %>%
#   inner_join(pp_startdates, by = "device_id") %>% # join df1 and df2 on device_id
#   filter(date.x >= date.y) %>% # filter out dates earlier than the one in df2
#   dplyr::select(-date.y) # remove the date column from df2
#   
# colnames(REdist)[2] = 'date' # rename date.x as date



#### Prediction Error ####

#Get participant IDs#
device_IDs <- unique(REdist$device_id)

PE_dataframe <- data.frame(matrix(ncol = 5, nrow = nrow(REdist)))
colNames <- c("Participant_ID", "RE_Filtered", "Recorded_Date","Prediction", "PE") #Column names to save.
colnames(PE_dataframe) <- colNames

PE_dataframe$Participant_ID = REdist$device_id;
PE_dataframe$Recorded_Date = REdist$date;
PE_dataframe$RE_Filtered = as.numeric(REdist$RE);

alpha = 0.1;
GroupAverage = mean(as.numeric(REdist$RE), na.rm = TRUE); #Calculate group mean igoring NA values.


# Initialize row_counter
row_counter <- 1

# Loop through each participant
for (i in 1:length(device_IDs)) {
  inside_counter <- 1
  
  # Check row_counter bounds and ensure valid comparison
  while (row_counter <= nrow(PE_dataframe) && 
         device_IDs[i] == PE_dataframe$Participant_ID[row_counter]) {
    
    print(paste("Processing i =", i, "row_counter =", row_counter))
    
    if (inside_counter == 1) { 
      PE_dataframe$Prediction[row_counter] <- GroupAverage
    } else if (inside_counter == 2) {
      if (!is.na(PE_dataframe$RE_Filtered[row_counter - 1])) {
        PE_dataframe$Prediction[row_counter] <- PE_dataframe$RE_Filtered[row_counter - 1]
        PE_dataframe$PE[row_counter] <- PE_dataframe$RE_Filtered[row_counter] - 
          PE_dataframe$Prediction[row_counter]
      } else {
        PE_dataframe$Prediction[row_counter] <- PE_dataframe$Prediction[row_counter - 1]
      }
    } else {
      yesterday_RE <- PE_dataframe$RE_Filtered[row_counter - 1]
      yesterday_prediction <- PE_dataframe$Prediction[row_counter - 1]
      yesterday_error <- PE_dataframe$PE[row_counter - 1]
      today_RE <- PE_dataframe$RE_Filtered[row_counter]
      
      if (!is.na(PE_dataframe$RE_Filtered[row_counter])) {
        prediction <- sum(alpha * yesterday_error, yesterday_prediction, na.rm = TRUE)
        PE_dataframe$Prediction[row_counter] <- prediction
        PE_dataframe$PE[row_counter] <- today_RE - prediction
      } else {
        prediction <- sum(alpha * yesterday_error, yesterday_prediction, na.rm = TRUE)
        PE_dataframe$Prediction[row_counter] <- prediction
      }
    }
    
    inside_counter <- inside_counter + 1
    row_counter <- row_counter + 1
  }
}


PE = PE_dataframe[,c(1,3:5)]
colnames(PE) = c("device_id","date","prediction","PE")

PE$PE_z = ave(PE$PE, PE$device_id, FUN=scale, center = TRUE, scale = FALSE)

#### Modal location (for weather data preprocessing) ####

for(a in 1:length(permin_files)){
  subj <- substr(pm_files_short[a],1,8)
  permin_dat <- read.csv(permin_files[a]) # Read in file for participant (every days file).
  
  #for each day, take part of file needed, calculate roaming entropy, enter into longform matrix
  pp_dates = unique(permin_dat$date)
  pp_dates = pp_dates[order(pp_dates)]

  for (d in 1:length(pp_dates)){
    singleday_dat = permin_dat[permin_dat$date == pp_dates[d],]
    if (nrow(singleday_dat)>1500){ # if there are more than 25 (because daylight savings) hours in a day then throw an error
      stop
    }
    singleday_dat = singleday_dat[!duplicated(singleday_dat),]
    
    if (d==1){
      modal_loc = data.frame(matrix(ncol = 3, nrow = 0))
      colnames(modal_loc) <- c("device_id","date","mod_location_coord")
    }
    
    if(nrow(singleday_dat)==0){ #if there's no data for this date, put NA for RE and dist
      modal_loc[d,] <- c(subj,pp_dates[d],NA)
      next
    }
    
    singleday_latlon = singleday_dat[,3:4]
    
    # Calculate modal location for that day
    uniquePositions <- ddply(singleday_latlon,.(lat,lon),nrow) #  number of time points (i.e. no. mins) recorded for a given position.
    modal_location_d = uniquePositions[uniquePositions$V1 == max(uniquePositions$V1),1:2]
    if (nrow(modal_location_d) > 1){
      modal_location_d <- modal_location_d[sample(nrow(modal_location_d), size = 1), ]
    }
    modal_loc[d,] = c(subj,pp_dates[d],paste(modal_location_d$lat, modal_location_d$lon,sep=','))
  }
  
  #### Bind together different participants ####
  
  if (a == 1){
    modal_location = data.frame(matrix(ncol = 3, nrow = 0))
    colnames(modal_location) <- c("device_id","date","mod_location_coord")
  }
  
  modal_location = rbind(modal_loc,modal_location)
  
  print(permin_files[a])
  print(paste(as.character(a), 'of', as.character(length(permin_files)), 'complete'))
}

#### Count total number of unique locations ####
tot_loc_4dig = modal_location %>%
  filter(!duplicated(mod_location_coord)) 
nrow(tot_loc_4dig)

#### Merge together, then with spark IDs and export as CSV ####

novelty$date = as.Date(novelty$date)
gps_parameters = merge(REdist,PE)
# Merge novelty into gps_parameters
gps_parameters <- merge(gps_parameters, novelty, by = c("device_id", "date"), all.x = TRUE)


############################################################
########### CHANGE THESE FILE PATHS & FILENAMES ############
############################################################

#merge with IDs 
id_dir = "/Users/susanbenear/Library/CloudStorage/GoogleDrive-susanbenear@gmail.com/My Drive/NYU/MemGeoVal Project/MemGeo Study/Data/GPS Data/"
########## CHANGE FILE NAME BELOW ########
ids = read.csv(paste0(id_dir,"memgeo_IDs_7.16.25.csv"), header = TRUE)
gps_parameters = merge(gps_parameters,ids, by = "device_id")
gps_parameters$date = as.Date(gps_parameters$date)

out_dir = "/Users/susanbenear/Library/CloudStorage/GoogleDrive-susanbenear@gmail.com/My Drive/NYU/MemGeoVal Project/MemGeo Study/Data/GPS Data/gpgps_csv_files_7.16.25/gps_entropy/"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}
write.table(gps_parameters,file = paste0(out_dir,"GPS_parameters.csv"), sep = ',', row.names = FALSE)
write.table(REdist, file = paste0(out_dir,'new_RE_calc.csv'), sep = ',',row.names = F)

#Split latlon and save as a csv
latlon = data.frame(do.call('rbind', strsplit(as.character(modal_location$mod_location_coord),',',fixed=TRUE)))
modal_location = cbind(modal_location,latlon)
names(modal_location)[names(modal_location) == "X1"] <- "lat"
names(modal_location)[names(modal_location) == "X2"] <- "lon"
write.table(modal_location, file = paste0(out_dir, 'daily_modal_location.csv'), sep = ',', row.names = FALSE)
write.table(tot_loc_4dig, file = paste0(out_dir,'unique_modal_locations.csv'), sep = ',', row.names = FALSE)
