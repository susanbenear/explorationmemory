
# Identify pp for whom there is FM data but not memgeo data
# before running calculate_gps_parameters

####### CHANGE FILE PATHS (INCL FOR OUTPUT TXT FILES) AND UPDATE NEW MEM ID CSV FILE WITH NEW IDs ADDED ######
#### mem id's file is just copied from the participant log with only included participants included #####

in_dir_filtered = "/Users/susanbenear/Library/CloudStorage/GoogleDrive-susanbenear@gmail.com/My Drive/NYU/MemGeoVal Project/MemGeo Study/Data/GPS Data/gps_csv_files_7.16.25/gps_filtered_2025-07-16"
id_dir = "/Users/susanbenear/Library/CloudStorage/GoogleDrive-susanbenear@gmail.com/My Drive/NYU/MemGeoVal Project/MemGeo Study/Data/GPS Data/"

#get device IDs
files_short = list.files(in_dir_filtered, full.names = FALSE)
device_IDs <- substr(files_short,1,8)

#get memgeo IDs
#mem_data = read.csv(paste0(id_dir,"memgeo_IDs_1.22.25.csv"), header = TRUE)
mem_data = read.csv(paste0(id_dir,"memgeo_IDs_7.16.25.csv"), header = TRUE)

# IDs in each dataset
gps_ids <- unique(device_IDs)
memgeo_ids <- unique(mem_data$device_id)

# IDs in gps_parameters but not in ids
ids_only_in_gps <- setdiff(gps_ids, memgeo_ids)

# IDs in ids but not in gps_parameters
ids_only_in_memgeo <- setdiff(memgeo_ids, gps_ids)

if (length(ids_only_in_gps) > 0) {
  write.table(ids_only_in_gps, file = "/Users/susanbenear/Library/CloudStorage/GoogleDrive-susanbenear@gmail.com/My Drive/NYU/MemGeoVal Project/MemGeo Study/Data/GPS Data/gps_csv_files_7.16.25/ids_only_in_gps.txt", row.names = FALSE, col.names = FALSE)
  print("Logged IDs only in gps_parameters to ids_only_in_gps.txt")
}

if (length(ids_only_in_memgeo) > 0) {
  write.table(ids_only_in_memgeo, file = "/Users/susanbenear/Library/CloudStorage/GoogleDrive-susanbenear@gmail.com/My Drive/NYU/MemGeoVal Project/MemGeo Study/Data/GPS Data/gps_csv_files_7.16.25/ids_only_in_memgeo.txt", row.names = FALSE, col.names = FALSE)
  print("Logged IDs only in memgeo_IDs.csv to ids_only_in_memgeo.txt")
}

################################################################################
######Get IDs from txt output and paste into calculate_gps_parameters.R#########
################################################################################

