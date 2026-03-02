# Ingest Raw Solinst Data.R
# Part of Louisiana Wells Project
# Created on 20250115 by Opal Otenburg

# ---- Load Libraries ----
library(OGFLtools)
library(tidyverse)
library(hms)


# ---- Ingest Raw CSV Files ----
# For each file
# Save the filename to a new column 
# Save the metadata_SN to a new column
# Remove row 1-13
# Rename columns
compensated_files <- list.files(path = "Louisiana_Wells_Sensor_Da ta/Louisiana_Wells_Sensor_Data_Compensated", full.names = TRUE) #pulling in csv files from my local that were donwloaded from solinst cloud and read into the levelogger software to convert to csv

for(file in compensated_files) { #will complete following steps for each file within compensated_files
  
  # Import the csv file
  df <- read_csv(file, skip = 13) #don't need to designate file path, already done. removing first 13 rows which just explain units etc
  
  # Clean up the dataframe
  df <- df %>% 
    mutate(file_name = basename(file)) %>% #creating new column with the file name data
    mutate(serial_number = as.character(read_csv(file, col_names = FALSE)[2,1])) %>% #add a column with the serial number extracted above
    #rename column names to how I want them
    rename(conductivity_uscm = CONDUCTIVITY,
           temperature_c = TEMPERATURE,
           date = Date,
           time_est = Time,
           level_m = LEVEL)
  
  assign(substr(basename(file), 1, nchar(basename(file)) - 4), df) #naming the df with the initial file name, taking out last 4 char (.csv), by using substr starting at first character 1 and subtracting 4
}


# ---- Merge Dataframes ----
# Merge individual dataframes into one new dataframe
# Remove everything from environment except final file

#master_compensated_file <- bind_rows(compiled_dataframes)

#rm(df, file, compiled_dataframes, compensated_files) #remove transient df, file after renaming with initial file names  

lw_dfs <- ls(pattern = "^LW_") #making a list of the names of the dfs that start with LW
master_compensated_file <- do.call(bind_rows, mget(lw_dfs)) #making df with all sensors and date ranges. do.call completes bind_rows multiple times for each of the listed dataframes in lw_dfs, mget gets the dfs from the list of df names


# ---- Extract metadata from filename ----
# Extract series_start and export_date and other values from filename
master_compensated_file_new <- master_compensated_file %>% 
  separate(file_name, into = c("project","start_date", "SN", "end_date", "data_type", "file_type"), sep = "[_.]")

# need to change date formatting

formatted_master_compensated <- master_compensated_file_new %>%
  #update date & time
  mutate(
    date = mdy(date), # Convert date column as character to as date
    date_time_est = as.POSIXct(date) + as.numeric(time_est) # Combine date and time
  ) %>% 
  #start_date
  mutate(
    start_date = ymd(start_date),
    start_date = as.POSIXct(start_date)
  ) %>% 
  #end_date
  mutate(
    end_date = ymd(end_date),
    end_date = as.POSIXct(end_date)
  )

## playing around to check start date (in progress)
#test <- formatted_master_compensated %>% 
# filter(start_date == ymd(20241206)) 



# ---- Remove Duplicate Timeseries ----
# Flag any values where the same sensor:datetime has different values
# if the data is the same, archive the dataframe from the "old" data processing period, then continue only with "new" period, not sure how to do this yet!!!!!!

cleaned_data <- formatted_master_compensated %>%
  group_by(across(-end_date)) %>% # Group by the columns you want to check for duplicates
  summarise(end_date = max(end_date), .groups = 'drop') # Use max to keep the latest end_date

# ---- Data cleaning ----
# need to remove data from before sensors were actually in wells but had already began recording
# should I remove data while the sensors are still equilibrating?
# need to change date formatting


# ---- QA/QC Flagging ----

# flag if file name SN is different than SN in file
# flag for 9999s or NaN
# flag for above or below thresholds (need to determine these values)


# ---- GW conversion ----
# converting to actual water depth
# need to be separate by different sensors 
data <- data %>% 
  mutate(`levels<-`() = Compensated_Level - 1.8) # 1.8 in meters 

# ---- sp cond to psu ----
# need to convert conductivity (us/cm) to practical salinity units using one of the R packages
df$salinity = ec_to_sal(df$temperature_c, df$condictivity_uscm)  


# ---- create relevant plots ----
# regional comparison plot of salinity (box or violin) 
# time series of each sensor 
# regional time series (each sensor is a different color) 

# ---- upload cleaned & processed data ----
# need to decide where to store these files... Gdrive? GitHub?

