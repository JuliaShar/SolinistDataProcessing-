library(OGFLtools)
library(tidyverse)
library(hms)
library(lubridate)   
library(stringr)     
library(ggplot2)     
library(zoo)        
library(ggside)
library(dplyr)
library(googledrive)
library(readr)

#SETUP

# Function to detect flatlines (constant values over extended periods)
# x: vector of values to check
# interval_mins: time interval between readings in minutes
# flat_hours: minimum hours of constant value to flag as flatline
run_length_flag <- function(x, interval_mins, flat_hours){
  r <- rle(x)                                          # Run-length encoding: identifies consecutive identical values
  lengths_in_hours <- r$lengths * interval_mins / 60   # Convert number of readings to hours
  rep(lengths_in_hours >= flat_hours, r$lengths)       # Return TRUE for each value in runs >= threshold
}

well_corrections <- tibble(
  serial_number = c("1094201"),      # Serial number of the sensor
  cable_correction = c(1.69)         # Correction value in meters
)

# Main QA/QC function that applies all quality checks to the dataset
apply_qaqc <- function(df, expected_interval_mins, temp_min, temp_max, 
                       level_min, level_max,
                       temp_spike_thresh, level_spike_thresh,
                       z_thresh, flatline_hours) {
  
  df <- df %>%
    arrange(serial_number, date_time) %>%    # Sort by sensor and time to ensure chronological order
    group_by(serial_number) %>%                  # Group by sensor so calculations are done per sensor
    mutate(
      time_diff_mins = as.numeric(difftime(date_time, lag(date_time), units = "mins")),
      # Check if current timestamp is a duplicate of any previous timestamp
      duplicate_time = duplicated(date_time),
      # Flag if time gap exceeds expected interval by 50% (e.g., > 90 mins for 60-min interval)
      gap_flag = time_diff_mins > expected_interval_mins * 1.5,
      temp_range_flag  = temperature_c < temp_min | temperature_c > temp_max,
     # cond_range_flag  = conductivity_uscm < cond_min | conductivity_uscm > cond_max,
      level_range_flag = level_m < level_min | level_m > level_max,
      d_temp  = temperature_c - lag(temperature_c),
      #   d_cond  = conductivity_uscm - lag(conductivity_uscm),
      d_level = level_m - lag(level_m),
      temp_spike_flag  = abs(d_temp)  > temp_spike_thresh,
     # cond_spike_flag  = abs(d_cond)  > cond_spike_thresh,
      level_spike_flag = abs(d_level) > level_spike_thresh,
      temp_z  = as.numeric(scale(temperature_c)),
      #    cond_z  = as.numeric(scale(conductivity_uscm)),
      level_z = as.numeric(scale(level_m)),
      temp_z_flag  = abs(temp_z)  > z_thresh,
      level_z_flag = abs(level_z) > z_thresh,
      temp_flat_flag  = run_length_flag(temperature_c, expected_interval_mins, flatline_hours),
      #cond_flat_flag  = run_length_flag(conductivity_uscm, expected_interval_mins, flatline_hours),
      level_flat_flag = run_length_flag(level_m, expected_interval_mins, flatline_hours)
    ) %>%
    ungroup() %>%    # Remove grouping to apply overall flags
    mutate(
      qa_flag = case_when(
        duplicate_time ~ "Duplicate Timestamp",                                    
        gap_flag ~ "Time Gap",
        temp_range_flag |  level_range_flag ~ "Range Exceedance",
        temp_spike_flag | level_spike_flag ~ "Spike",
        temp_z_flag | level_z_flag ~ "Statistical Outlier",
        temp_flat_flag | level_flat_flag ~ "Flatline",
        TRUE ~ "OK"                                                                
      ),
      # Assign numeric code for severity: 0=OK, 1=minor issue, 2=major issue
      qa_code = case_when(
        qa_flag == "OK" ~ 0,
        qa_flag %in% c("Statistical Outlier","Time Gap") ~ 1,    # Minor flags
        TRUE ~ 2                                                  # Major flags (duplicates, spikes, range, flatline)
      )
    )
  
  return(df)  # Return the data frame with all QA columns added
}

expected_interval_mins <- 60   

# Physical bounds - minimum and maximum acceptable values for each parameter
temp_min <- -5         # Minimum acceptable temperature in degrees Celsius
temp_max <- 50         # Maximum acceptable temperature in degrees Celsius
cond_min <- 0          # Minimum acceptable conductivity in microsiemens per centimeter (µS/cm)
cond_max <- 100000     # Maximum acceptable conductivity in µS/cm
level_min <- -50       # Minimum acceptable water level in meters
level_max <- 500       # Maximum acceptable water level in meters

# Rate-of-change thresholds - maximum acceptable change between consecutive readings
temp_spike_thresh <- 3       # Maximum temperature change (°C) between readings
cond_spike_thresh <- 500     # Maximum conductivity change (µS/cm) between readings
level_spike_thresh <- 0.5    # Maximum level change (m) between readings

# Z-score threshold for statistical outlier detection
# Values with z-scores exceeding this threshold are flagged as outliers
z_thresh <- 3

# Flatline detection threshold
# Number of hours a value must remain constant to be flagged as flatlined
flatline_hours <- 24


#intake of data from google drive
drive_auth()

folder <- drive_get("Solinist")

all_csv <- drive_ls(folder, type = "csv")

all_data <- map_df(all_csv$id, function(file_id) {
  temp_loc <- tempfile(fileext = ".csv")
  drive_download(as_id(file_id), path = temp_loc, overwrite = TRUE)
  read_csv(temp_loc, show_col_types = FALSE)
})

#quick check
head(all_data)

#rename
df <- all_data %>%
  mutate(
    # Ensure SerialNumber is character
    serial_number = as.character('SerialNumber')) %>% 
  # Rename columns as desired
  rename(
  #  conductivity_uscm = 'Level',                       !!!! there is no conductivty in the file?
    temperature_c = 'Temperature',
    level_m = 'Level'
  )


#add time and date columns
df <- df %>%
  mutate(
    date_time = mdy_hms(Time),
    date      = as_date(date_time),
  )


processed_df <- apply_qaqc(
  df,                # Input dataframe
  expected_interval_mins,     # All the threshold parameters defined earlier
  temp_min, temp_max,
  level_min, level_max,
  temp_spike_thresh, level_spike_thresh,
  z_thresh, flatline_hours
)


# CORRECT GW LEVEL BY WELL 
processed_df <- processed_df %>%
  left_join(well_corrections, by = "serial_number") %>%
  mutate(
    corrected_level = if_else(
      !is.na(cable_correction),           # If correction value exists
      level_m - cable_correction,         # Apply correction
      level_m                             # Otherwise use original value
    )
  )


#SPLIT BY LOCATION?
blackwater <- processed_df %>% filter(Location == "Blackwater")
other <- processed_df %>% filter(is.na((Location)))



#PLOTS
ggplot(processed_df, aes(x = date_time, y = temperature_c, color = factor(`Serial Number`))) +
  geom_line() +
  labs(x = "Date", y = "Temperature (°C)", title = "Temperature") +
  theme_bw()



ggplot(blackwater, aes(x = date_time, y = temperature_c, color = factor(`Serial Number`))) +
  geom_line() +
  labs(x = "Date", y = "Temperature (°C)", color = "Logger", title = "Temperature in Blackwater") +
  theme_bw()


ggplot(other, aes(x = date_time, y = temperature_c)) +
  geom_line() +
  labs(x = "Date", y = "Temperature (°C)", title = "Temperature for Sensor") +
  theme_bw()

# installed in december?? nov 20, 2025 (label is WF)
#1094201 is actual blackwater
#1094251 is AF (abbends)

#water level 
ggplot(processed_df, aes(x = date_time, y = level_m, color = factor(`Serial Number`))) +
  geom_line() +
  labs(x = "Date", y = "Level (m)", title = "Level")

ggplot(blackwater, aes(x = date_time, y = level_m, color = factor(`Serial Number`))) +
  geom_line() +
  labs(x = "Date", y = "Level (m)", color = "Logger", title = "Level in Blackwater")


ggplot(other, aes(x = date_time, y = level_m)) +
  geom_line() +
  labs(x = "Date", y = "Level (m)", title = "Level by Sensor")