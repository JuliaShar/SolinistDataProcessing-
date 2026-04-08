#load pacakges 
library(tidyverse)   
library(lubridate)   
library(stringr)     
library(ggplot2)     
library(hms)        
library(zoo)        
library(OGFLtools)   

# QA/QC DEFINED SETTINGS 

# Expected time interval between consecutive sensor readings in minutes
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

# Well-specific corrections table
# Add rows for each sensor that needs cable/height correction
# cable_correction is the length of cable below ground or sensor offset from reference point
well_corrections <- tibble(
  serial_number = c("1094201"),      # Serial number of the sensor
  cable_correction = c(1.69)         # Correction value in meters
)

# ^ so far above is just for Abends well but can input the others 

# QA/QC FUNCTIONS 

# Function to detect flatlines (constant values over extended periods)
# x: vector of values to check
# interval_mins: time interval between readings in minutes
# flat_hours: minimum hours of constant value to flag as flatline
run_length_flag <- function(x, interval_mins, flat_hours){
  r <- rle(x)                                          # Run-length encoding: identifies consecutive identical values
  lengths_in_hours <- r$lengths * interval_mins / 60   # Convert number of readings to hours
  rep(lengths_in_hours >= flat_hours, r$lengths)       # Return TRUE for each value in runs >= threshold
}

# Main QA/QC function that applies all quality checks to the dataset
apply_qaqc <- function(df, expected_interval_mins, temp_min, temp_max, 
                       cond_min, cond_max, level_min, level_max,
                       temp_spike_thresh, cond_spike_thresh, level_spike_thresh,
                       z_thresh, flatline_hours) {
  
  df <- df %>%
    arrange(serial_number, date_time_est) %>%    # Sort by sensor and time to ensure chronological order
    group_by(serial_number) %>%                  # Group by sensor so calculations are done per sensor
    mutate(
      time_diff_mins = as.numeric(difftime(date_time_est, lag(date_time_est), units = "mins")),
      # Check if current timestamp is a duplicate of any previous timestamp
      duplicate_time = duplicated(date_time_est)
      # Flag if time gap exceeds expected interval by 50% (e.g., > 90 mins for 60-min interval)
      gap_flag = time_diff_mins > expected_interval_mins * 1.5,
      temp_range_flag  = temperature_c < temp_min | temperature_c > temp_max,
      cond_range_flag  = conductivity_uscm < cond_min | conductivity_uscm > cond_max,
      level_range_flag = level_m < level_min | level_m > level_max,
      d_temp  = temperature_c - lag(temperature_c),
      d_cond  = conductivity_uscm - lag(conductivity_uscm),
      d_level = level_m - lag(level_m),
      temp_spike_flag  = abs(d_temp)  > temp_spike_thresh,
      cond_spike_flag  = abs(d_cond)  > cond_spike_thresh,
      level_spike_flag = abs(d_level) > level_spike_thresh,
      temp_z  = as.numeric(scale(temperature_c)),
      cond_z  = as.numeric(scale(conductivity_uscm)),
      level_z = as.numeric(scale(level_m)),
      cond_z_flag  = abs(cond_z)  > z_thresh,
      level_z_flag = abs(level_z) > z_thresh,
      temp_flat_flag  = run_length_flag(temperature_c, expected_interval_mins, flatline_hours),
      cond_flat_flag  = run_length_flag(conductivity_uscm, expected_interval_mins, flatline_hours),
      level_flat_flag = run_length_flag(level_m, expected_interval_mins, flatline_hours)
    ) %>%
    ungroup() %>%    # Remove grouping to apply overall flags
    mutate(
      qa_flag = case_when(
        duplicate_time ~ "Duplicate Timestamp",                                    
        gap_flag ~ "Time Gap",
        temp_range_flag | cond_range_flag | level_range_flag ~ "Range Exceedance",
        temp_spike_flag | cond_spike_flag | level_spike_flag ~ "Spike",
        temp_z_flag | cond_z_flag | level_z_flag ~ "Statistical Outlier",
        temp_flat_flag | cond_flat_flag | level_flat_flag ~ "Flatline",
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

# LOAD CSV FILES 

compensated_files <- list.files(
  path = "Sensor_Data/Sensor_Data_Compensated",   # Path to folder containing CSV files
  full.names = TRUE,                               # Return full path (not just filename)
  pattern = "\\.csv$"                              # Only select files ending in .csv
)

# Initialize empty list to store individual data frames
all_data_list <- list()

# Loop through each file and import it
for(file in compensated_files) {
  
  # Import the csv file
  # skip = 13: Skip first 13 rows (Solinst metadata)
  # show_col_types = FALSE: Suppress column type messages
  df <- read_csv(file, skip = 13, show_col_types = FALSE)
  
  # Clean up and standardize the dataframe
  df <- df %>% 
    mutate(
      # Extract just the filename (remove directory path)
      file_name = basename(file),
      serial_number = as.character(read_csv(file, col_names = FALSE, show_col_types = FALSE)[2,1])
    ) %>%
    rename(
      conductivity_uscm = CONDUCTIVITY,    # Conductivity in microsiemens/cm
      temperature_c = TEMPERATURE,          # Temperature in Celsius
      date = Date,                          # Date column
      time_est = Time,                      # Time column
      level_m = LEVEL                       # Water level in meters
    )
  all_data_list[[basename(file)]] <- df
}

# MAKE ONE DATAFRAME 

master_data <- bind_rows(all_data_list)

master_data <- master_data %>% 
  separate(file_name, 
           into = c("project", "start_date", "SN", "end_date", "data_type", "file_type"), 
           sep = "[_.]",           # Split on underscores OR periods
           remove = FALSE) %>%     # Keep original file_name column
  mutate(
    date = mdy(date),
    date_time_est = as.POSIXct(date, tz = "America/Chicago") + as.numeric(time_est), # may have to change this because LA in different timezone?
    start_date = ymd(start_date),
    end_date = ymd(end_date)
  )

# REMOVE DUPLICATES 

master_data <- master_data %>%
  group_by(serial_number, date_time_est, temperature_c, conductivity_uscm, level_m) %>%
  summarise(
    end_date = max(end_date)
    file_name = first(file_name),
    project = first(project),
    start_date = first(start_date),
    SN = first(SN),
    data_type = first(data_type),
    .groups = 'drop'    
  )

# QA/QC 

master_data_qc <- apply_qaqc(
  master_data,                # Input dataframe
  expected_interval_mins,     # All the threshold parameters defined earlier
  temp_min, temp_max,
  cond_min, cond_max,
  level_min, level_max,
  temp_spike_thresh, cond_spike_thresh, level_spike_thresh,
  z_thresh, flatline_hours
)

# QA/QC SUMAMRY 

# Overall summary across all sensors
overall_qa_summary <- master_data_qc %>%
  count(qa_flag) %>%                                    # Count occurrences of each flag type
  mutate(percent = round(n / sum(n) * 100, 2)) %>%     # Calculate percentage
  arrange(desc(n))                                      # Sort by most common first

# Print overall summary to console
print("=== OVERALL QA SUMMARY ===")
print(overall_qa_summary)
cat("\nTotal records:", nrow(master_data_qc), "\n")
cat("Percent flagged (not OK):",
    round(sum(master_data_qc$qa_flag != "OK") / nrow(master_data_qc) * 100, 2), "%\n\n")

# Summary broken down by individual sensor
sensor_qa_summary <- master_data_qc %>%
  group_by(serial_number, qa_flag) %>%                  # Group by sensor and flag type
  summarise(n = n(), .groups = "drop_last") %>%        # Count within each group
  mutate(percent = round(n / sum(n) * 100, 2)) %>%     # Calculate percentage within each sensor
  arrange(serial_number, desc(n))                       # Sort by sensor, then by count

# Print sensor-specific summary to console
print("=== QA SUMMARY BY SENSOR ===")
print(sensor_qa_summary)

# Create output directory for saving plots
# showWarnings = FALSE prevents error if directory already exists
dir.create("qc_plots", showWarnings = FALSE)

# 1. QA Flags by Sensor - Stacked bar chart showing flag distribution
ggplot(master_data_qc, aes(x = serial_number, fill = qa_flag)) +
  geom_bar(position = "fill") +                        # Stacked bar chart (proportions sum to 1)
  scale_y_continuous(labels = scales::percent) +       # Format y-axis as percentages
  coord_flip() +                                       # Flip to horizontal bars for readability
  
  
# CALCULATE SALINITY 
# Convert electrical conductivity to salinity using temperature correction
master_data_qc <- master_data_qc %>%
mutate(salinity = ec_to_sal(temperature_c, conductivity_uscm))

# CORRECT GW LEVEL BY WELL 
master_data_qc <- master_data_qc %>%
  left_join(well_corrections, by = "serial_number") %>%
  mutate(
    corrected_level = if_else(
      !is.na(cable_correction),           # If correction value exists
      level_m - cable_correction,         # Apply correction
      level_m                             # Otherwise use original value
    )
  )

# DATA VISUALIZATION 

# Plot Corrected Groundwater Level over time
ggplot(df) +
  geom_line(aes(x = Time, y = salinity)) +  
  geom_line(aes(x = Time, y = Corrected_Level, color = "blue")) +         # line for trend
  labs(
    title = "Corrected Groundwater Level Over Time",
    x = "Time",
    y = "Corrected Level (m)"
  ) +
  theme_minimal()

ggplot(df) +
  geom_smooth(aes(x = Time, y = salinity)) +  
  #  geom_smooth(aes(x = Time, y = Corrected_Level, color = "blue")) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +  # horizontal line at 0.5
  labs(
    x = "Time",
    y = "Salinity (ppt)"
  ) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 2, ymax = 4, 
           fill = "gray", alpha = 0.3) +  # shaded area for 2-4 ppt
  scale_y_continuous(limits = c(0, NA)) +  # start y-axis at 0
  theme_minimal()