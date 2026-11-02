library(OGFLtools)
library(tidyverse)
library(hms)
library(lubridate)   
library(stringr)     
library(ggplot2)     
library(zoo)        
library(OGFLtools)   
library(ggside)
library(dplyr)
library(googledrive)
library(readr)

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

#processing of one file
file <- all_csv %>% filter(name == "SolinstCloud Prod Project Report V1.1.0_All Data Table_Table (1).csv")

df <- file %>%
  mutate(
    # Ensure SerialNumber is character
    serial_number = as.character(SerialNumber)) %>% 
  # Rename columns as desired
  rename(
    conductivity_uscm = Conductivity,
    temperature_c = Temperature,
    level_m = `Compensated Level`
  )

# QA/QC 

# USER-DEFINED SETTINGS
serial_filter <- "1094251"
start_date <- as.POSIXct("2025-09-01", tz = "UTC")

expected_interval_mins <- 60   # change if needed

# Physical bounds (adjust to site conditions)
temp_min <- -5
temp_max <- 50

cond_min <- 0
cond_max <- 100000

level_min <- -50
level_max <- 500

# Rate-of-change thresholds (per interval)
temp_spike_thresh <- 3        # °C
cond_spike_thresh <- 500      # µS/cm
level_spike_thresh <- 0.5     # m

z_thresh <- 3                 # z-score cutoff
flatline_hours <- 24          # hours of identical values


