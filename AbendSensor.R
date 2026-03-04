# ==================================
# Full QA/QC Pipeline (Split Date/Time)
# ==================================

library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(hms)
library(zoo)

# ----------------------------
# Load Data
# ----------------------------
df_raw <- read_csv("SolinstCloud Prod Project Report V1.1.0_All Data Table_Table.csv")

# ----------------------------
# User-Defined Settings
# ----------------------------
serial_filter <- "1094201"
start_date <- as.Date("2025-09-01")  # local date comparison

expected_interval_mins <- 60   # interval between readings

# Physical bounds
temp_min <- -5
temp_max <- 50
cond_min <- 0
cond_max <- 100000
level_min <- -50
level_max <- 500

# Rate-of-change thresholds
temp_spike_thresh <- 3
cond_spike_thresh <- 500
level_spike_thresh <- 0.5

# Z-score threshold
z_thresh <- 3

# Flatline detection
flatline_hours <- 24

# ----------------------------
# Clean + Parse Timestamps (Split Date/Time)
# ----------------------------
df <- df_raw %>%
  mutate(
    serial_number = as.character(SerialNumber),
    # clean non-breaking spaces
    Time_clean = str_replace_all(Time, "\u202F", " "),
    Time_clean = str_trim(Time_clean),
    # parse full datetime with AM/PM in local timezone
    Time = mdy_hms(Time_clean, tz = "America/New_York"),
    Time_24hr = format(Time, "%Y-%m-%d %H:%M:%S")
  ) %>%
  select(-Time_clean) %>%
  rename(
    conductivity_uscm = Conductivity,
    temperature_c = Temperature,
    level_m = `Compensated Level`
  ) %>%
  filter(serial_number == serial_filter,
         Time >= as.POSIXct(start_date, tz = "America/New_York")) %>%
  arrange(Time)

# ----------------------------
# Time QA
# ----------------------------
df <- df %>%
  mutate(
    time_diff_mins = as.numeric(difftime(Time, lag(Time), units = "mins")),
    duplicate_time = duplicated(Time),
    gap_flag = time_diff_mins > expected_interval_mins * 1.5
  )

# ----------------------------
# Range Checks
# ----------------------------
df <- df %>%
  mutate(
    temp_range_flag  = temperature_c < temp_min | temperature_c > temp_max,
    cond_range_flag  = conductivity_uscm < cond_min | conductivity_uscm > cond_max,
    level_range_flag = level_m < level_min | level_m > level_max
  )

# ----------------------------
# Rate-of-Change Checks
# ----------------------------
df <- df %>%
  mutate(
    d_temp  = temperature_c - lag(temperature_c),
    d_cond  = conductivity_uscm - lag(conductivity_uscm),
    d_level = level_m - lag(level_m),
    
    temp_spike_flag  = abs(d_temp)  > temp_spike_thresh,
    cond_spike_flag  = abs(d_cond)  > cond_spike_thresh,
    level_spike_flag = abs(d_level) > level_spike_thresh
  )

# ----------------------------
# Z-Score Outlier Detection
# ----------------------------
df <- df %>%
  mutate(
    temp_z  = as.numeric(scale(temperature_c)),
    cond_z  = as.numeric(scale(conductivity_uscm)),
    level_z = as.numeric(scale(level_m)),
    
    temp_z_flag  = abs(temp_z)  > z_thresh,
    cond_z_flag  = abs(cond_z)  > z_thresh,
    level_z_flag = abs(level_z) > z_thresh
  )

# ----------------------------
# Flatline Detection
# ----------------------------
run_length_flag <- function(x, interval_mins, flat_hours){
  r <- rle(x)
  lengths_in_hours <- r$lengths * interval_mins / 60
  rep(lengths_in_hours >= flat_hours, r$lengths)
}

df <- df %>%
  mutate(
    temp_flat_flag  = run_length_flag(temperature_c, expected_interval_mins, flatline_hours),
    cond_flat_flag  = run_length_flag(conductivity_uscm, expected_interval_mins, flatline_hours),
    level_flat_flag = run_length_flag(level_m, expected_interval_mins, flatline_hours)
  )

# ----------------------------
# Consolidated QA Flag
# ----------------------------
df <- df %>%
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
    qa_code = case_when(
      qa_flag == "OK" ~ 0,
      qa_flag %in% c("Statistical Outlier","Time Gap") ~ 1,
      TRUE ~ 2
    )
  )

# ----------------------------
# Summary Report
# ----------------------------
qa_summary <- df %>%
  count(qa_flag) %>%
  mutate(percent = round(n / sum(n) * 100, 2))

print(qa_summary)
cat("\nTotal records:", nrow(df), "\n")
cat("Percent flagged (not OK):",
    round(sum(df$qa_flag != "OK") / nrow(df) * 100, 2), "%\n")

# ----------------------------
# Visual Check
# ----------------------------
ggplot(df, aes(Time, level_m)) +
  geom_line(color = "black") +
  geom_point(data = df %>% filter(qa_flag != "OK"),
             aes(Time, level_m),
             color = "red", size = 1) +
  ggtitle("Level with QA Flags (Red = Flagged)")


library(OGFLtools)
library(dplyr)


df <- df %>%
  filter(as.Date(Time) != as.Date("2026-02-17"))

# Calculate salinity
df <- df %>%
  mutate(
    salinity = ec_to_sal(temperature_c, conductivity_uscm)
  )

# Quick summary
summary(df$salinity)

# Plot salinity over time
ggplot(df, aes(Time, salinity)) +
  geom_line() +
  ggtitle("Salinity over Time") +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA))  # y-axis starts at 0

# calculating corrected groundwater level 
df <- df %>% mutate(Corrected_Level = level_m - 1.8)

# Plot Corrected Groundwater Level over time
ggplot(df, aes(x = Time, y = Corrected_Level)) +
  geom_line(color = "blue") +         # line for trend
  labs(
    title = "Corrected Groundwater Level Over Time",
    x = "Time",
    y = "Corrected Level (m)"
  ) +
  theme_minimal()


