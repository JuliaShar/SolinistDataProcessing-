library(ggplot2)
library(scales)
library(ggside)
library(OGFLtools)
library(tidyverse)
library(hms)
library(lubridate)
library(zoo)

df <- read_csv("SolinstCloud Prod Project Report V1.1.0_All Data Table_Table.csv")

# Clean up the dataframe
df <- df %>%
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


# STRUCTURAL CHECK

required_cols <- c("SerialNumber","Time","conductivity_uscm", "temperature_c", "level_m")
missing_cols <- setdiff(required_cols, colnames(df))

if(length(missing_cols) > 0){
  stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
}

# CLEAN + FORMAT
df <- df %>%
  mutate(
    serial_number = as.character(SerialNumber)) %>%
  filter(
    serial_number == serial_filter,
    Time >= start_date
  ) %>%
  arrange(Time)

# TIME QA
# Clean hidden non-breaking spaces in original Time column

df <- df %>%
mutate(Time_clean = str_replace_all(Time, "\u202F", " "),
       Time_clean = str_trim(Time_clean)) %>%
  
  # Parse AM/PM timestamps in local timezone (DST-safe)
  mutate(Time = mdy_hms(Time_clean, tz = "America/New_York")) %>%
  
  # Create 24-hour formatted character column for duplicate detection
  mutate(Time_24hr = format(Time, "%Y-%m-%d %H:%M:%S")) %>%
  
  # Remove intermediate column
  select(-Time_clean) %>%
  
  # Order by Time
  arrange(Time) %>%
  
  # QA: detect gaps and duplicates
  mutate(
    time_diff_mins = as.numeric(difftime(Time, lag(Time), units = "mins")),
    
    # Detect duplicates **based on parsed POSIXct**, not raw string
    duplicate_time = duplicated(Time),
    
    # Flag gaps
    gap_flag = time_diff_mins > expected_interval_mins * 1.5
  )

# RANGE CHECKS
df <- df %>%
  mutate(
    temp_range_flag  = temperature_c < temp_min | temperature_c > temp_max,
    cond_range_flag  = conductivity_uscm < cond_min | conductivity_uscm > cond_max,
    level_range_flag = level_m < level_min | level_m > level_max
  )

# RATE-OF-CHANGE CHECKS

df <- df %>%
  mutate(
    d_temp  = temperature_c - lag(temperature_c),
    d_cond  = conductivity_uscm - lag(conductivity_uscm),
    d_level = level_m - lag(level_m),
    
    temp_spike_flag  = abs(d_temp)  > temp_spike_thresh,
    cond_spike_flag  = abs(d_cond)  > cond_spike_thresh,
    level_spike_flag = abs(d_level) > level_spike_thresh
  )

#  Z-SCORE OUTLIERS

df <- df %>%
  mutate(
    temp_z  = as.numeric(scale(temperature_c)),
    cond_z  = as.numeric(scale(conductivity_uscm)),
    level_z = as.numeric(scale(level_m)),
    
    temp_z_flag  = abs(temp_z)  > z_thresh,
    cond_z_flag  = abs(cond_z)  > z_thresh,
    level_z_flag = abs(level_z) > z_thresh
  )

# FLATLINE DETECTION

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

# CONSOLIDATED QA FLAG

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
    )
  )

# Numeric flag (regulatory-style)
df <- df %>%
  mutate(
    qa_code = case_when(
      qa_flag == "OK" ~ 0,
      qa_flag %in% c("Statistical Outlier","Time Gap") ~ 1,
      TRUE ~ 2
    )
  )

# SUMMARY REPORT

qa_summary <- df %>%
  count(qa_flag) %>%
  mutate(percent = round(n / sum(n) * 100, 2))

print(qa_summary)

cat("\nTotal records:", nrow(df), "\n")
cat("Percent flagged (not OK):",
    round(sum(df$qa_flag != "OK") / nrow(df) * 100, 2), "%\n")

# Visual Check 
ggplot(df, aes(Time, level_m)) +
  geom_line(color = "black") +
  geom_point(data = df %>% filter(qa_flag != "OK"),
             aes(Time, level_m),
             color = "red", size = 1) +
  ggtitle("Level with QA Flags (Red = Flagged)")

# Assessing for duplicate timestamps 
df %>%
  group_by(SerialNumber, Time) %>%
  tally() %>%
  filter(n > 1)

# Assessing for data gaps 
df %>%
  arrange(Time) %>%
  mutate(diff_minutes = as.numeric(difftime(Time, lag(Time), units = "mins")))

# calculating corrected groundwater level 
df <- df %>% 
  mutate(Corrected_Level = Compensated_Level - 1.8) 

# Define function to convert conductivity to salinity 
convert_conductivity_to_salinity <- function(conductivity, temperature) {
  salinity <- (0.5811 * conductivity) - 0.1296
  return(salinity)
}


data$Conductivity_m_cm = (data$Conductivity /1000)

# Process and visualize the data
data <- data %>%
  filter(SerialNumber %in% c("1094251", "1094201")) %>%
  mutate(Salinity = convert_conductivity_to_salinity(Conductivity_m_cm)) %>% # Convert to S/m for salinity calculation
  mutate(Sensor = case_when(
    SerialNumber == "1094251" ~ "Sensor 1",
    SerialNumber == "1094201" ~ "Sensor 2"))

# Plotting salinity over time with ggplot2
ggplot(data_processed, aes(x = Time, y = Salinity, color = Sensor)) +
  geom_line(size = 1) +
  geom_point(size = 1) +
  labs(title = "Salinity over Time",
       x = "Time",
       y = "Salinity (ppt)",
       color = "Sensor") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "bottom")

# Convert data to long format for faceting, including conductivity, salinity, temperature, and level
data_long <- data %>%
  pivot_longer(cols = c(Corrected_Level, Salinity), 
               names_to = "Measurement", 
               values_to = "Value")

# Define custom colors
custom_colors <- c("Salinity" = "#00BFC4",  # Custom color for Salinity
                   "Corrected_Level" = "#F8766D")  # Custom color for Level

ggplot(data_long, aes(x = Time, y = Value, color = Measurement)) +
  geom_line(size = 1) +
  geom_point(size = 1) +
  # facet_wrap(~ Sensor) +  # Create separate panels for each sensor
  labs(title = "Salinity and Level over Time by Sensor",
       x = "Time",
       y = "Value") +  # Customize y-axis label here
  theme_minimal() +
  scale_fill_manual(values = custom_colors) +  # Apply custom color
  theme(legend.position = "bottom", 
        axis.title.x = element_text(family = "Arial", size = 14),  # Customize font size if desired
        axis.title.y = element_text(family = "Arial", size = 14),  # Customize the y-axis title font
        plot.title = element_text(family = "Arial", size = 14, face = "bold"),  # Customize title font
        panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
        panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
        panel.grid.major.y = element_line(colour = "light grey"),  # Retain the horizontal grid lines
        #panel.grid.minor.y = element_line(colour = "grey")   # Retain minor horizontal grid lines
  )

ggplot(data_long, aes(x = Measurement, y = Value, fill = Measurement)) +
  geom_boxplot() +  # Create boxplots
  labs(title = "Boxplots of Salinity and Level by Sensor",
       x = "Measurement",
       y = "Value") +
  theme_minimal() +
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  theme(
    legend.position = "none", 
    axis.title.x = element_text(family = "Arial", size = 14),  # Customize x-axis title font
    axis.title.y = element_text(family = "Arial", size = 14),  # Customize y-axis title font
    plot.title = element_text(family = "Arial", size = 14, face = "bold"),  # Customize title font
    panel.background = element_blank(),  # Remove background color of panel
    plot.background = element_blank(),  # Remove background color of the entire plot
    panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
    panel.grid.minor.x = element_blank()   # Remove minor vertical grid lines
  )

ggplot(data_processed, aes(x = Sensor, y = Salinity, fill = Sensor)) +
  geom_boxplot() +
  labs(title = "Salinity Distribution by Sensor") +
  theme_minimal(base_family = "Arial") +  # Set base font family to Arial
  scale_color_brewer(palette = "Set1") +
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(family = "Arial", size = 12),  # Optional: customize font size if desired
    axis.title.y = element_text(family = "Arial", size = 12),  # Customize the y-axis title font as well
    plot.title = element_text(family = "Arial", size = 14, face = "bold")  # Customize title font if desired
  )

# Summary statistics calculation
summary_stats <- data_long %>%
  group_by(Measurement) %>%  # Group by Measurement type (e.g., salinity, level)
  summarise(
    Average = mean(Value, na.rm = TRUE),  # Calculate mean, removing NA values
    Standard_Deviation = sd(Value, na.rm = TRUE)  # Calculate standard deviation, removing NA values
  )

# View the summary statistics
print(summary_stats)

ggplot(data, aes(x = DateTime, y = Temperature, color = SerialNumber)) + geom_point() + geom_line()  +
  scale_x_datetime(breaks=date_breaks("1 week"),
                   minor_breaks=date_breaks("1 day")) + 
  theme_bw()

sensor1data = data %>% 
  filter(SerialNumber %in% c("1094251"))

sensor2data = data %>% 
  filter(SerialNumber %in% c("1094201"))

ggplot(sensor1data, aes(x = DateTime)) +
  geom_line(aes(y = Conductivity, color = "Conductivity")) +
  geom_line(aes(y = Temperature * 100, color = "Temperature")) +  # Scale temperature for visualization
  scale_y_continuous(
    name = "Conductivity (µS/cm)",
    sec.axis = sec_axis(~./100, name = "Temperature (°C)")
  ) +
  labs(title = "Conductivity and Temperature Over Time") +
  theme_minimal()

# Calculate correlation coefficient
cor.test(sensor1data$Conductivity, sensor1data$Temperature)

ggplot(sensor2data, aes(x = DateTime)) +
  geom_line(aes(y = Conductivity, color = "Conductivity")) +
  geom_line(aes(y = Temperature * 100, color = "Temperature")) +  # Scale temperature for visualization
  scale_y_continuous(
    name = "Conductivity (µS/cm)",
    sec.axis = sec_axis(~./100, name = "Temperature (°C)")
  ) +
  labs(title = "Conductivity and Temperature Over Time") +
  theme_minimal()

# Calculate correlation coefficient
cor.test(sensor2data$Conductivity, sensor2data$Temperature)

# Filter and prepare data for both sensors
sensor1data <- data %>% 
  filter(SerialNumber == "1094251") %>%
  mutate(Sensor = "Sensor 1") #Abends 

sensor2data <- data %>% 
  filter(SerialNumber == "1094201") %>%
  mutate(Sensor = "Sensor 2") #Blackwater

# Combine the datasets
combined_data <- rbind(sensor1data, sensor2data)

# Plotting both sensors in the same ggplot
ggplot(combined_data, aes(x = DateTime)) +
  geom_line(aes(y = Conductivity, color = "Conducitvity"), size = 1) +
  geom_line(aes(y = Temperature * 100, color = "Temperature"), size = 1) +  # Scale temperature for visualization
  scale_y_continuous(
    name = "Conductivity (µS/cm)",
    sec.axis = sec_axis(~./100, name = "Temperature (°C)")
  ) +
  facet_wrap(~ Sensor) +  # Create separate plots for each sensor
  labs(
    title = "Conductivity and Temperature Over Time",
    color = "Measurement",
    x = "Time"  # Set x-axis title to "Time"
  ) +
  theme_minimal(base_family = "Arial") +  # Set base font family to Arial
  scale_color_brewer(palette = "Set2") +
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(family = "Arial", size = 12),  # Optional: customize font size if desired
    axis.title.y = element_text(family = "Arial", size = 12),  # Customize the y-axis title font as well
    plot.title = element_text(family = "Arial", size = 14, face = "bold")  # Customize title font if desired
  )


# Plotting Salinity 

ggplot(data, aes(x = DateTime, y = Salinity_PSU)) + geom_line() 

#Plotting both sensors in the same ggplot
ggplot(combined_data, aes(x = DateTime)) +
  geom_line(aes(y = Salinity_PSU, color = "Salinity"), size = 1) +
  geom_line(aes(y = Temperature, color = "Temperature"), size = 1) +  # Scale temperature for visualization
  scale_y_continuous(
    name = "Salinity (PSU)",
    sec.axis = sec_axis(~./100, name = "Temperature (°C)")
  ) +
  facet_wrap(~ Sensor) +  # Create separate plots for each sensor
  labs(
    title = "Salinity and Temperature Over Time",
    color = "Measurement",
    x = "Time"  # Set x-axis title to "Time"
  ) +
  theme_minimal(base_family = "Arial") +  # Set base font family to Arial
  scale_color_brewer(palette = "Set2") +
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(family = "Arial", size = 12),  # Optional: customize font size if desired
    axis.title.y = element_text(family = "Arial", size = 12),  # Customize the y-axis title font as well
    plot.title = element_text(family = "Arial", size = 14, face = "bold")  # Customize title font if desired
  )

# Function to convert conductivity to salinity
convert_conductivity_to_salinity <- function(conductivity) {
  # Using a simple empirical conversion (this example divides by 1000)
  salinity <- conductivity / 1000  # Modify as needed according to your specific conversion
  return(salinity)
}

# Applying conversion to the data
data <- data %>%
  mutate(Salinity = convert_conductivity_to_salinity(Conductivity))


# Filter and prepare data for both sensors and calculate salinity
sensor1data <- data %>% 
  filter(SerialNumber == "1094251") %>%
  mutate(Sensor = "Sensor 1")

sensor2data <- data %>% 
  filter(SerialNumber == "1094201") %>%
  mutate(Sensor = "Sensor 2")

# Calculate salinity if it's not done yet
sensor1data <- sensor1data %>%
  mutate(Salinity = convert_conductivity_to_salinity(Conductivity))

sensor2data <- sensor2data %>%
  mutate(Salinity = convert_conductivity_to_salinity(Conductivity))

# Combine the datasets
combined_data <- rbind(sensor1data, sensor2data)

# Plot with a custom color palette
ggplot(combined_data, aes(x = DateTime)) +
  geom_line(aes(y = Salinity, color = "Salinity (ppt)"), size = 1) +
  geom_line(aes(y = Temperature, color = "Temperature (°C)"), size = 1, linetype = "dashed") +
  scale_y_continuous(
    name = "Salinity (ppt)",
    sec.axis = sec_axis(~ ., name = "Temperature (°C)")
  ) +
  facet_wrap(~ Sensor) +  # Separate plots for each sensor
  labs(title = "Salinity and Temperature Over Time", color = "Measurement") +
  scale_color_brewer(palette = "Set2") +  # Use color palette here
  theme_minimal() +
  theme(legend.position = "bottom")
