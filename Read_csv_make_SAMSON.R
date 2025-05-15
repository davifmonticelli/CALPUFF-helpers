# ------------------------------------------------------------------------------
# Pre-process data:
# ------------------------------------------------------------------------------

# Import surface .csv file that will become SAMSON:
library(readr)

# Example, from BC Station Data: https://www.pacificclimate.org/data/bc-station-data

# Format:

# station_observations														
# wind_direction	 air_temperature	 wind_gust_speed	 total_cloud_cover	 tendency_amount	 wind_speed	 mean_sea_level	 dew_point	 snow_amount	 air_temperature_yesterday_high	 relative_humidity	 time	 air_temperature_yesterday_low	 total_precipitation	 rain_amount
# 90	7.2	 None	8	-0.1	15	101.7	6.9	 None	 None	98	 2021-01-01 00:00:00	 None	 None	 None
# 67.5	7.2	 None	8	-0.2	23	101.5	6.8	 None	 None	97	 2021-01-01 01:00:00	 None	 None	 None
# 67.5	7.5	32	8	-0.2	22	101.5	6.9	 None	 None	96	 2021-01-01 02:00:00	 None	 None	 None


AIRPORT_data <- read_csv("AIRPORT_2021.csv", 
                         skip = 1)

# From POWER DAV (NASA): https://power.larc.nasa.gov/data-access-viewer/

# Format:

# -BEGIN HEADER-						
# NASA/POWER Source Native Resolution Hourly Data 						
# Dates (month/day/year): 01/01/2021 through 03/31/2021 in LST						
# Location: Latitude  49.0985   Longitude -123.0631 						
# Elevation from MERRA-2: Average for 0.5 x 0.625 degree lat/lon region = 62.89 meters						
# The value for missing source data that cannot be computed or is outside of the sources availability range: -999 						
# Parameter(s): 						
# ALLSKY_SFC_SW_DWN      CERES SYN1deg All Sky Surface Shortwave Downward Irradiance (Wh/m^2) 						
# ALLSKY_SFC_SW_DNI      CERES SYN1deg All Sky Surface Shortwave Downward Direct Normal Irradiance (Wh/m^2) 						
# ALLSKY_SFC_SW_DIFF     CERES SYN1deg All Sky Surface Shortwave Diffuse Irradiance (Wh/m^2) 						
# -END HEADER-						
# YEAR	MO	DY	HR	ALLSKY_SFC_SW_DWN	ALLSKY_SFC_SW_DNI	ALLSKY_SFC_SW_DIFF
# 2021	1	1	0	0	0	0
# 2021	1	1	1	0	0	0
# 2021	1	1	2	0	0	0

SOLAR_data <- read_csv("SOLAR_2021.csv", 
                       skip = 11)

# Fix skipped times and merge datasets:
library(openair)
library(lubridate)
library(dplyr)

AIRPORT_data <- AIRPORT_data %>% mutate(across(.cols = -time, .fns = as.double))
names(AIRPORT_data)[names(AIRPORT_data) == 'time'] <- 'date'
colnames(SOLAR_data) <- c("YR", "MO", "DA", "HR", "global_horizontal_radiation", "direct_normal_radiation", "direct_horizontal_radiation")
SOLAR_data <- SOLAR_data %>% mutate(date = make_datetime(YR, MO, DA, HR))

if (nrow(AIRPORT_data) == nrow(SOLAR_data)){
  final_data <- merge(AIRPORT_data, SOLAR_data, by = "date")
}else{
  # Ensure df$date is POSIXct and ordered
  fill_missing_hours <- function(df, datetime_col = "date") {
    # Ensure datetime column is POSIXct
    df[[datetime_col]] <- as.POSIXct(df[[datetime_col]])
    
    # Order by datetime
    df <- df[order(df[[datetime_col]]), ]
    
    # Create full hourly sequence from first to last timestamp
    full_seq <- data.frame(date_full = seq(from = df[[datetime_col]][1], 
                                           to = df[[datetime_col]][nrow(df)], 
                                           by = "hour"))
    
    # Rename original datetime column temporarily to match
    names(df)[names(df) == datetime_col] <- "date_full"
    
    # Merge to get full series with NA for missing hours
    df_filled <- merge(full_seq, df, by = "date_full", all.x = TRUE)
    
    # Rename datetime column back to original
    names(df_filled)[names(df_filled) == "date_full"] <- datetime_col
    
    return(df_filled)
  }
  airport_1h <- fill_missing_hours(AIRPORT_data)
  solar_1h <- fill_missing_hours(SOLAR_data)
  final_data <- merge(AIRPORT_data, SOLAR_data, by = "date")
}


library(openair)
openair_check_df <- final_data
openair_check_df$wind_speed <- openair_check_df$wind_speed/3.6 
windRose(openair_check_df, ws = "wind_speed", wd = "wind_direction", type = "month")
windRose(openair_check_df, ws = "wind_speed", wd = "wind_direction", type = "daylight")

# ------------------------------------------------------------------------------
# Make SANSOM file:
# ------------------------------------------------------------------------------

final_data$wind_speed <- final_data$wind_speed/3.6 # convert to m/s
final_data$cloud_cover_tenths <- round(final_data$total_cloud_cover * 10 / 8)

# Apply to specific columns
final_data <- fill_missing_hours(final_data)

library(zoo)
z <- read.zoo(final_data, FUN = identity)
z <- na.approx(z)
final_data_test <- data.frame(date = index(z), coredata(z))


# -------------------------------------------------------------------------------
# Check missing hours before proceeding
# -------------------------------------------------------------------------------

# Ensure your column is POSIXct
final_data_test$date <- as.POSIXct(final_data_test$date)

# Create complete hourly sequence from start to end
full_seq <- seq(from = final_data_test$date[1], 
                to = final_data_test$date[nrow(final_data_test)], by = "hour")

# Find missing hours
missing_hours <- setdiff(full_seq, final_data_test$date)

# Print missing hours (if any)
if (length(missing_hours) > 0) {
  cat("Missing hourly timestamps:\n")
  print(missing_hours)
} else {
  cat("✅ No missing hourly timestamps in the date column.\n")
}


# -------------------------------------------------------------------------------

# IF OK
final_data <- final_data_test

# Output file path
output_file <- "SURFACE_fromR_AIRPORT.txt"

# Header line (example station info)
# Station ID, CITY, STATE, TIME ZONE, LAT, LONG, HEIGHT
header_line <- "~10253 VANCOUVER              BC  -8  N49 19  W123 18     10"

# Variable positions (line 2)
positions_line <- "~YR MO DA HR I    1    2       3       4       5  6  7     8     9  10   11  12    13     14     15        16   17     18   19  20      21"

# Write headers
writeLines(c(header_line, positions_line), output_file)

# Helper to replace NA with SAMSON missing values
samson_na <- function(x, type = "numeric") {
  if (is.na(x)) {
    return(
      switch(type,
             glo_rad = "9999", # %4s
             dir_rad = "9999", # %4s
             dif_rad = "9999", # %4s
             cloud = "99", # %2s
             temp = "9999.", # %5s
             dew = "9999.", # %5s
             rh = "999", # %3s
             press = "9999", # %4s
             wd = "999", # %3s
             ws = "99.0", # %4s
             snow = "9999" # %4s
             )
    )
  } else {
    return(
      switch(type,
             glo_rad = sprintf("%4d", as.integer(x)),
             dir_rad = sprintf("%4d", as.integer(x)),
             dif_rad = sprintf("%4d", as.integer(x)),
             cloud = sprintf("%2d", as.integer(x)),
             temp = sprintf("%2.1f", x),
             dew = sprintf("%2.1f", x),
             rh = sprintf("%3d", as.integer(x)),
             press = sprintf("%4d", as.integer(round(x* 10))),  # e.g. 101.8 → 1018
             wd = sprintf("%3d", as.integer(x)),
             ws = sprintf("%2.1f", x),
             snow = sprintf("%4d", as.integer(x)),
             as.character(x))
    )
  }
}

# Loop through rows and write SAMSON-formatted data
for (i in 1:nrow(final_data)) {
  row <- final_data[i, ]
  
  samson_line <- sprintf(
    " %2d %2d %2d %2d %1s %4s %4s %4s %2s %4s %2s %4s %2s %2s %2s %5s %5s %3s %4s %3s  %4s %6s %6s         %1s %4s %6s %4s %3s %15s",
    as.integer(row$YR) %% 100,  # Last 2 digits of year
    as.integer(row$MO),
    as.integer(row$DA),
    as.integer(row$HR),
    "0",
    "9999",  # placeholder
    "9999",  # placeholder
    samson_na(row$global_horizontal_radiation, "glo_rad"),
    "?0",
    samson_na(row$direct_normal_radiation, "dir_rad"),
    "?0",
    samson_na(row$direct_horizontal_radiation, "dif_rad"),
    "?0",
    samson_na(row$cloud_cover_tenths, "cloud"),
    samson_na(row$cloud_cover_tenths, "cloud"),  # opaque cloud cover (repeats previous)
    samson_na(row$air_temperature, "temp"),
    samson_na(row$dew_point, "dew"),
    samson_na(row$relative_humidity, "rh"),
    samson_na(row$mean_sea_level, "press"),
    samson_na(row$wind_direction, "wd"),
    samson_na(row$wind_speed, "ws"),
    "99999.",  # visibility
    "999999",  # ceiling height
    "9", # present weather
    "9999",    # precipitable water
    "99999.",  # aerosol optical depth
    samson_na(row$snow_amount, "snow"),
    "999",     # days since last snowfall
    "               ",
    "finish" # this line will not print but makes everything work
  )
  
  write(samson_line, file = output_file, append = TRUE)
}
