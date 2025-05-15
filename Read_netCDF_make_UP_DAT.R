# -----------------------------------------------------------------------------------
# Make dataframe of Upper air data from netCDF:
# -----------------------------------------------------------------------------------

library(ncdf4)
library(reshape2)
library(stringr)
library(dplyr)

# Set your folder path
files <- list.files(pattern = "\\.grib2\\.nc$", full.names = TRUE)

# Pressure levels of interest (in Pa)
levels <- c(1000, 975, 950, 900, 800, 700, 600, 500, 400, 300) * 100

level_indices <- 1:10
latitude = 49.25 # replace with the grid point coordinates
longitude = -123.25 # replace with the grid point coordinates

# Initialize combined dataframe
all_data <- list()

for (file in files) {
  # Open NetCDF
  nc <- nc_open(file)
  
  # Extract dimension vectors
  lon <- longitude
  lat <- latitude
  
  # Extract variables
  temp <- ncvar_get(nc, "TMP_L100")
  rh   <- ncvar_get(nc, "R_H_L100")
  u    <- ncvar_get(nc, "U_GRD_L100")
  v    <- ncvar_get(nc, "V_GRD_L100")
  h <- ncvar_get(nc, "HGT_L100")
  
  nc_close(nc)
  
  # Parse time from filename
  time_string <- str_extract(file, "\\d{10}")
  time_parsed <- as.POSIXct(time_string, format = "%Y%m%d%H", tz = "UTC")
  
  # Build individual df
  u_flat <- as.vector(u)
  v_flat <- as.vector(v)
  
  df <- data.frame(matrix(nrow = 10, ncol = 0))
  
  df$datetime <- rep(time_parsed, 10)
  df$timestring <- rep(time_string, 10)
  df$lon <- rep(lon, 10)
  df$lat <- rep(lat, 10)
  df$level <- levels
  df$height <- as.vector(h)
  df$temp <- as.vector(temp)
  df$rh <- as.vector(rh)
  df$u <- u_flat
  df$v <- v_flat
  df$wind_speed <- sqrt(u_flat^2 + v_flat^2)
  df$wind_dir <- (atan2(-u_flat, -v_flat) * 180 / pi) %% 360  # from where the wind is coming
  df$origin <- rep(file, 10)
  
  all_data[[length(all_data) + 1]] <- df

  message("Processed: ", basename(file))
}

# Combine all
final_df <- bind_rows(all_data)

# Save to one CSV file
output_file <- file.path("upper_air_all_data.csv")
write.csv(final_df, file = output_file, row.names = FALSE)

message("Saved all data to: ", output_file)



# -----------------------------------------------------------------------------
# Make UP.DAT file:
# -----------------------------------------------------------------------------

library(lubridate)

# Example: your data frame is named `df`
# Variables: datetime, timestring, lon, lat, level, height, temp, rh, u, v, wind_speed

# Metadata
Fixed <- 9999
Seconds <- 0
station_id <- 12345
TotalLevels <- 10
Levels <- 10

df <- final_df

# Get unique datetimes
df <- df %>% arrange(datetime, desc(level))  # Sort so highest pressure first (1000 → 300)
unique_times <- unique(df$datetime)

# Output file
file_conn <- file("UpperAir_from_R_dataframe.txt", "wt")

day_t1 <- df$datetime[1]
day_tf <- df$datetime[nrow(df)]
min_lvl <- min(df$level/100, na.rm = TRUE)


# Write general heading
writeLines("UP.DAT          2.1             Hour Start and End Times with Seconds                           ", file_conn)
writeLines("   1", file_conn)
writeLines("Produced by READ62 Version: 5.661  Level: 110225                                ", file_conn)
writeLines("NONE    ", file_conn)
writeLines("UTC+0000", file_conn)

# Write special line with time range and min level
special_line <- sprintf(
  "  %4d%5d%5d%5d %4d%5d%5d%5d%s%5d%5d",
  as.numeric(sprintf('%02d', year(day_t1) %% 100)),
  as.numeric(format(day_t1, "%j")),
  as.numeric(sprintf('%03d', hour(day_t1) %% 100)),
  0,
  as.numeric(sprintf('%02d', year(day_tf) %% 100)),
  as.numeric(format(day_tf, "%j")),
  as.numeric(sprintf('%03d', hour(day_tf) %% 100)),
  0,
  paste0(" ", min_lvl, "."),
  1,
  2
)
writeLines(special_line, file_conn)
writeLines("     F    F    F    F", file_conn)



# Loop over time steps
for (dt in unique_times) {
  df_sub <- df %>% filter(datetime == dt)
  day_t <- df_sub$datetime[1]
  
  # Extract time parts
  year <- as.numeric(sprintf('%02d', year(day_t) %% 100))
  full_year <- year(day_t)
  month <- month(day_t)
  day <- day(day_t)
  julian <- as.numeric(format(day_t, "%j"))
  hour <- hour(day_t)
  
  # Write header
  header <- sprintf("%7d %9d  %6d%3d%4d%3d%5d  %6d%3d%4d%3d%5d%5d%8d",
                    Fixed, station_id, full_year, month, day, hour, Seconds, 
                    full_year, month, day, hour, Seconds, TotalLevels, Levels)
  # header <- sprintf("   %4d     %5s    %4d  %2d  %3d  %2d     %2d           %2d",
  #                   Fixed, station_id, full_year,
  #                   month, julian, hour,
  #                   TotalLevels, Levels)
  writeLines(header, file_conn)
  
  # Write level data (assumes 10 rows)
  # Loop over levels in groups of 4
  for (i in seq(1, nrow(df_sub), by = 4)) {
    group_line <- ""
    for (j in 0:3) {
      idx <- i + j
      if (idx <= nrow(df_sub)) {
        level_hPa <- df_sub$level[idx] / 100
        height <- df_sub$height[idx]
        temp <- df_sub$temp[idx]
        wind_dir <- (atan2(df_sub$u[idx], df_sub$v[idx]) * 180 / pi + 360) %% 360
        wind_speed <- df_sub$wind_speed[idx]
        
        part <- sprintf(" %8.1f,%5.0f,%6.1f,%3.0f,%3.0f",
                        level_hPa, height, temp, wind_dir, wind_speed)
        
        group_line <- paste0(group_line, part)
      }
    }
    writeLines(group_line, file_conn)
  }}
  
close(file_conn)