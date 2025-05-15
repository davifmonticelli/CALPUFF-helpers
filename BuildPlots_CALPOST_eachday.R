# Load required libraries
library(sf)
library(mapview)
library(dplyr)
library(readr)
library(akima)
library(raster)
library(leaflet)

# Read and prepare smells + area data (this is static)
Cannabis_smells_2022 <- read_csv("Cannabis_smells_2022.csv")
# Convert 'date' column in smells dataframe to Date type (only once before the loop)
Cannabis_smells_2022 <- Cannabis_smells_2022 %>%
  mutate(date_parsed = as.Date(date))

# Define polygon regions
create_polygon_sf <- function(coords, name) {
  polygon <- st_sfc(st_polygon(list(as.matrix(coords))), crs = 4326)
  st_sf(name = name, geometry = polygon)
}

urban_coords <- data.frame(lon = c(-123.103474, -123.103361, -123.066034, -123.065845, -123.044163, 
                                   -123.044186, -123.034273, -123.033962, -123.010695, -123.010411, 
                                   -123.043712, -123.043889, -123.103474),
                           lat = c(49.078921, 49.112209, 49.111960, 49.099863, 49.099551, 
                                   49.093807, 49.093668, 49.090957, 49.090932, 49.081214, 
                                   49.081649, 49.078802, 49.078921))
ccf_coords <- data.frame(lon = c(-123.010020, -122.991660, -122.991332, -123.009455, -123.010020),
                         lat = c(49.089891, 49.089645, 49.081185, 49.081391, 49.089891))
rural_coords <- data.frame(lon = c(-122.990299, -122.991069, -122.979944, -122.980355, -122.927318, -122.927826, -122.990299),
                           lat = c(49.081545, 49.090794, 49.091240, 49.105888, 49.105972, 49.081553, 49.081545))

urban_sf <- create_polygon_sf(urban_coords, "Urban region")
ccf_sf <- create_polygon_sf(ccf_coords, "CCF Greenhouses area")
rural_sf <- create_polygon_sf(rural_coords, "Rural region")
all_polygons <- bind_rows(urban_sf, ccf_sf, rural_sf)

# Function to process one file
process_file <- function(file_path) {
  df <- read_csv(file_path, skip = 4)
  colnames(df) <- c("UTM X (km)", "UTM Y (km)", "BVOC (ug/m3)", "lat", "long")
  
  interp_res <- with(df, akima::interp(x = long, y = lat, z = `BVOC (ug/m3)`, 
                                       duplicate = "mean", linear = TRUE,
                                       nx = 1000,  # number of grid points in x-direction (longitude)
                                       ny = 1000   # number of grid points in y-direction (latitude)
  ))
  r <- raster(list(x = interp_res$x, y = interp_res$y, z = interp_res$z))  
  crs(r) <- CRS("+proj=longlat +datum=WGS84")  # optional but good practice
  
  # Color breaks
  myrcene_percents <- c(0.7, 0.6, 0.5, 0.4, 0.3, 0.2)
  odour_breaks <- round(72 / myrcene_percents, 1)
  all_breaks <- sort(unique(c(5, 50, odour_breaks, 500, 1000, 1500, 2000, 2500)))
  
  labels <- sapply(seq_along(all_breaks)[-length(all_breaks)], function(i) {
    lower <- all_breaks[i]
    upper <- all_breaks[i + 1]
    if (lower < 50 || lower < 102.9) {
      sprintf("%s–%s (may not be odorous)", lower, upper)
    } else if (lower > 499) {
      sprintf("%s–%s (odorous)", lower, upper)
    } else if (any(abs(odour_breaks - lower) < 1e-5)) {
      perc <- round(72 / lower * 100)
      sprintf("%s–%s (odorous if %s%% is myrcene)", lower, upper, perc)
    } else {
      sprintf("%s–%s", lower, upper)
    }
  })
  
  pal <- colorBin(
    palette = hcl.colors(length(all_breaks) - 1, "viridis", rev = TRUE),
    bins = all_breaks,
    domain = values(r),
    na.color = "transparent"
  )
  
  # Inside the loop: extract date from filename
  date_str <- sub(".*_(\\d{8})\\.csv$", "\\1", file_path)
  date_formatted <- as.Date(date_str, format = "%Y%m%d")
  
  # Filter smells for the given date
  Smells_filtered <- Cannabis_smells_2022 %>%
    filter(date_parsed == date_formatted)
  
  # Convert to sf
  Smells_2022 <- st_as_sf(Smells_filtered, coords = c("long", "lat"), crs = 4326) 
  
  leaflet() %>%
    addProviderTiles("Esri.WorldGrayCanvas") %>%
    addRasterImage(r, colors = pal, opacity = 0.5, group = "BVOC Raster") %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = values(r),
      title = "BVOC (µg/m³)",
      labFormat = function(type, cuts, p) { labels },
      opacity = 1
    ) %>%
    addCircleMarkers(
      data = Smells_filtered,
      color = "red",
      radius = 4,
      stroke = FALSE,
      fillOpacity = 1,
      group = "Cannabis Smells"
    ) %>%
    addPolygons(
      data = all_polygons,
      fillColor = "transparent",
      weight = 2,
      color = "black",
      group = "Sampling Areas"
    ) %>%
    addLegend(
      position = "bottomright",
      colors = c("red", "black"),
      labels = c("Cannabis smells (dots)", "Sampling areas (black outline)"),
      title = "Map Features",
      opacity = 1
    )
}

# List of CSV file names
filenames <- c(
  "RANK(ALL)_BVOC_1HR_CONC_C_20220727.csv",
  "RANK(ALL)_BVOC_1HR_CONC_C_20220728.csv",
  "RANK(ALL)_BVOC_1HR_CONC_C_20220729.csv",
  "RANK(ALL)_BVOC_1HR_CONC_C_20220809.csv",
  "RANK(ALL)_BVOC_1HR_CONC_C_20220810.csv",
  "RANK(ALL)_BVOC_1HR_CONC_C_20220812.csv",
  "RANK(ALL)_BVOC_1HR_CONC_C_20220815.csv"
)

# Generate leaflet maps for all files
leaflet_maps <- lapply(filenames, process_file)

# Optionally, assign names to list
names(leaflet_maps) <- gsub("RANK\\(ALL\\)_BVOC_1HR_CONC_C_", "", tools::file_path_sans_ext(filenames))

# Example: view one map
leaflet_maps[["20220727"]]
