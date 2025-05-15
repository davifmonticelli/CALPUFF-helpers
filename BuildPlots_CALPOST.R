# Load required packages
library(sf)
library(mapview)
library(dplyr)

# --- STEP 1: Define your 4 UTM points (Easting, Northing) and UTM Zone ---
# Example: UTM Zone 10N (EPSG:32610). Replace these with your actual coordinates.
utm_coords <- matrix(c(
  499276.97, 5436660.59,
  499276.97, 5437482.54,
  500682.24, 5437482.54,
  500682.24, 5436660.59,
  499276.97, 5436660.59  # close the polygon
), ncol = 2, byrow = TRUE)

# --- STEP 2: Generate points along the boundary at 20m intervals ---
# Create polygon in UTM (e.g., EPSG:32610 = UTM Zone 10N)
source_poly <- st_polygon(list(utm_coords)) %>%
  st_sfc(crs = 32610) %>%
  st_sf()

source_boundary <- st_cast(source_poly, "LINESTRING")
boundary_points <- st_line_sample(source_boundary, density = 1 / 20) %>%
  st_cast("POINT") %>%
  st_sf()

# --- STEP 3: Create buffer zones for each receptor spacing range ---
buffer_500m <- st_buffer(source_poly, 500)
buffer_2km <- st_buffer(source_poly, 2000)
buffer_5km <- st_buffer(source_poly, 5000)
buffer_10km <- st_buffer(source_poly, 10000)

# --- STEP 4: Create grids for each zone ---
make_grid <- function(polygon, spacing) {
  st_make_grid(polygon, cellsize = spacing, square = TRUE) %>%
    st_centroid() %>%
    st_intersection(polygon) %>%
    st_sf()
}

# 50 m spacing within 500 m of the source
grid_50 <- make_grid(buffer_500m, 50)

# 250 m spacing within 2 km (excluding previous buffer)
# Create 250 m grid within 2 km
grid_250_full <- make_grid(buffer_2km, 250)

# Keep only those NOT inside the 500 m buffer
in_500m <- st_within(grid_250_full, buffer_500m, sparse = FALSE)[,1]
grid_250 <- grid_250_full[!in_500m, ]

# 500 m spacing within 5 km, excluding inside 2 km
grid_500_full <- make_grid(buffer_5km, 500)
in_2km <- st_within(grid_500_full, buffer_2km, sparse = FALSE)[,1]
grid_500 <- grid_500_full[!in_2km, ]

# 1000 m spacing beyond 5 km
grid_1000_full <- make_grid(buffer_10km, 1000)
in_5km <- st_within(grid_1000_full, buffer_5km, sparse = FALSE)[,1]
grid_1000 <- grid_1000_full[!in_5km, ]

# --- STEP 5: Combine all receptor points ---
all_receptors <- bind_rows(
  boundary_points,
  grid_50,
  grid_250,
  grid_500,
  grid_1000
)

# Remove duplicates
all_receptors <- all_receptors %>% distinct()

# --- STEP 6: Plot everything ---
Receptors_map <- mapview(all_receptors, color = "black", cex = 1, layer.name = "Receptors") +
  mapview(source_poly, col.regions = "red", alpha.regions = 0.4, layer.name = "Source")


#------------------------------------------------------------------------------------
# Load CALPOST results
#------------------------------------------------------------------------------------

library(readr)
RANK_ALL_BVOC_1HR_CONC_C <- read_csv("RANK(ALL)_BVOC_1HR_CONC_C.CSV", 
                                     skip = 4)
View(RANK_ALL_BVOC_1HR_CONC_C)

colnames(RANK_ALL_BVOC_1HR_CONC_C) = c("UTM X (km)", "UTM Y (km)", "BVOC (ug/m3)", "lat", "long")

df <- RANK_ALL_BVOC_1HR_CONC_C

library(akima)
library(raster)

interp_res <- with(df, akima::interp(x = long, y = lat, z = `BVOC (ug/m3)`, 
                                     duplicate = "mean", linear = TRUE,
                                     nx = 1000,  # number of grid points in x-direction (longitude)
                                     ny = 1000   # number of grid points in y-direction (latitude)
))
r <- raster(list(x = interp_res$x, y = interp_res$y, z = interp_res$z))  
crs(r) <- CRS("+proj=longlat +datum=WGS84")  # optional but good practice

CALPUFF_map <- mapview(r, legend = TRUE, layer.name = "BVOC (ug.m-3)", at = pretty(values(r), 10))

library(readr)
Cannabis_smells_2022 <- read_csv("Cannabis_smells_2022.csv")
#View(Cannabis_smells_2022_07_27)

AQ_2022_Smells <- Cannabis_smells_2022

#Reconfigure data to be GIS-based
Smells_2022 = st_as_sf(AQ_2022_Smells, coords = c("long", "lat"))
st_crs(Smells_2022) = 4326 #addresses a Datum/Coordinate System to the dataframe

Smells_2022s_Map = mapview(Smells_2022, map.types = "Esri.WorldGrayCanvas", lwd = 0, 
                           alpha.regions = 1, cex = 3, col.regions = "green",
                           legend = TRUE, layer.name = "Cannabis smells", homebutton = FALSE, fgb = FALSE)

# Define a set of latitude and longitude coordinates
urban_coords <- data.frame(
  lon = c(-123.103474, -123.103361, -123.066034, -123.065845, -123.044163, 
          -123.044186, -123.034273, -123.033962, -123.010695, -123.010411, 
          -123.043712, -123.043889, -123.103474), # Ensure the polygon closes
  lat = c(49.078921, 49.112209, 49.111960, 49.099863, 49.099551, 
          49.093807, 49.093668, 49.090957, 49.090932, 49.081214, 
          49.081649, 49.078802, 49.078921)
)

# Define a set of latitude and longitude coordinates
ccf_coords <- data.frame(
  lon = c(-123.010020, -122.991660, -122.991332, -123.009455, -123.010020), # Closing the polygon
  lat = c(49.089891, 49.089645, 49.081185, 49.081391, 49.089891)
)

# Define a set of latitude and longitude coordinates
rural_coords <- data.frame(
  lon = c(-122.990299, -122.991069, -122.979944, -122.980355, -122.927318, -122.927826, -122.990299), # Closing the polygon
  lat = c(49.081545, 49.090794, 49.091240, 49.105888, 49.105972, 49.081553, 49.081545)
)


# Load required libraries
library(ggspatial)
library(prettymapr)

# Function to convert coordinates to an sf polygon
create_polygon_sf <- function(coords, name) {
  polygon <- st_sfc(st_polygon(list(as.matrix(coords))), crs = 4326)
  st_sf(name = name, geometry = polygon)
}

# Create sf objects for each region
urban_sf <- create_polygon_sf(urban_coords, "Urban region")
ccf_sf <- create_polygon_sf(ccf_coords, "CCF Greenhouses area")
rural_sf <- create_polygon_sf(rural_coords, "Rural region")

# Combine all polygons into one dataframe
all_polygons <- bind_rows(urban_sf, ccf_sf, rural_sf)

Areas_map <- mapview(all_polygons, alpha.regions = 0, lwd = 2, color = "white", col.regions = NA,
                     layer.name = "Sampling areas", homebutton = FALSE, fgb = FALSE, legend = FALSE)


Final_map <- CALPUFF_map + Smells_2022s_Map + Areas_map
Final_map


#-------------------------------
# LEAFLET:

library(leaflet)

# Define color palette again if not already in session
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

# Create the map
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
  addPolygons(
    data = all_polygons,
    fillColor = "transparent",
    weight = 2,
    color = "black",
    group = "Sampling Areas"
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("black"),
    labels = c("Sampling areas (black outline)"),
    title = "Map Features",
    opacity = 1
  )