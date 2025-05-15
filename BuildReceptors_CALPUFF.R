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
mapview(all_receptors, color = "black", cex = 1, layer.name = "Receptors") +
  mapview(source_poly, col.regions = "red", alpha.regions = 0.4, layer.name = "Source")



########################################################################################
# SAVING:
########################################################################################

# Convert to data.frame with coordinates
receptors_df <- all_receptors %>%
  st_coordinates() %>%                   # Extract X and Y
  as.data.frame() %>%                    # Turn into a data.frame
  setNames(c("X", "Y"))                  # Name the columns

# If you have additional attributes, bind them
if (ncol(all_receptors) > 0) {
  receptors_df <- bind_cols(receptors_df, st_drop_geometry(all_receptors))
}

# Save to CSV
write.csv(receptors_df, "all_receptors.csv", row.names = FALSE)


# CAPUFF READY-FORMAT:

# Assuming 'all_receptors' is your sf object in UTM (easting/northing in meters)
# Convert to data.frame with X and Y in kilometers
coords_km <- st_coordinates(all_receptors) / 1000  # convert meters to kilometers
receptors_df <- as.data.frame(coords_km)
colnames(receptors_df) <- c("X", "Y")

# Add elevation and height above ground (set to dummy values for now)
receptors_df$elevation <- round(runif(nrow(receptors_df), 1, 2), 1)  # dummy elevation (1 to 30 m)
receptors_df$height <- 0.0

# Create the header lines
header_lines <- c(
  "                                               a",
  "           NON-GRIDDED (DISCRETE) RECEPTOR DATA",
  "           ------------------------------------",
  "              c           X            Y          Ground        Height   b",
  "Receptor Group        Coordinate   Coordinate    Elevation   Above Ground",
  "  No.    Name            (km)         (km)          (m)           (m)",
  "-------- -----        ----------   ----------    ---------   ------------"
)

# Function to calculate leading spaces based on receptor number
get_leading_spaces <- function(index) {
  n_digits <- nchar(as.character(index))
  return(strrep(" ", max(5 - n_digits, 1)))
}

# Create formatted receptor lines with dynamic spacing
formatted_lines <- sapply(1:nrow(receptors_df), function(i) {
  lead_space <- get_leading_spaces(i)
  sprintf("%s%d ! X = %12.5f, %12.5f, %10.1f, %12.1f !   !END!",
          lead_space,
          i,
          receptors_df$X[i],
          receptors_df$Y[i],
          receptors_df$elevation[i],
          receptors_df$height[i])
})

# Combine with header
full_output <- c(header_lines, formatted_lines)

# Save to .txt file
writeLines(full_output, "all_receptors_formatted.txt")