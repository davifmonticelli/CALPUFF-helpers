#-------------------------------------------------------------------------------------------
# CREATING GENERIC DATAFRAMES AND MAPPING LAND USE CODES TO CALMET'S
#-------------------------------------------------------------------------------------------

library(sf)
library(leaflet)
library(mapview)
library(dplyr)

CALMET_landcodes <- data.frame(
  `CALMET Description` = c(
    # Level I
    "Urban or Built-up Land",
    "Agricultural Land — Unirrigated",
    "Agricultural Land — Irrigated",
    "Rangeland",
    "Forest Land",
    "Water",
    "Wetland",
    "Barren Land",
    "Tundra",
    "Perennial Snow or Ice",
    
    # Level II
    "Residential",
    "Commercial and Services",
    "Industrial",
    "Transportation, Communications and Utilities",
    "Industrial and Commercial Complexes",
    "Mixed Urban or Built-up Land",
    "Other Urban or Built-up Land",
    
    "Cropland and Pasture",
    "Orchards, Groves, Vineyards, Nurseries, and Ornamental Horticultural Areas",
    "Confined Feeding Operations",
    "Other Agricultural Land",
    
    "Cropland and Pasture",
    "Orchards, Groves, Vineyards, Nurseries, and Ornamental Horticultural Areas",
    "Confined Feeding Operations",
    "Other Agricultural Land",
    
    "Herbaceous Rangeland",
    "Shrub and Brush Rangeland",
    "Mixed Rangeland",
    
    "Deciduous Forest Land",
    "Evergreen Forest Land",
    "Mixed Forest Land",
    
    "Streams and Canals",
    "Lakes",
    "Reservoirs",
    "Bays and Estuaries",
    "Oceans and Seas",
    
    "Forested Wetland",
    "Nonforested Wetland",
    
    "Dry Salt Flats",
    "Beaches",
    "Sandy Areas Other than Beaches",
    "Bare Exposed Rock",
    "Strip Mines, Quarries, and Gravel Pits",
    "Transitional Areas",
    "Mixed Barren Land",
    
    "Shrub and Brush Tundra",
    "Herbaceous Tundra",
    "Bare Ground",
    "Wet Tundra",
    "Mixed Tundra",
    
    "Perennial Snowfields",
    "Glaciers"
  ),
  Level = c(
    # Level flags
    rep("Level I", 10),
    rep("Level II", 42)
  ),
  Code = c(
    # Level I
    10, 20, -20, 30, 40, 50, 60, 70, 80, 90,
    # Level II
    11, 12, 13, 14, 15, 16, 17,
    21, 22, 23, 24,
    -21, -22, -23, -24,
    31, 32, 33,
    41, 42, 43,
    51, 52, 53, 54, 55,
    61, 62,
    71, 72, 73, 74, 75, 76, 77,
    81, 82, 83, 84, 85,
    91, 92
  )
)

colnames(CALMET_landcodes) <- c("CALMET_Description", "Level", "Code")

# MetroVan_landcodes <- data.frame(
#   Code = 1:16,
#   Classification = c(
#     "Buildings",
#     "Paved",
#     "Other Built",
#     "Barren",
#     "Soil",
#     "Coniferous",
#     "Deciduous",
#     "Shrub",
#     "Modified Herb",
#     "Natural Herb",
#     "Non-Photosynthetic Vegetation",
#     "Water",
#     "Shadow",
#     "Snow/Ice",
#     "Conifer/Paved",
#     "Deciduous/Paved"
#   ),
#   Criteria = c(
#     "Housing, warehouses, towers, industrial structures, etc.",
#     "Asphalt and concrete surfaces",
#     "Sports surfaces, transit or rail areas, other impervious surfaces, etc.",
#     "Beaches, alpine rock, shoreline rock, quarries, gravel pits, gravel roads, lacking vegetation, but not soil",
#     "Agricultural soils (light or dark), cleared/open areas where darker colours indicate organic matter present",
#     "Predominantly coniferous (>75%)",
#     "Predominantly broadleaf (>75%)",
#     "Woody, leafy, and rough-textured vegetation (~ <3-4m)",
#     "Most crops, golf course greens, city park grass, lawns, etc.",
#     "Alpine meadows, near-shore grass areas, fine-textured bog/wetland areas",
#     "Dead grass, cutblock slash, and log booms.",
#     "Lakes, rivers, inlets, irrigation channels, retention ponds, pools, etc.",
#     "Dark pixels with very low reflectance values",
#     "Snow or Ice features with high reflectance",
#     "Asphalt and concrete surfaces covered by coniferous",
#     "Asphalt and concrete surfaces covered by deciduous"
#   ),
#   stringsAsFactors = FALSE
# )
# 
# 
# 
# MetroVan_landcodes <- MetroVan_landcodes %>%
#   mutate(
#     CALMET_Code = case_when(
#       Code %in% c(1) ~ 16,  # Mixed Urban and Built-up Land (16)
#       Code %in% c(2) ~ 14,  # Transportation, Communication and Services (14)
#       Code %in% c(3) ~ 14,  # Transportation, Communication and Services (14)
#       Code %in% c(4) ~ 77,  # Mixed Barren Land (77)
#       Code %in% c(5) ~ 24,  # Other Agricultural Land (24)
#       Code %in% c(6) ~ 42,  # Evergreen Forest Land (42)
#       Code %in% c(7) ~ 41,  # Deciduous Forest Land (41)
#       Code %in% c(8) ~ 81,  # Shrub or Tundra (81) 
#       Code %in% c(9) ~ 31,  # Herbaceous Rangeland (31)
#       Code %in% c(10) ~ 31,  # Herbaceous Rangeland (31)
#       Code %in% c(11) ~ 33,  # Mixed Rangeland (33)
#       Code %in% c(12) ~ 54,  # Bays and Estuaries (54) --> freshwater, because need more to class 51-55
#       Code %in% c(13) ~ 17,  # Other Urban and Built-up Land (17)
#       Code %in% c(14) ~ 91,  # Perennial Snowfields (91)
#       Code %in% c(15) ~ 16,  # Mixed Urban and Built-up Land (16)
#       Code %in% c(16) ~ 16  # Mixed Urban and Built-up Land (16)
#     ),
#     CALMET_Description = case_when(
#       CALMET_Code == 16 ~ "Mixed Urban and Built-up Land",
#       CALMET_Code == 14 ~ "Transportation, Communication and Services",
#       CALMET_Code == 17 ~ "Other Urban and Built-up Land",
#       CALMET_Code == 77 ~ "Mixed Barren Land",
#       CALMET_Code == 24 ~ "Other Agricultural Land",
#       CALMET_Code == 42 ~ "Evergreen Forest Land",
#       CALMET_Code == 41 ~ "Deciduous Forest Land",
#       CALMET_Code == 81 ~ "Shrub or Tundra", 
#       CALMET_Code == 31 ~ "Herbaceous Rangeland",
#       CALMET_Code == 33 ~ "Mixed Rangeland",
#       CALMET_Code == 54 ~ "Bays and Estuaries",
#       CALMET_Code == 91 ~ "Perennial Snowfields"
#     )
#   )


LANDSAT_landcodes <- data.frame(
  Code = 1:19,
  Classification = c(
    "Temperate or sub-polar needleleaf forest",
    "Sub-polar taiga needleleaf forest",
    "Tropical or sub-tropical broadleaf evergreen forest",
    "Tropical or sub-tropical broadleaf deciduous forest",
    "Temperate or sub-polar broadleaf deciduous forest",
    "Mixed Forest",
    "Tropical or sub-tropical shrubland",
    "Temperate or sub-polar shrubland",
    "Tropical or sub-tropical grassland",
    "Temperate or sub-polar grassland",
    "Sub-polar or polar shrubland-lichen-moss",
    "Sub-polar or polar grassland-lichen-moss",
    "Sub-polar or polar barren-lichen-moss",
    "Wetland",
    "Cropland",
    "Barren lands",
    "Urban",
    "Water",
    "Snow and Ice"
  ),
  RGB = c(
    "0, 61, 0",
    "148, 156, 112",
    "0, 99, 0",
    "30, 171, 5",
    "20, 140, 61",
    "92, 117, 43",
    "179, 158, 43",
    "179, 138, 51",
    "232, 220, 94",
    "225, 207, 138",
    "156, 117, 84",
    "186, 212, 143",
    "64, 138, 112",
    "107, 163, 138",
    "230, 174, 102",
    "168, 171, 174",
    "220, 33, 38",
    "76, 112, 163",
    "255, 250, 255"
  ),
  stringsAsFactors = FALSE
)



LANDSAT_landcodes <- LANDSAT_landcodes %>%
  mutate(
    CALMET_Code = case_when(
      Code %in% c(1, 2) ~ 42,  # Evergreen Forest Land
      Code %in% c(3, 4) ~ 41,  # Deciduous Forest Land
      Code == 5 ~ 41,          # Deciduous Forest Land
      Code == 6 ~ 43,          # Mixed Forest Land
      Code %in% c(7, 8) ~ 32,  # Shrub and Brush Rangeland
      Code %in% c(9, 10) ~ 31, # Herbaceous Rangeland
      Code == 11 ~ 81,         # Shrub or Tundra
      Code == 12 ~ 31,         # Herbaceous Rangeland (some lichen/moss in tundra regions)
      Code == 13 ~ 77,         # Mixed Barren Land
      Code == 14 ~ 62,         # Nonforested Wetland
      Code == 15 ~ 21,         # Cropland and Pasture
      Code == 16 ~ 77,         # Mixed Barren Land
      Code == 17 ~ 16,         # Mixed Urban and Built-up Land
      Code == 18 ~ 51,         # Streams and Canals (generic water category)
      Code == 19 ~ 91          # Perennial Snowfields
    ),
    CALMET_Description = case_when(
      CALMET_Code == 41 ~ "Deciduous Forest Land",
      CALMET_Code == 42 ~ "Evergreen Forest Land",
      CALMET_Code == 43 ~ "Mixed Forest Land",
      CALMET_Code == 31 ~ "Herbaceous Rangeland",
      CALMET_Code == 32 ~ "Shrub and Brush Rangeland",
      CALMET_Code == 81 ~ "Shrub or Tundra",
      CALMET_Code == 62 ~ "Nonforested Wetland",
      CALMET_Code == 21 ~ "Cropland and Pasture",
      CALMET_Code == 77 ~ "Mixed Barren Land",
      CALMET_Code == 16 ~ "Mixed Urban and Built-up Land",
      CALMET_Code == 51 ~ "Streams and Canals",
      CALMET_Code == 91 ~ "Perennial Snowfields"
    )
  )

write.csv(LANDSAT_landcodes, "LANDSAT_CALMET_landcodes.csv", row.names = FALSE)

#------------------------------------------------------------------------------
# LOAD RASTER FILES
#------------------------------------------------------------------------------
library(terra)

# List all .tif files in a folder
landsat_tif_file <- list.files("./", pattern = "\\.tif$", full.names = TRUE)[1]
#metrovan_tif_file <- list.files("./", pattern = "\\.tif$", full.names = TRUE)[2]

# Read all tiles
landsat_tiles <- lapply(landsat_tif_file, rast)
#metrovan_tiles <- lapply(metrovan_tif_file, rast)

# Transform them into one raster
landsat_raster <- landsat_tiles[[1]]
#metrovan_raster <- metrovan_tiles[[1]]

# if (crs(metrovan_raster) == crs(landsat_raster)){
#   print("CRS are equal")
# }else{
#   crs(metrovan_raster) <- crs(landsat_raster)
# }

# Define your target CRS (UTM Zone 10N, WGS84)
utm_crs <- "EPSG:4326"
landsat_wgs84 <- terra::project(landsat_raster, utm_crs)

# Plot it
#plot(metrovan_raster)
#plot(landsat_raster)

# Check levels
#levels(metrovan_raster)
levels(landsat_raster)

# Make sure Code is integer (to match raster values)
landcover_classes <- LANDSAT_landcodes
landcover_classes$Code <- as.integer(landcover_classes$Code)

# Helper: convert "R, G, B" -> "#RRGGBB"
rgb_to_hex <- function(rgb_str) {
  rgb_vals <- as.numeric(unlist(strsplit(rgb_str, ",")))
  rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], maxColorValue = 255)
}

landcover_classes$color <- sapply(landcover_classes$RGB, rgb_to_hex)

# Create a data.frame with raster value (ID) and label
levels_table <- data.frame(
  ID = landcover_classes$Code,
  class = landcover_classes$Classification
)

levels_table[nrow(levels_table) + 1,] = c(0, "Ocean")

levels_table$ID <- as.integer(levels_table$ID)

# Assign this table to your raster
r <- landsat_wgs84$Class
levels(r) <- levels_table

plot(r, col = landcover_classes$color, main = "Land Cover (2020)")


#-------------------------------------------------------------------------------------------
# SPECIFIC TO YOUR CALMET SET UP:
#-------------------------------------------------------------------------------------------

# Convert center coordinates (in km) to meters, and adjust to lower-left corner of the cell
x_min <- (477.647) * 1000  
y_min <- (5415.154) * 1000  

# Grid specs
n_cells <- 200  # 200 km × 200 km
res <- 250     # 0.25 km resolution
x_max <- x_min + n_cells * res
y_max <- y_min + n_cells * res

# Create raster template
r_template <- rast(ext(x_min, x_max, y_min, y_max), resolution = res, crs = "EPSG:26910")

# Convert to polygons and then to sf
grid_sf <- as.polygons(r_template) |> st_as_sf()

# Compute centroid coordinates
grid_sf <- grid_sf |>
  mutate(centroid = st_centroid(geometry),
         cx = st_coordinates(centroid)[,1],
         cy = st_coordinates(centroid)[,2])

# Compute row and column indices
grid_sf <- grid_sf |>
  mutate(
    col = ((cx - x_min) %/% res) + 1,
    row = ((cy - y_min) %/% res) + 1
  )

# Arrange in CTGPROC order (bottom to top rows, left to right columns)
grid_sf <- grid_sf |>
  arrange(row, col) |>
  mutate(CTGPROC_ID = row_number()) |>
  select(-centroid, -cx, -cy)

# Reorder columns for clarity
grid_sf <- grid_sf |> select(CTGPROC_ID, row, col, everything())


# Reproject grid to WGS84 for leaflet
grid_wgs84 <- st_transform(grid_sf, utm_crs)

r_wgs84 <- terra::project(r, utm_crs)

library(tmap)

tm_shape(r_wgs84) +
     tm_raster() +
     tm_shape(grid_wgs84) +
     tm_borders(col = "red")

#>>>>>>>>>>>>>>>>>>>>>>> next line takes a lot of time <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


# Extract with exact area overlap
extracted <- terra::extract(r_wgs84, vect(grid_wgs84), exact = TRUE, coverage = TRUE)

# extracted <- extracted %>%
#   mutate(class = if_else(is.na(class), "Ocean", class))

# Add the grid cell ID
#extracted$cell_id <- extracted$ID  # 'ID' is assigned by terra::extract()

# Summarize: total fraction per class in each grid cell
percent_cover <- extracted %>%
  group_by(ID, class = class) %>%
  summarise(fraction = sum(fraction), .groups = "drop")

percent_cover <- percent_cover %>%
  mutate(percent = fraction/max(fraction))


library(tidyr)
percent_cover_wide <- percent_cover %>%
  select(ID, class, percent) %>%
  pivot_wider(names_from = class, values_from = percent, values_fill = 0)

write.csv(percent_cover_wide, "percent_cover_wide.csv", row.names = FALSE)

# Copy percent_cover_wide to preserve original
df <- percent_cover_wide

# Extract existing names (excluding "ID")
old_names <- colnames(df)[-1]

# Create a lookup table from your classification dataframe
name_lookup <- setNames(landcover_classes$CALMET_Code, landcover_classes$Classification)

# Add "Ocean" manually as 55
name_lookup["Ocean"] <- 55

# Replace column names
new_names <- sapply(old_names, function(x) name_lookup[[x]])

# Set new column names
colnames(df) <- c("ID", new_names)

# Transpose to group by column name
df_collapsed <- as.data.frame(t(rowsum(t(df[,-1]), group = colnames(df)[-1])))

# Add ID column back
df_collapsed <- cbind(ID = df$ID, df_collapsed)

# Optional: sort columns by CALMET code
df_collapsed <- df_collapsed[, c("ID", sort(as.numeric(colnames(df_collapsed)[-1])) |> as.character())]

# Vector of all required CALMET codes as character
all_codes <- as.character(c(
  11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24,
  31, 32, 33, 41, 42, 43, 51, 52, 53, 54, 55,
  61, 62, 71, 72, 73, 74, 75, 76, 77, 81, 82,
  83, 84, 85, 91, 92
))

# Find codes missing from the current dataframe
missing_codes <- setdiff(all_codes, colnames(df_collapsed))

# Add missing columns with 0
for (code in missing_codes) {
  df_collapsed[[code]] <- 0
}

# Reorder columns: ID first, then sorted CALMET codes
df_collapsed <- df_collapsed[, c("ID", sort(as.numeric(all_codes)) |> as.character())]

# Round
df_collapsed[setdiff(names(df_collapsed), "ID")] <- 
  round(df_collapsed[setdiff(names(df_collapsed), "ID")], 3)

#------------------------------------------------------------------------------
# CALPUFF VIEW EXPORTED FILE (.SHP) from DEFAULT LAND USE AND MET GRID
#------------------------------------------------------------------------------

landuse_GLCC <- st_read("landuse_glcc.shp")

# Multiply all coordinates by 1000
st_geometry(landuse_GLCC) <- st_geometry(landuse_GLCC) * 1000

landuse_GLCC$LEGEND.1 <- NULL
colnames(landuse_GLCC) <- c("RWID", "RWE_INDEX", "CALMET_Code", "geometry")

st_crs(landuse_GLCC) <- crs(grid_sf)

# Get nearest features
nearest_idx <- st_nearest_feature(grid_sf, landuse_GLCC)

# Add RWID and RWE_INDEX to grid_sf
grid_sf$ID <- nearest_idx

#------------------------------------------------------------------------------

# CTGPROC writes first the columns of a a single row (e.g., 1, 1 to 200) followed by the next row
# (e.g., 2, 1 to 200). The rows are numbered from SW corner up
# The final file looks like:

# LU.DAT          2.1             Coordinate parameters and LandUse categories                    
# 2
# Produced by CTGPROC Version: 7.0.0  Level: 150211                                     
# Internal Coordinate Transformations  ---  COORDLIB   Version: 1.99   Level: 070921    
# FRACTION
# UTM     
# 10N   
# WGS-84  02-21-2003  
# 200     200     477.647    5415.154       0.250       0.250    38
# KM  
#           11    12    13    14    15    16    17    21    22    23    24    31    32    33    41    42    43    51    52    53    54    55    61    62    71    72    73    74    75    76    77    81    82    83    84    85    91    92
# 1    1 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 1.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000   .                
# 2    1 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 1.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000   .                
# 3    1 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 1.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000   .                
# 4    1 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 1.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000   .                


# FINAL TOUCHES
df_collapsed$row <- grid_wgs84$row
df_collapsed$col <- grid_wgs84$col

# Move RWID and RWE_INDEX to the first two columns
df_collapsed <- df_collapsed[, c("col", "row", setdiff(names(df_collapsed), c("col", "row")))]

df_collapsed$ID <- NULL

# MAKE CTGPROC.DAT!!!
# NOTE: Some spacing may still need adjustment after. Compare with default file

# 1. Prepare the file connection
output_file <- "LU_formatted.txt"
con <- file(output_file, open = "wt")

# 2. Write LU.DAT-style header
writeLines(c(
  "LU.DAT          2.1             Coordinate parameters and LandUse categories                    ",
  "   2",
  "Produced by CTGPROC Version: 7.0.0  Level: 150211                                     ",
  "Internal Coordinate Transformations  ---  COORDLIB   Version: 1.99   Level: 070921    ",
  "FRACTION",
  "UTM     ",
  "  10N   ",
  "WGS-84  02-21-2003  ",
  "     200     200     477.647    5415.154       0.250       0.250    38",
  "KM  "
), con)

# 3. Write land use column headers (14 spaces, then 6-wide columns)
header_spacing <- strrep(" ", 14)
formatted_headers <- sprintf("%6s", colnames(df_collapsed)[-(1:2)])
writeLines(paste0(header_spacing, paste(formatted_headers, collapse = "")), con)

# 4. Write each data row
for (i in 1:nrow(df_collapsed)) {
  row_vals <- df_collapsed[i, ]
  numeric_vals <- as.numeric(row_vals[-c(1, 2)])
  formatted_vals <- sprintf("%6.3f", numeric_vals)
  line <- paste0(
    sprintf("%5d  %4d", row_vals$col, row_vals$row),  # col and row with 2 spaces between
    paste(formatted_vals, collapse = ""),
    "   ."  # three spaces before period
  )
  writeLines(line, con)
}

# 5. Close file
close(con)
