## ________________________________________________________________________

## Title:
## Purpose:
## Author:
## Date:

## Libraries
library(lubridate)
library(gridExtra)
library(ggpubr)
library(sf)
library(viridis)
library(readxl)
library(raster)
library(tidyverse)
library(glue)

conflicted::conflict_prefer("filter", "dplyr", "stats")
conflicted::conflict_prefer("select", "dplyr", "raster")
conflicted::conflict_scout()

## ________________________________________________________________________

start <- Sys.time()

# Projections -------------------------------------------------------------
aeaproj <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


# Select species ----------------------------------------------------------
x <- glue("data input/Queries/SDM_query_{sppselect}.xlsx")

# Import query details ----------------------------------------------------
query <- read_xlsx(x) %>%
  print()

# Import occurrence data --------------------------------------------------

sdm_dir <- "data output/sdm data processing"
load(glue("{sdm_dir}/{sppselect}/occ_data_clean.RData"))

## Run code to make manual adjustments
# source("helper functions/spp_data_clean.R")

# Spatial projections  ----------------------------------------------------
geo_proj <- query$Value[which(query$Input == "Geographic projection")]


if (geo_proj == "Yes") {
  st_crs(occ_data_sf) <- latlongCRS
  occ_points <- occ_data_sf %>%
    st_as_sf(coords = c("decimallongitude", "decimallatitude"), crs = latlongCRS) %>%
    st_transform(aeaproj)
} else {
  occ_points <- occ_data_sf %>%
    st_as_sf(coords = c("decimallongitude", "decimallatitude"), crs = latlongCRS)
}

# Remove duplicate coordinates --------------------------------------------
occ_points <- occ_points %>%
  distinct(geometry, .keep_all = TRUE)

# Create background points ------------------------------------------------

## Extract buffer value from query
buffval <- as.numeric(
  query$Value[which(query$Input == "Background buffer (km)")]
) * 1000

buff_latlong <- as.numeric(
  query$Value[which(query$Input == "Background buffer (km)")]
) / 100 # Divide km by 100 to convert to arc_degree

## Import RSA shapefile
if (geo_proj == "Yes") {
  za <- st_read("data input/RSA_fixed.shp", crs = latlongCRS) %>%
    st_transform(aeaproj)
} else {
  za <- st_read("data input/RSA_fixed.shp", crs = latlongCRS)
}

## Choose bounding geometry
geom_choice <- query$Value[which(query$Input == "PA geometery")]

## Create buffer around occ points, create single buffer/MCP polygon and intersect with SA border
if (geom_choice == "Buffer") {
  if (geo_proj == "Yes") {
    occ_buffer <- st_intersection(st_union(st_buffer(occ_points, buffval)), za)
  } else {
    occ_buffer <- st_intersection(st_union(st_buffer(occ_points, buff_latlong)), za)
  }
} else if (geom_choice == "MCP") {
  if (geo_proj == "Yes") {
    occ_buffer <- st_intersection(st_buffer(st_convex_hull(st_union(occ_points)), buffval), za)
  } else {
    occ_buffer <- st_intersection(st_buffer(st_convex_hull(st_union(occ_points)), buff_latlong), za)
  }
} else if (geom_choice == "RSA"){

  occ_buffer <- za

}


## Read in value of background points from query
n_points <- as.numeric(query$Value[which(query$Input == "Background points")])

## Randomly sample points within buffer (returns sfc object - use st_sf() to convert to sf)
bck_points <- st_sample(occ_buffer, n_points) %>%
  st_sf()

## Write temporary files used in Python processing
bck_points %>%
  st_transform(crs = latlongCRS) %>%
  st_write("data output/temp_bck_dir/bck_points.shp", delete_layer = TRUE)

occ_buffer %>%
  st_transform(crs = latlongCRS) %>%
  st_write("data output/temp_bck_dir/occ_buffer.shp", delete_layer = TRUE)

occ_points %>%
  st_transform(crs = latlongCRS) %>%
  st_write("data output/temp_bck_dir/occ_points.shp", delete_layer = TRUE)

## Run python background point processing
system('"C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python" src/00_bck_points_processing.py',
       intern = TRUE)

## Read in adjusted background points
bck_points <- st_read("data output/temp_bck_dir/bck_points_updated.shp") %>%
  st_transform(aeaproj)

## Read in RSA grid
if (geo_proj == "Yes") {
  zagrid_sf <- readRDS("data input/zagrid_aea_sf.rds")
} else {
  ## Add non-projected sf ZA grid
}

## Create a background point unique ID
bck_points <- mutate(bck_points, ID = str_c("B", 1:nrow(bck_points)))

## Assign a polygonID to each background point
bck_grid <- st_join(bck_points, zagrid_sf, join = st_intersects, left = FALSE)

## Group by polygons and keep first point (i.e. remove duplicate points)
bck_points <- bck_grid %>% #
  group_by(ID) %>%
  filter(row_number() == 1) %>%
  ungroup

# Occurrence and BP plot checks --------------------------------------------

## Create a bounding box for map inset
occ_bbox <- st_make_grid(occ_buffer, n = 1)

p1 <- ggplot() +
  geom_sf(data = za, fill = alpha("grey",0.5), size = 1.0)+
  geom_sf(data = occ_points, size = 1.5)+
  geom_sf(data = occ_buffer, fill = alpha("red",0.1))+
  coord_sf(xlim = c(st_bbox(occ_bbox)$xmin,st_bbox(occ_bbox)$xmax),
           ylim = c(st_bbox(occ_bbox)$ymin,st_bbox(occ_bbox)$ymax))+
  theme_bw()+
  theme(panel.border = element_rect(colour = "dodgerblue", fill=NA, size=1.6))+
  theme(legend.text = element_text(size = 14))+
  labs(title = glue("{sppselect}"),
       subtitle = glue("{buffval/1000}km buffer
                       {nrow(occ_points)} occurrence points
                       {nrow(bck_points)} background points"))

ggsave(glue("{sdm_dir}/{sppselect}/occ_buffer_{sppselect}.jpg"),
       plot = p1, width = 12, height = 9)

# Environmental data ------------------------------------------------------
env_layer_list <- read_xlsx(x, sheet = "env_var_list") %>%
  filter(.data[[glue("select - {sppselect}")]] == 1) %>% # .data[[]] is tidy eval for a string input
  dplyr::select(folder,layer) %>%
  mutate(path = glue("C:/Users/DominicH/Documents/GIS data/Environmental data 30s reduced/{folder}/{layer}")) %>%
  dplyr::select(path) %>%
  pull

env_layer_list
envstack <- readAll(stack(env_layer_list))

# Project rasters ---------------------------------------------------------

## Project the rasters to AEA and crop (20% buffer added) to occurrence buffer
projenv_aea <- function(x){
  projection(x) <- latlongCRS
  projectRaster(x, crs=aeaproj)
}

projenv_latlong <- function(x){
  projection(x) <- latlongCRS
  projectRaster(x, crs=latlongCRS)
}

if (geo_proj == "Yes") {
  envstack <- stack(envstack)
  envstack <- stack(map(envstack@layers,projenv_aea))
  envstack <- readAll(stack(crop(envstack,extent(bck_points)*1.10))) # Add a 10% buffer
} else {
  envstack <- stack(envstack)
  envstack <- stack(map(envstack@layers,projenv_latlong))
  envstack <- readAll(stack(crop(envstack,extent(bck_points)*1.10)))
}


# Collinearity of environemental predictors -------------------------------
cormatrix <- raster::layerStats(envstack, "pearson", na.rm = TRUE)

cormatrix <- as_tibble(cormatrix$`pearson correlation coefficient`) %>%
  mutate(var = colnames(cormatrix$`pearson correlation coefficient`)) %>%
  select(var, everything()) %>%
  write_csv(glue("{sdm_dir}/{sppselect}/raster_corr_matrix.csv"))

var_reduced <- usdm::vifcor(envstack, th=0.6) # alternative: th=10
var_reduced@results$Variables

## Exclude the collinear variables that were identified in the previous step
envstack <- usdm::exclude(envstack,var_reduced)

usdm::vif(envstack) %>%
  as_tibble %>%
  write_csv(glue("{sdm_dir}/{sppselect}/vif_check.csv")) %>%
  print(n = 20)

# Raster predictor plots --------------------------------------------------
enlen <- 1:nlayers(envstack)
plists <- split(enlen, ceiling(seq_along(enlen)/4))
plotras <- function(x){plot(envstack[[x]])}

pdf(glue("{sdm_dir}/{sppselect}/env_rasters.pdf"), width = 16, height = 9)
map(plists, plotras)
dev.off()

# Write query file --------------------------------------------------------
copyfrom <- x
copyto <- glue("{sdm_dir}/{sppselect}/")
file.copy(copyfrom, copyto, recursive=TRUE)

# Write occurence points to shapefile -------------------------------------
occ_points %>%
  st_write(glue("{sdm_dir}/{sppselect}/occ_points_{sppselect}.shp"),
           delete_dsn=TRUE)

# Write workspace ---------------------------------------------------------
save(list = c("occ_points","bck_points","envstack"),
     file = glue("{sdm_dir}/{sppselect}/sdm_input_data.RData"))

# Timing ------------------------------------------------------------------
end <- Sys.time()
print(glue("SUCCESSFULLY COMPLETED - RUNTIME: {round(end-start,2)} {attr(end-start, 'units')}"))



