## ________________________________________________________________________

## Title:
## Purpose:
## Author:
## Date:

## Libraries
library(DBI)
library(RPostgres)
library(glue)
library(lubridate)
library(sf)
library(viridis)
library(rgdal)
library(raster)
library(rgeos)
library(sp)
library(tidyverse)

conflicted::conflict_prefer("select", "dplyr", "raster")
conflicted::conflict_prefer("filter", "dplyr", "stats")
conflicted::conflict_scout()

## ________________________________________________________________________

print(glue("PROCESSING: {sppselect}"))

walk(.x = c("helper functions/check_synonyms.R",
            "helper functions/check_bat_spp.R",
            "helper functions/check_subspp.R"),
     .f = source)

(is_bat <- check_bat_spp(sppselect))
(is_subspp <- check_subspp(sppselect))

# Create species directory ------------------------------------------------
if (dir.exists(glue("data output/sdm data processing/{sppselect}"))) {
  print("Folder exists")
} else {
  print("Folder created")
  dir.create(glue("data output/sdm data processing/{sppselect}"))
}

# Connect to EWT RL database & extract spp data ---------------------------

## Deal with synonyms
spp_qry <- check_synonyms(sppselect)

## Create a connection to the database
con <- DBI::dbConnect(RPostgres::Postgres(),
  dbname = "mammalredlist",
  host = "ewtcsusrv01.postgres.database.azure.com",
  port = 5432,
  user = "mrl_viewer@ewtcsusrv01",
  password = "UvwxyZ579"
)

## Create a table from data source
mammal_db <- tbl(con, "rldata")

## Species: Perform query and check header (doesn't pull data)
if(is_subspp){

  query <- mammal_db %>%
    select(genus, specificEpithet, infraspecificEpithet, everything()) %>%
    filter(str_c(genus, specificEpithet, infraspecificEpithet, sep = " ") %in% spp_qry)

} else if (!is_subspp){

  query <- mammal_db %>%
    select(scientificName, everything()) %>%
    filter(scientificName %in% spp_qry)

}

## Pull data from query
occ_data_all <- query %>%
  collect()

## Write raw data to file
occ_data_all <- occ_data_all %>%
  mutate(scientificName = sppselect) %>% # Replace any synonyms
  write_csv(glue("data output/sdm data processing/{sppselect}/occurrence_raw_{sppselect}.csv"))

# Screen occurrence data --------------------------------------------------

## List of filter files based on KBA screening
filelist <- dir("data input/record filter files/", full.names = TRUE)

source("helper functions/select_records.R")

keeplist <- map2(
  filelist,
  "Keep",
  select_records
)

excludelist <- map2(
  filelist,
  "Exclude",
  select_records
)

names(excludelist) <- names(keeplist) <- map_chr(
  filelist,
  function(x) {
    return(names(readxl::read_xlsx(x))[1])
  }
)

## Create filter variables
occ_data_all <- occ_data_all %>%
  mutate(
    expert_include = case_when(
      Expert_comments %in% keeplist$Expert_comments ~ "yes",
      Expert_comments %in% excludelist$Expert_comments ~ "no"
    ),
    geo_include = case_when(
      georeferenceRemarks %in% keeplist$georeferenceRemarks ~ "yes",
      georeferenceRemarks %in% excludelist$georeferenceRemarks ~ "no"
    ),
    identification_include = case_when(
      identificationRemarks %in% keeplist$identificationRemarks ~ "yes",
      identificationRemarks %in% excludelist$identificationRemarks ~ "no"
    ),
    loc_include = case_when(
      locationRemarks %in% keeplist$locationRemarks ~ "yes",
      locationRemarks %in% excludelist$locationRemarks ~ "no"
    ),
    rl_manager_include = case_when(
      RL_DataManagers_Comments %in% keeplist$RL_DataManagers_Comments ~ "yes",
      RL_DataManagers_Comments %in% excludelist$RL_DataManagers_Comments ~ "no"
    ),
    occstatus_include = case_when(
      occurrenceStatus %in% keeplist$occurrenceStatus ~ "yes",
      occurrenceStatus %in% excludelist$occurrenceStatus ~ "no"
    ),
    georef_include = case_when(
      georeferenceVerificationStatus %in% keeplist$georeferenceVerificationStatus ~ "yes",
      georeferenceVerificationStatus %in% excludelist$georeferenceVerificationStatus ~ "no"
    )
  )

## Remove excludelist observations, records that are not georeferenced & QDS records
occ_data <- occ_data_all %>%
  filter(expert_include == "yes" &
    geo_include == "yes" &
    identification_include == "yes" &
    loc_include == "yes" &
    rl_manager_include == "yes" &
    occstatus_include == "yes" &
    georef_include == "yes") %>%
  mutate_at(vars(decimalLongitude, decimalLatitude), as.numeric) %>%
  filter(!is.na(decimalLatitude) | !is.na(decimalLongitude)) %>%
  filter(!Coords_source %in% "QDS")

## Remove erroneous adhoc records (based on visual map plotting)
adhoc_rm <- read_csv("data input/adhoc_records_rm.csv") %>%
  pull(value)

occ_data <- occ_data %>%
  filter(!occurrenceID %in% adhoc_rm)

## Remove species which had a individualCount == 0
## This happens when a species is on a species list but hasn't actually been seen
ref <- which(occ_data$individualCount == 0)

if(length(ref) > 0){
  occ_data <- occ_data[-ref, ]
}

## Clean column names
occ_data <- occ_data %>%
  janitor::clean_names()

## Write filtered occurrence data
occ_data %>%
  write_csv(glue("data output/sdm data processing/{sppselect}/occurrence_clean_{sppselect}.csv"))

## Check
glimpse(occ_data)

# Filter spatial records --------------------------------------------------

## Import RSA shapefile
latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
za <- st_read("data input/RSA_fixed.shp", crs = latlongCRS)
za_buff_uni <- st_union(st_buffer(za, 0.01)) %>%
  st_make_valid()

## Filter columns
occ_data <- occ_data %>%
  dplyr::select(
    occurrence_id, order, genus, scientific_name, decimal_latitude, decimal_longitude,
    year, month, day, basis_of_record, tali_id, country, state_province,
    georeference_verification_status, order:identification_remarks,
    data_platform, recorded_by
  ) %>%
  print(n = 10)

## Remove clear outliers & convert occ_data to simple feature
occ_data_sf <- occ_data %>%
  filter(!(is.na(decimal_latitude) | is.na(decimal_longitude) |
    decimal_latitude == 0 | decimal_longitude == 0 |
    decimal_latitude > -15 | decimal_latitude < -35 |
    decimal_longitude < 15 | decimal_longitude > 35)) %>%
  st_as_sf(coords = c("decimal_longitude", "decimal_latitude"), crs = latlongCRS) %>%
  st_crop(st_bbox(za))

## Intersect with RSA feature
occ_data_sf <- occ_data_sf %>%
  st_intersection(za_buff_uni)

## Identify records with location errors
occ_data_locerr <- occ_data %>%
  filter(is.na(decimal_latitude) | is.na(decimal_longitude) |
    decimal_latitude == 0 | decimal_longitude == 0 |
    decimal_latitude > -21 | decimal_latitude < -35 |
    decimal_longitude < 15 | decimal_longitude > 35) %>%
  print(n = 10) %>%
  write_csv(glue("data output/sdm data processing/{sppselect}/spatial_errors_{sppselect}.csv"))

# Identify spatial outliers based on QDS or IUCN range map -------------------
source("helper functions/check_occ_QDS_IUCN.R")

if (is_bat) {

  iucn_shp <- glue("data input/IUCN range - bats/{sppselect}/{sppselect}.shp")

  iucn <- st_read(iucn_shp, crs = latlongCRS) %>%
    st_make_valid(.) %>%
    st_intersection(za_buff_uni)

  st_write(iucn, glue("data output/sdm data processing/{sppselect}/IUCN_{sppselect}.shp"),
    delete_dsn = TRUE, delete_layer = TRUE
  )

  qds_iucn_results <- check_occ_QDS_IUCN(iucn, occ_data_sf)

} else if (!is_bat) {

  qds_dir <- glue("data input/QDS files/{sppselect}")
  qds_shps <- list.files(qds_dir, pattern = "QDS", full.names = TRUE)
  qds_shps <- qds_shps[str_detect(qds_shps, ".shp$")]

  qds <- map(
    .x = qds_shps,
    .f = function(x) st_read(x, crs = latlongCRS) %>% st_make_valid(.)
  ) %>%
    bind_rows() %>%
    distinct(geometry, .keep_all = TRUE) %>%
    st_make_valid()

  st_write(qds, glue("data output/sdm data processing/{sppselect}/QDS_{sppselect}.shp"),
    delete_dsn = TRUE, delete_layer = TRUE
  )

  qds_iucn_results <- check_occ_QDS_IUCN(qds, occ_data_sf)
}

qds_df <- qds_iucn_results %>%
  st_drop_geometry()

occ_data <- occ_data %>%
  left_join(qds_df %>% dplyr::select(occurrence_id, core:buff3), by = "occurrence_id")

occ_data_sf <- occ_data_sf %>%
  left_join(qds_df %>% dplyr::select(occurrence_id, core:buff3), by = "occurrence_id")

## Identify points outside all three buffers
buff_rm <- occ_data_sf %>%
  filter(core == FALSE & buff1 == FALSE & buff2 == FALSE & buff3 == FALSE)

# Write outlying points to file -------------------------------------------
buff_rm %>%
  st_write(glue("data output/sdm data processing/{sppselect}/points_outside_buffers_{sppselect}.shp"),
           delete_dsn = TRUE)

buff_rm %>%
  st_drop_geometry() %>%
  st_write(glue("data output/sdm data processing/{sppselect}/points_outside_buffers_{sppselect}.csv"),
           delete_dsn = TRUE)

# Remove outliers ---------------------------------------------------------

## NOT CURRENTLY IMPLEMENTED - NEED TO CHECK ON A CASE BY CASE BASIS THEN COME BACK ##
# occ_data_sf <- occ_data_sf %>%
#   filter(!occurrence_id %in% buff_rm$occurrence_id)

# Write to shapefile ------------------------------------------------------
st_write(occ_data_sf, glue("data output/sdm data processing/{sppselect}/occ_records_full_{sppselect}.shp"),
         delete_dsn = TRUE, delete_layer = TRUE)

# Write workspace --------------------------------------------------------
save(
  list = c("occ_data", "occ_data_sf", "sppselect"),
  file = glue("data output/sdm data processing/{sppselect}/occ_data_clean.RData")
)

