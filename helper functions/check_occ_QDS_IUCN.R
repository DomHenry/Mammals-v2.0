# Check whether points fall within QDS/IUCN polygons -------------------------
check_occ_QDS_IUCN <- function(x, occ) {
  
  # x <- st_union(x)
  # x <- sf::st_make_valid(x)
  x <- as(x, "Spatial")
  buff1 <- raster::buffer(x, width = 0.05)
  buff2 <- raster::buffer(x, width = 0.1)
  buff3 <- raster::buffer(x, width = 0.2)
  
  # rgeos::gIsValid(x, byid = FALSE, reason=TRUE)
  
  b1 <- buff1 - x
  b2 <- buff2 - buff1
  b3 <- buff3 - buff2

  if (nrow(occ) == 0) {
    occ <- NULL
  } else {
    occ <- occ %>%
      st_transform(st_crs(x))

    occ <- occ %>%
      mutate(core = st_intersects(occ, st_union(st_as_sf(x)), sparse = FALSE)) %>%
      mutate(buff1 = st_intersects(occ, st_as_sf(b1), sparse = FALSE)) %>%
      mutate(buff2 = st_intersects(occ, st_as_sf(b2), sparse = FALSE)) %>%
      mutate(buff3 = st_intersects(occ, st_as_sf(b3), sparse = FALSE))
  }

  return(occ)
}
