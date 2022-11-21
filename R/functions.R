# Download OpenStreetMaps residential zones for Ottawa, Ontario
get_osm_residential_polygons <- function(ons_shp, union = TRUE){
  
  # etxract original CRS. we need to use WGS84 for the osmdata call
  crs_orig <- sf::st_crs(ons_shp)
  
  # query openstreetmaps
  ottawa_osm <- osmdata::opq(bbox = sf::st_bbox(sf::st_transform(ons_shp, crs = "WGS84")),
                             nodes_only = FALSE,
                             timeout = 10000,
                             memsize = 1000000000) %>%
    osmdata::add_osm_feature(key = "landuse", value = "residential") %>%
    osmdata::osmdata_sf(quiet = FALSE)
  
  # need both polygons and multipolygons or we miss barrhaven
  result <- dplyr::bind_rows(ottawa_osm$osm_polygons, ottawa_osm$osm_multipolygons) %>%
    dplyr::select(osm_id) 
  
  # if we're unioning the individual polygons
  if (union){
    message("Running spatial union, this may take a minute...")
    result <- result %>% #ottawa_osm$osm_polygons %>%
      sf::st_union() %>%
      sf::st_transform(crs = crs_orig)
  }
  
  # put the crs back
  result <- sf::st_transform(result, crs = crs_orig)
  
  return(result)
}


# Some LDUs have more than one disconnected regin, and the LDU input file separates
# these into separate rows. Here we group by LDU then take the spatial union,
# creating one single (perhaps disconnected multipolygon) feature for each LDU.
union_ldus <- function(ldus) {
  ldus %>%
    select(POSTALCODE) %>%
    group_by(POSTALCODE) %>%
    #head(1000) %>%
    sf::st_make_valid() %>%
    tidyr::nest(geometry = geometry) %>%
    mutate(geometry = purrr::map(geometry, sf::st_union)) %>%
    unnest(geometry) %>%
    sf::st_as_sf(crs = "WGS84")
}



# get intersections between LDUs and the OSM-trimmed neighbourhoods in batches.
get_ldu_intersection <- function(ldus, ons_trim, batch_size = 1000){
  
  
  # get the list of postal codes
  pcodes <- ldus %>%
    dplyr::pull(POSTALCODE) %>%
    sort() %>%
    unique()
  
  
  
  # set batch size: how many postal codes will we process at once?
  #batch_size <- 1000
  #batch_size <- 1
  # how many batches will we need? it seems ok to give  a vector an index that's too big, it returns NA
  num_batches <- (length(pcodes) / batch_size ) %>% ceiling()
  
  batch_times <- NA
  #num_batches <- 1
  
  # here is our results tibble.
  results <- tibble()
  
  # eventually we will iterate over batches
  for (batch in 1:num_batches) {
    #batch <- 1
    secs_remaining <- (num_batches - batch) * mean(batch_times, na.rm= TRUE)
    message(batch,"/",num_batches, " : Est. ", (secs_remaining %/% 60) , ":", round(secs_remaining %% 60, digits = 0), " remaining" )
    # now we filer the big shape to get the postal codes we want
    # and make sure the ldus are valid
    ldus_batch <- ldus %>%
      dplyr::filter(POSTALCODE %in% pcodes[((batch-1)*batch_size + 1):(batch*batch_size)]) %>%
      sf::st_make_valid()
    
    if (batch_size == 1) message(batch,"/",num_batches, ": ", ldus_batch$POSTALCODE)
    
    tictoc::tic()
    ldus_intersect <- ldus_batch %>%
      #mutate(geometry = purrr::map(geometry, st_buffer, dist = 0)) %>%
      dplyr::mutate(results = purrr::map(geometry, function(x) {
        purrr::map(ons_trim$geometry, sf::st_intersection, y=x) %>%
          purrr::map_dbl(sf::st_area) %>%
          dplyr::tibble(ONS_ID = ons_trim$ONS_ID, Name = ons_trim$Name, intersection_area = .)
      })) %>%
      sf::st_set_geometry(NULL)
    
    batch_time <- tictoc::toc()
    batch_times <- c(batch_times, (batch_time$toc - batch_time$tic))
    
    results <- dplyr::bind_rows(results, ldus_intersect)
  } #end for
  
  
  # this is where we do the intersection calculations
  # for each postal code, we find the total area that intersects with any neighbourhood
  # then we find the percentage of that value that each intersection represents
  # so it is NOT % of total postal code area; it is % of postal code area that intersects any residential area.
  # so the 
  ldus_unnest <- results %>%
    tidyr::unnest(cols = results) %>%
    dplyr::group_by(POSTALCODE) %>%
    dplyr::mutate(total_intersection_area = sum(intersection_area),
           intersection_pct = intersection_area / total_intersection_area,
           intersection_pct = round(intersection_pct, digits=3 )) %>%
    dplyr::filter(intersection_pct > 0 ) %>%
    dplyr::select(POSTALCODE, ONS_ID, intersection_pct) %>%
    tidyr::pivot_wider(names_from = ONS_ID, values_from = intersection_pct) %>%
    dplyr::select(POSTALCODE, sort(current_vars()))
  
  return(ldus_unnest)
}
