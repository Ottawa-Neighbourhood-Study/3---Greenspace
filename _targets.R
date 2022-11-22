# So the Postal code file ask is:
#
# to use the 2020 LDU file (attached here) to reproduce a postal
# code-to-neighbourhoods file similar to the csv_SSOT one I've attached here
# (using OSM residential like last time)
#
# then look for any postal codes from our old file that are not in our new file,
# and add them to our new file with the same weights as before

library(targets)
library(tarchetypes)

# tell targets which packages we need to load to do the analysis
tar_option_set(packages = c("tidyverse",
                            "osmdata",
                            "leaflet",
                            "sf"
))


source("R/functions.R")

list(
  #################################### -
  ## LOAD DATA ----
  tar_target(ons_shp_gen2, sf::st_make_valid(neighbourhoodstudy::ons_shp_gen2)),
  tar_target(ons_shp_gen3, sf::st_make_valid(neighbourhoodstudy::ons_shp_gen3) %>% dplyr::filter(ONS_Region == "OTTAWA")),
  tar_target(ldu_shp, sf::read_sf("shapefiles/_ags_DMTI_2020_CMPCS_LocalDeliveryUnitsRegion1/DMTI_2020_CMPCS_LocalDeliveryUnitsRegion.shp") %>% 
               dplyr::filter(PROV == "ON") %>%
               union_ldus()),
  tar_target(osm_res_shp, get_osm_residential_polygons(ons_shp_gen3)),
  
  #################################### -
  ## TRIM ONS NEIGHBOURHOODS TO OSM RESIDENTIAL REGIONS ----
  
  # we use st_collection_extract() to remove some errant linestrings that mess us up later
  tar_target(ons_shp_gen2_trim, sf::st_intersection(ons_shp_gen2, osm_res_shp)),
  tar_target(ons_shp_gen3_trim, sf::st_intersection(ons_shp_gen3, osm_res_shp) %>% 
               sf::st_collection_extract(type = "POLYGON") %>%
               sf::st_make_valid()),
  
  #####################
  ### Gen2 boundaries ----
  # get intersections of LDUs with inhabited areas
  tar_target(ldus_intersect_gen2, get_ldu_intersection(ldu_shp, ons_shp_gen2_trim, batch_size = 50)),
  
  # for any postalcodes without such an intersection, get their intersection with the regular area
  tar_target(ldus_intersect_gen2_extra, 
             ldu_shp %>% 
               filter(!POSTALCODE %in% ldus_intersect_gen2$POSTALCODE) %>%
             get_ldu_intersection(ons_shp_gen2, batch_size = 50)),
  
  # put them together
  tar_target(ldus_intersect_gen2_long, dplyr::bind_rows(ldus_intersect_gen2, ldus_intersect_gen2_extra) 
             %>% pivot_longer(cols = -POSTALCODE, names_to = "ONS_ID") %>%
               drop_na()),
  
  #####################
  ### Gen3 boundaries ----
  # get intersections of LDUs with inhabited areas
  tar_target(ldus_intersect_gen3, get_ldu_intersection(ldu_shp, ons_shp_gen3_trim, batch_size = 50)),
  
  # for any postalcodes without such an intersection, get their intersection with the regular area
  tar_target(ldus_intersect_gen3_extra, 
             ldu_shp %>% 
               filter(!POSTALCODE %in% ldus_intersect_gen3$POSTALCODE) %>%
               get_ldu_intersection(ons_shp_gen3, batch_size = 50)),
  
  # put them together
  tar_target(ldus_intersect_gen3_long, dplyr::bind_rows(ldus_intersect_gen3, ldus_intersect_gen3_extra) %>% pivot_longer(cols = -POSTALCODE, names_to = "ONS_ID") %>% drop_na()),
  
  #####################
  ## SAVE RESULTS ----
  
  tar_target(save_results, {
    readr::write_csv(ldus_intersect_gen2_long, sprintf("results/ldus_ons_gen2_weighted_%s.csv", Sys.Date()))
    readr::write_csv(ldus_intersect_gen3_long, sprintf("results/ldus_ons_gen3_weighted_%s.csv", Sys.Date()))
  }),
  
  # create images saved in results/images to visually confirm
  tar_target(create_check_images, {
    create_ldu_check_plots(ldu_shp, ons_shp_gen2, ldus_intersect_gen2_long, "Gen2")
    create_ldu_check_plots(ldu_shp, ons_shp_gen3, ldus_intersect_gen3_long, "Gen3")
  }),
  
  NULL
)