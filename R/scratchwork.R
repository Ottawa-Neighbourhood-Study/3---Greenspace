
library(tidyverse)
library(sf)
library(leaflet)

# test for a few ldus
ldus <- head(ldu_shp %>% dplyr::filter(MUNICIPAL == "OTTAWA"), n = 5)

get_ldu_intersection(ldus, ons_shp_gen2_trim)

ons_shp_gen2 %>% dplyr::filter(ONS_ID == 943) %>% 
  ggplot() + geom_sf() + 
  geom_sf(data = dplyr::filter(ldu_shp, POSTALCODE %in% c("K1E1G9", "K1E3L7", "K1E3L8","K1E3L6","K1E3L9")))



#### multi-region ldus
multi_ldus <- ldus %>% sf::st_set_geometry(NULL) %>% group_by(POSTALCODE) %>% mutate(n = n()) %>% arrange(desc(n)) %>% distinct(POSTALCODE, n)

multi_ldus

test <- ldus %>%
  select(POSTALCODE) %>%
  group_by(POSTALCODE) %>%
  head(1000) %>%
  sf::st_make_valid() %>%
  tidyr::nest(geometry = geometry) %>%
  mutate(geometry = purrr::map(geometry, sf::st_union)) %>%
  unnest(geometry) %>%
  sf::st_as_sf(crs = "WGS84")
#sf::st_union(by_feature = TRUE, is_coverage = TRUE)


ggplot(test) + geom_sf()

ldu_shp %>% sf::st_make_valid() %>% filter(POSTALCODE == "K0G1J0")  %>% sf::st_union()




# Mu nster problems
# it intersects one part in a straight line which leads it to be a geometrycollection with a multilinestring instead of a multipolygon...


z <- ons_shp_gen3 %>% filter(ONS_ID == 3075) %>% sf::st_make_valid()


z %>% leaflet() %>% addTiles() %>% addPolygons()

z %>% leaflet() %>% addTiles() %>% addPolygons() %>% addPolygons(data = osm_res_shp)
sf::st_intersection(z, sf::st_as_sf(osm_res_shp))

sf::st_intersection(ons_shp_gen3, osm_res_shp) %>% mutate(type = sf::st_geometry_type(geometry), .before = 1) %>%
  filter(ONS_ID == 3075) %>% 
  ggplot() + geom_sf()



sf::st_intersection(ons_shp_gen3, osm_res_shp) %>% sf::st_collection_extract(type = "POLYGON") %>%
  leaflet() %>% addTiles() %>% addPolygons()




## error with postal code K2C3H1

z <- ldu_shp %>%
  dplyr::filter(POSTALCODE == "K2C3H1")

leaflet(z) %>% addTiles() %>% addPolygons()

sf::st_intersection(z, ons_shp_gen3_trim)

sf::st_is_valid(z)

all(sf::st_is_valid(ons_shp_gen3_trim))

z %>%
  get_ldu_intersection(ons_shp_gen3_trim)


ldu_shp %>%
  filter(!POSTALCODE %in% ldus_intersect_gen2_long$POSTALCODE) %>%
  ggplot() + geom_sf() + geom_sf(data = ons_shp_gen2)


get_ldu_intersection(ldu_shp, dplyr::filter(ons_shp_gen3_trim, ONS_ID == 3075), batch_size = 50)


####### TESTING

targets::tar_load(everything())

hood_id <- "3075"

ldu_hood <- dplyr::filter(ldus_intersect_gen3_long, ONS_ID == hood_id )
ldus <- ldu_hood$POSTALCODE

ldus_shp <- filter(ldu_shp, POSTALCODE %in% ldus) %>%
  left_join(ldu_hood)

hood_shp <- filter(ons_shp_gen3, ONS_ID == hood_id)

ggplot(hood_shp) + geom_sf(colour = "blue") + geom_sf(data = ldus_shp, mapping = aes(fill = value), colour = "green")


ldu_shp %>%
  filter(POSTALCODE %in% ldus_intersect_gen3_long$POSTALCODE) %>%
  #  head() %>%
  ggplot() + geom_sf(colour = NA, fill = "blue" ) + geom_sf(data = ons_shp_gen3, fill = NA)


ldu_shp %>%
  filter(POSTALCODE %in% ldus_intersect_gen3_long$POSTALCODE) %>%
  #  head() %>%
  ggplot() + geom_sf(colour = NA, fill = "blue" ) + geom_sf(data = ons_shp_gen3, fill = NA)

ldu_shp %>%
  filter(!POSTALCODE %in% ldus_intersect_gen3_long$POSTALCODE) %>%
  #  head() %>%
  ggplot() + geom_sf(colour = NA, fill = "blue" ) + geom_sf(data = ons_shp_gen3, fill = NA)



create_ldu_check_plots <- function(ldu_shp, ons_shp, ldus_intersect, title_name){
  
  plot_included <- ldu_shp %>%
    dplyr::filter(POSTALCODE %in% ldus_intersect$POSTALCODE) %>%
    ggplot2::ggplot() + 
    ggplot2::geom_sf(colour = NA, fill = "blue" ) + 
    ggplot2::geom_sf(data = ons_shp, fill = NA) +
    ggplot2::labs(title = paste0(title_name,": LDUs in DMTI file overlapping neighbourhoods"))
  
  plot_excluded <- ldu_shp %>%
    dplyr::filter(!POSTALCODE %in% ldus_intersect$POSTALCODE) %>%
    ggplot2::ggplot() + 
    ggplot2::geom_sf(colour = NA, fill = "blue" ) + 
    ggplot2::geom_sf(data = ons_shp, fill = NA) +
    ggplot2::labs(title = paste0(title_name,": LDUs in DMTI file not overlapping neighbourhoods"))
  
  ggplot2::ggsave(plot_included, filename = sprintf("results/images/%s-included-%s.csv", title_name, Sys.Date()))
  ggplot2::ggsave(plot_excluded, filename = sprintf("results/images/%s-excluded-%s.csv", title_name, Sys.Date()))
  
}

create_ldu_check_plot(ldu_shp, ons_shp_gen3, ldus_intersect_gen3_long, "ONS Gen3")
