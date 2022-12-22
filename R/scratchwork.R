
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
  
  in_ldu_shp <- ldu_shp %>%
    dplyr::filter(POSTALCODE %in% ldus_intersect$POSTALCODE)
  
  plot_included <-  gen2_ldu_shp %>%
    ggplot2::ggplot() + 
    ggplot2::geom_sf(colour = NA, fill = "blue" ) + 
    ggplot2::geom_sf(data = ons_shp, fill = NA) +
    ggplot2::labs(title = paste0(title_name,": LDUs in DMTI file overlapping neighbourhoods"),
                  subtitle = paste0("n=",nrow(in_ldu_shp)))
  
  out_ldu_shp <- ldu_shp %>%
    dplyr::filter(POSTALCODE %in% ldus_intersect$POSTALCODE)
  
  plot_excluded <- out_ldu_shp %>%
    ggplot2::ggplot() + 
    ggplot2::geom_sf(colour = NA, fill = "blue" ) + 
    ggplot2::geom_sf(data = ons_shp, fill = NA) +
    ggplot2::labs(title = paste0(title_name,": LDUs in DMTI file not overlapping neighbourhoods"),
                  subtitle = paste0("n=",nrow(out_ldu_shp)))
  
  ggplot2::ggsave(plot_included, filename = sprintf("results/images/%s-included-%s.csv", title_name, Sys.Date()))
  ggplot2::ggsave(plot_excluded, filename = sprintf("results/images/%s-excluded-%s.csv", title_name, Sys.Date()))
  
}

create_ldu_check_plot(ldu_shp, ons_shp_gen3, ldus_intersect_gen3_long, "ONS Gen3")






########### THERE ARE EDGE EFFECTS WE NEED TO WORRY ABOUT

# LDUs around the edges may overlap a neighbourhood by a tiny amount and have
# most of their area outside of Ottawa. in such cases, we don't want to assign
# all of their value to ottawa!

ons_mask <- sf::st_union(ons_shp_gen3) %>%
  sf::st_transform(crs = 32189) %>%
  sf::st_buffer(250) %>%
  sf::st_transform(crs = "WGS84")

ggplot(ons_mask) + geom_sf()

ldus <- ldu_shp[as.vector(t(sf::st_intersects(ons_mask, ldu_shp, sparse = FALSE))),]
ggplot(ldus)+ geom_sf()


index_contained_ldus <- as.vector( t(sf::st_contains(ons_mask, ldus, sparse = FALSE)) )

ldus_not_contained <- ldus[!index_contained_ldus,]  
ldus[!index_contained_ldus,] %>%
  ggplot() + geom_sf(data = ons_mask, fill = "blue") + geom_sf()


leaflet() %>% addTiles() %>% addPolygons(data = ons_mask) %>% addPolygons(data = ldus_not_contained, label = ~ POSTALCODE, highlightOptions = highlightOptions(color = "orange", fillColor = "red"))

  dplyr::filter(sf::st_intersects(geometry, ons_mask, sparse = FALSE)) 
tictoc::toc()
  ggplot() + geom_sf(data = ons_mask) + geom_sf()

  
  
  ## OPTIMIZE INTERSECTION

  tictoc::tic()
ldus_intersect <- ldus_batch %>%
  #mutate(geometry = purrr::map(geometry, st_buffer, dist = 0)) %>%
  dplyr::mutate(results = purrr::map(geometry, function(x) {
    purrr::map(ons_trim$geometry, sf::st_intersection, y=x) %>%
      purrr::map_dbl(sf::st_area) %>%
      dplyr::tibble(ONS_ID = ons_trim$ONS_ID, Name = ons_trim$Name, intersection_area = .)
  })) %>%
  sf::st_set_geometry(NULL) %>%
  unnest() %>%
  filter(intersection_area > 0)
tictoc::toc()

tictoc::tic()
ldus_intersect2 <- sf::st_intersection(ldus_batch, ons_trim) %>%
  dplyr::mutate(intersection_area = purrr::map_dbl(geometry, sf::st_area))
tictoc::toc()



tictoc::tic()
ons_trim <- sf::st_transform(ons_trim, crs = 32189)
ldus_batch <- sf::st_transform(ldus_batch, crs = 32189)  %>%
  sf::st_make_valid()
  
ldus_area <- sf::st_set_geometry(ldus_batch, NULL)

ldus_batch <- ldus %>% sf::st_transform(crs  = 32189)
ldus_batch <- ldus %>% head(1000) %>% sf::st_transform(crs  = 32189) %>% sf::st_make_valid()
ldus_batch <- ldu_shp %>% head(30000) %>% sf::st_transform(crs  = 32189) %>% sf::st_make_valid()
  
#ldus_batch <- ldu_shp
ons_trim <- ons_shp_gen3 %>% sf::st_transform(crs = 32189) %>% sf::st_make_valid()
ons_trim <- ons_shp_gen3_trim %>% sf::st_transform(crs = 32189)

tictoc::tic()
ldus_intersect3 <-  ldus_batch %>%
  dplyr::mutate(total_area = purrr::map_dbl(geometry, sf::st_area)) %>%
  sf::st_intersection(ons_trim ) %>%
  #left_join(ldus_area)
  dplyr::mutate(intersection_area = purrr::map_dbl(geometry, sf::st_area))
tictoc::toc()

ldus_intersect3 %>%
  sf::st_set_geometry(NULL) %>%
  select(POSTALCODE, ONS_ID, total_area, intersection_area) %>%
  arrange(POSTALCODE) %>%
  mutate(weight = intersection_area / total_area) %>%
  group_by(POSTALCODE) %>%
  summarise(total = sum(weight)) %>%
  filter(total < 0.999)
  

ldus_intersect3 %>%
  select(POSTALCODE, ONS_ID, total_area, intersection_area) %>%
  arrange(POSTALCODE) %>%
  group_by(POSTALCODE) %>%
  mutate(total_intersection_area = sum(intersection_area)) %>%
  ungroup() %>%
  filter(total_area - total_intersection_area > 1)







##

tictoc::tic()
z <- get_ldu_intersection2(sf::st_transform(ldu_shp_trim, crs = 32189), sf::st_transform(ons_shp_gen3_trim, crs = 32189), denominator_area = "intersecting")
tictoc::toc()


ldus_sli_gen2 %>% filter(POSTALCODE %in% c("K0A2H0", "K0A1L0"))

forplot <- left_join(ldu_shp, ldus_sli_gen2, by = "POSTALCODE") %>% mutate(ONS_ID = as.character(ONS_ID))

pal <- leaflet::colorFactor(palette ="RdYlBu", domain = forplot$ONS_ID)

leaflet(forplot) %>% addPolygons(label = ~ paste0(POSTALCODE,": ", ONS_ID), fillColor = ~ pal (forplot$ONS_ID), weight = 1)




############ check against last crosswalk
library(tidyverse)
targets::tar_load(c(previous_2021_sli, ldus_sli_gen2, ldus_sli_gen2_augmented))

sli_new <- readr::read_csv("results/ldus_ons_gen2_sli_2022-11-25.csv") %>%
  rename(ONS_ID_NEW = ONS_ID)

sli_old <- readr::read_csv("results/archive/LDUS_ONS_augmented_SLI_(2020.12.02).csv") %>%
  rename(ONS_ID_OLD = ONS_ID)


sli_older = previous_2021_sli %>%
  rename(ONS_ID_KADY2021 = ONS_ID)

sli_comp <- full_join(sli_new, sli_old, by = "POSTALCODE") %>%
  full_join(sli_older, by = "POSTALCODE") %>%
  mutate(diff = ONS_ID_NEW != ONS_ID_OLD) %>%
  mutate(oldkadydiff = ONS_ID_OLD != ONS_ID_KADY2021) %>%
  mutate(newkadydiff = ONS_ID_NEW != ONS_ID_KADY2021)

full_join(dplyr::rename(ldus_sli_gen2, ONS_ID_NEW = ONS_ID), 
          dplyr::rename(previous_2021_sli, ONS_ID_OLD = ONS_ID), 
          by = "POSTALCODE") %>%
  dplyr::filter(is.na(ONS_ID_NEW)) %>%
  dplyr::select(POSTALCODE, ONS_ID = ONS_ID_OLD)



# tests!

# there are MORE THAN ZERO postal codes in the old SLI that are not in the new unaugmented SLI
testthat::expect_gt(nrow(dplyr::filter(previous_2021_sli, !POSTALCODE %in% ldus_sli_gen2$POSTALCODE)), 0)

# there are ZERO postal codes in the old SLI that are not in the new  augmented SLI
testthat::expect_equal(nrow(dplyr::filter(previous_2021_sli, !POSTALCODE %in% ldus_sli_gen2_augmented$POSTALCODE)), 0)




#############

# looking at MORE missing LDUs...
library(tidyverse)
targets::tar_load(ldus_sli_gen2_augmented)

missing_codes <- readr::read_csv("data/missing postal codes mortgage_debt_2019.csv") %>%
  rename(POSTALCODE = `Postal Code`, ONS_ID = HOOD_ID)

ldus_sli_gen2_augmented

missing_codes %>%
  filter(!POSTALCODE %in% ldus_sli_gen2_augmented$POSTALCODE)
