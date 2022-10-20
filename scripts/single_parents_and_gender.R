# single parents and gender 

# setup 
# packages 
library(here)
library(tidyverse)
library(tidycensus)
library(sf)
library(tmap)
library(tigris)

# options
options(tigris_use_cache = TRUE)

# get data 
# read county boundary 
county_boundary <- st_read(dsn = "../gis_data/County_Boundary.geojson")
county_muni_boundaries <- st_read(dsn = "../gis_data/County_Municipal_Boundaries.geojson")

# load decennial census variables 
dc_2010_var <- load_variables(year = 2010,
                              dataset = "sf1",
                              cache = TRUE)

# from decennial census 
# "Male householder, no wife present!!With own children under 18 years" = "P038010",
# "Female householder, no husband present!!With own children under 18 years" = "P038016"
single_parent_data_spatial <- get_decennial(geography = "tract",
              variables = c(
                "total" = "P040001",
                "male" = "P038010",
                "female" = "P038016"
              ),
              cache_table = TRUE,
              year = 2010,
              state = "Arizona",
              county = "Coconino",
              geometry = TRUE
                )

single_parent_data <- get_decennial(geography = "tract",
                                            variables = c(
                                              "total" = "P040001",
                                              "male" = "P038010",
                                              "female" = "P038016"
                                            ),
                                            cache_table = TRUE,
                                            year = 2010,
                                            state = "Arizona",
                                            county = "Coconino",
                                            geometry = FALSE
)

household_sample <- single_parent_data %>%
  mutate(variable = factor(variable, levels = c("total", "female", "male"))) %>%
  pivot_wider(names_from = "variable", 
              names_prefix = "value_",
              values_from = "value") %>%
  mutate(prop_male = value_male / value_total,
         prop_female = value_female / value_total)
  
household_sample_count <- household_sample %>%
  select(1:5) %>%
  pivot_longer(cols = starts_with("value"),
               names_to = "variable",
               values_to = "count") %>%
  mutate(variable = if_else(str_detect(variable, "total"), "total", 
                            if_else(str_detect(variable, "female"), "female", "male")))

household_sample_prop <- household_sample %>%
  select(1,2,6,7) %>%
  pivot_longer(cols = starts_with("prop"),
               names_to = "variable",
               values_to = "prop") %>%
  mutate(variable = if_else(str_detect(variable, "total"), "total", 
                            if_else(str_detect(variable, "female"), "female", "male")))

household_sample <- inner_join(x = household_sample_count,
          y = household_sample_prop,
          by = c("GEOID", "NAME", "variable"))

household_sample_spatial <- geo_join(
  spatial_data = single_parent_data_spatial,
  data_frame = household_sample,
  # by_sp = c("GEOID", "NAME", "variable"),
  # by_df = c("GEOID", "NAME", "variable"),
  by = c("GEOID", "NAME", "variable"),
  how = "inner"
)
  
  full_join(
  x = household_sample,
  y = single_parent_data_spatial,
  by = c("GEOID", "NAME", "variable")
)


quantile(single_parent_data$value)

ggplot(data = single_parent_data) +
  geom_histogram(mapping = aes(value))


household_sample_spatial %>%
  filter(variable != "total") %>%
  ggplot() +
  geom_sf(data = county_boundary) +
  # geom_sf_text(data = county_muni_boundaries, mapping = aes(label = NAME)) +
  geom_sf(mapping = aes(fill = prop, color = prop)) +
  scale_fill_viridis_c(alpha = .6) +
  scale_color_viridis_c(alpha = .6) +
  facet_wrap(~variable) +
  theme_void()

single_parent_household_by_gender <- 
household_sample_spatial %>%
  filter(variable != "total") %>%
  ggplot() +
  geom_sf(data = county_boundary) +
  geom_sf(mapping = aes(fill = prop, color = prop)) +
  geom_sf_text(data = county_muni_boundaries, mapping = aes(label = NAME)) +
  scale_fill_viridis_b(alpha = .9,) +
  scale_color_viridis_b(alpha = .9) +
  facet_wrap(~variable) +
  theme_void() +
  labs(title = "Single Parents and Gender",
       subtitle = "2010 Decennial Census",
       caption = "Male householder, no wife present!!With own children under 18 years, 
       Female householder, no husband present!!With own children under 18 years",
       fill = "Proportion",
       color = "Proportion")

write_rds(single_parent_household_by_gender,
          file = "data/single_parent_household_by_gender_plot.rds")

tm_shape(household_sample_spatial) +
  tm_polygons("value")

household_sample %>%
  filter(variable == "total") %>%
  tm_shape() +
  tm_fill("value", 
          alpha = .9, 
          title = "Total Households with Children <18 years")

household_sample_spatial %>%
  filter(variable == "female") %>%
  tm_shape() +
  tm_fill("prop", 
          alpha = .9, 
          title = "Single Parent Female Households with Children <18 years")

household_sample_spatial %>%
  filter(variable == "male") %>%
  tm_shape() +
  tm_fill("prop", 
          alpha = .9, 
          title = "Single Parent Male Households with Children <18 years")

household_sample_spatial %>%
  filter(variable != "total") %>%
  tm_shape() +
  tm_fill("value") +
  tm_facets(by = "variable")
