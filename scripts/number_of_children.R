# Number of children
# total number of children in census tract?
# number of children in each household or family? 

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

# view the table 
view(dc_2010_var)

# get the variables from the decennial census with spatial geometry 
sex_by_age_spatial <- get_decennial(geography = "tract",
              variables = c(
              "total"  = "P012001",
              "male_total" = "P012002",
              "male_<5" = "P012003",
              "male_5-9" = "P012004",
              "male_10-14" = "P012005",
              "male_15-17" = "P012006",
              "female_total" = "P012026",
              "female_<5" = "P012027",
              "female_5-9" = "P012028",
              "female_10-14" = "P012029",
              "female_15-17" = "P012030"
              ),
              cache_table = TRUE,
              year = 2010,
              state = "Arizona",
              county = "Coconino",
              geometry = TRUE
)

# get the variables from the decennial census with no spatial geometry 
sex_by_age <- get_decennial(geography = "tract",
                                    variables = c(
                                      "total"  = "P012001",
                                      "male_total" = "P012002",
                                      "male_<5" = "P012003",
                                      "male_5-9" = "P012004",
                                      "male_10-14" = "P012005",
                                      "male_15-17" = "P012006",
                                      "female_total" = "P012026",
                                      "female_<5" = "P012027",
                                      "female_5-9" = "P012028",
                                      "female_10-14" = "P012029",
                                      "female_15-17" = "P012030"
                                    ),
                                    cache_table = TRUE,
                                    year = 2010,
                                    state = "Arizona",
                                    county = "Coconino",
                                    geometry = FALSE
)

# show total population for each census tract 
sex_by_age_total <- sex_by_age %>%
  filter(variable == "total")

# view 
sex_by_age_total 

# calculate count of female children in each census tract 
sex_by_age_fem <- sex_by_age %>%
  filter(str_starts(variable, "fem") & variable != "female_total") %>%
  group_by(GEOID) %>%
  summarise(`female` = sum(value))

# inspect 
sex_by_age_fem

# calculate count of male children in each census tract 
sex_by_age_male <- sex_by_age %>%
  filter(str_starts(variable, "male") & variable != "male_total") %>%
  group_by(GEOID) %>%
  summarise(`male` = sum(value))

# inpect
sex_by_age_male

# join male and female child tables 
sex_by_age_counts <- full_join(
  x = sex_by_age_fem,
  y = sex_by_age_male,
  by = "GEOID"
)  %>% # calculate total 
  mutate(total_children = female + male)

# inspect 
sex_by_age_counts %>%
  arrange(GEOID)

# calculate total population for each census tract
sex_by_age_spatial_totals <- sex_by_age_spatial %>%
  filter(variable == "total") %>%
  arrange(GEOID) %>%
  group_by(GEOID) %>%
  summarise(total = sum(value))

# inspect
sex_by_age_spatial_totals

# create the spatial data
sex_by_age_total_spatial <- geo_join(
  spatial_data = sex_by_age_spatial_totals,
  data_frame = sex_by_age_counts,
  by_sp = c("GEOID"),
  by_df = c("GEOID"),
  how = "inner"
)

# plot of count of number of children age 17 and younger for each census tract 
sex_by_age_counts_spatial_plot <- sex_by_age_total_spatial %>%
  ggplot() +
  geom_sf(data = county_boundary) +
  geom_sf(mapping = aes(fill = total_children, color = total_children)) +
  geom_sf_text(data = county_muni_boundaries, mapping = aes(label = NAME)) +
  scale_fill_viridis_b(alpha = .9,) +
  scale_color_viridis_b(alpha = .9) +
  theme_void() +
  labs(title = "Number of Children",
       subtitle = "2010 Decennial Census",
       caption = str_wrap("Total number of children age 17 and younger in each Census tract.", width = 80),
       fill = "Count",
       color = "Count")

# view 
sex_by_age_counts_spatial_plot

# save to disk 
write_rds(sex_by_age_counts_spatial_plot,
          "data/number_of_children.rds")

# calculate the proportion 
sex_by_age_proportion_spatial <- sex_by_age_total_spatial %>%
  mutate(prop = total_children / total)

# plot of the proportion of number of children age 17 and younger for each census tract 
sex_by_age_proportion_spatial_plot <- sex_by_age_proportion_spatial %>%
  ggplot() +
  geom_sf(data = county_boundary) +
  geom_sf(mapping = aes(fill = prop, color = prop)) +
  geom_sf_text(data = county_muni_boundaries, mapping = aes(label = NAME)) +
  scale_fill_viridis_b(alpha = .9,) +
  scale_color_viridis_b(alpha = .9) +
  theme_void() +
  labs(title = "Proportion of Children to Total Population",
       subtitle = "2010 Decennial Census",
       caption = str_wrap("Proportion of children age 17 and younger in each Census tract.", width = 80),
       fill = "Proportion",
       color = "Proportion")

# view 
sex_by_age_proportion_spatial_plot

write_rds(sex_by_age_proportion_spatial_plot,
          "data/proportion_of_children.rds")

