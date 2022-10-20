# Number renting vs. owning vs. homeless

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
housing_tenure_spatial <- get_decennial(geography = "tract",
                                    variables = c(
                                      "total"  = "H011001",
                                      "owned_with_mortgage" = "H011002",
                                      "owned_outright" = "H011003",
                                      "renting" = "H011004"
                                    ),
                                    cache_table = TRUE,
                                    year = 2010,
                                    state = "Arizona",
                                    county = "Coconino",
                                    geometry = TRUE
)

# get the variables from the decennial census with no spatial geometry 
housing_tenure <- get_decennial(geography = "tract",
                            variables = c(
                              "total"  = "H011001",
                              "owned_with_mortgage" = "H011002",
                              "owned_outright" = "H011003",
                              "renting" = "H011004"
                            ),
                            cache_table = TRUE,
                            year = 2010,
                            state = "Arizona",
                            county = "Coconino",
                            geometry = FALSE
)

# recode to compare between Number renting vs. owning vs. homeless
housing_tenure_totals <- housing_tenure %>%
  mutate(
    occupation_code = case_when(
      str_detect(variable, "owned_") ~ "owner_occupied",
      str_detect(variable, "rent") ~ "renter_occuped",
      str_detect(variable, "total") ~ "total",
    )
  ) %>%
  mutate(occupation_code = factor(occupation_code))

# summarize group total for each census tract
housing_tenure_summary <- housing_tenure_totals %>%
  group_by(GEOID, occupation_code) %>%
  summarise(value = sum(value))

# view 
housing_tenure_summary

# join with spatial 
housing_tenure_summary_spatial <- geo_join(spatial_data = housing_tenure_spatial,
         data_frame = housing_tenure_summary,
         by_sp = "GEOID",
         by_df = "GEOID",
         how = "inner")

# plot spatial 
housing_tenure_summary_spatial_plot <- housing_tenure_summary_spatial %>%
  filter(occupation_code != "total") %>%
  ggplot() +
  geom_sf(mapping = aes(fill = value.y)) +
  facet_wrap(~occupation_code) +
  scale_fill_viridis_b() +
  theme_void() +
  labs(title = "Total population in occupied housing units",
       fill = "Count")

# view 
housing_tenure_summary_spatial_plot

# save to disk 
write_rds(housing_tenure_summary_spatial_plot, "data/housing_tenure_summary_spatial_plot.rds")
