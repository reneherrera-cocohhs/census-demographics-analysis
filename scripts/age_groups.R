# Ages broken out 0-5, 5-14, 15-19, 20-44, 45-64, 65-69, 70+


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
age_groups_spatial <- get_decennial(geography = "tract",
                                    variables = c(
                                      "total"  = "P012001",
                                      "male_total" = "P012002",
                                      "male_<5" = "P012003",
                                      "male_5-9" = "P012004",
                                      "male_10-14" = "P012005",
                                      "male_15-17" = "P012006",
                                      "male_18-19" = "P012007",
                                      "male_20" = "P012008",
                                      "male_21" = "P012009",
                                      "male_22-24" = "P012010",
                                      "male_25-29" = "P012011",
                                      "male_30-34" = "P012012",
                                      "male_35-39" = "P012013",
                                      "male_40-44" = "P012014",
                                      "male_45-49" = "P012015",
                                      "male_50-54" = "P012016",
                                      "male_55-59" = "P012017",
                                      "male_60-61" = "P012018",
                                      "male_62-64" = "P012019",
                                      "male_65-66" = "P012020",
                                      "male_67-69" = "P012021",
                                      "male_70-74" = "P012022",
                                      "male_75-79" = "P012023",
                                      "male_80-84" = "P012024",
                                      "male_85+" = "P012025",
                                      "female_total" = "P012026",
                                      "female_<5" = "P012027",
                                      "female_5-9" = "P012028",
                                      "female_10-14" = "P012029",
                                      "female_15-17" = "P012030",
                                      "female_18-19" = "P012031",
                                      "female_20" = "P012032",
                                      "female_21" = "P012033",
                                      "female_22-24" = "P012034",
                                      "female_25-29" = "P012035",
                                      "female_30-34" = "P012036",
                                      "female_35-39" = "P012037",
                                      "female_40-44" = "P012038",
                                      "female_45-49" = "P012039",
                                      "female_50-54" = "P012040",
                                      "female_55-59" = "P012041",
                                      "female_60-61" = "P012042",
                                      "female_62-64" = "P012043",
                                      "female_65-66" = "P012044",
                                      "female_67-69" = "P012045",
                                      "female_70-74" = "P012046",
                                      "female_75-79" = "P012047",
                                      "female_80-84" = "P012048",
                                      "female_85+" = "P012049"
                                    ),
                                    cache_table = TRUE,
                                    year = 2010,
                                    state = "Arizona",
                                    county = "Coconino",
                                    geometry = TRUE
)

# get the variables from the decennial census with no spatial geometry 
age_groups <- get_decennial(geography = "tract",
                                    variables = c(
                                      "total"  = "P012001",
                                      "male_total" = "P012002",
                                      "male_<5" = "P012003",
                                      "male_5-9" = "P012004",
                                      "male_10-14" = "P012005",
                                      "male_15-17" = "P012006",
                                      "male_18-19" = "P012007",
                                      "male_20" = "P012008",
                                      "male_21" = "P012009",
                                      "male_22-24" = "P012010",
                                      "male_25-29" = "P012011",
                                      "male_30-34" = "P012012",
                                      "male_35-39" = "P012013",
                                      "male_40-44" = "P012014",
                                      "male_45-49" = "P012015",
                                      "male_50-54" = "P012016",
                                      "male_55-59" = "P012017",
                                      "male_60-61" = "P012018",
                                      "male_62-64" = "P012019",
                                      "male_65-66" = "P012020",
                                      "male_67-69" = "P012021",
                                      "male_70-74" = "P012022",
                                      "male_75-79" = "P012023",
                                      "male_80-84" = "P012024",
                                      "male_85+" = "P012025",
                                      "female_total" = "P012026",
                                      "female_<5" = "P012027",
                                      "female_5-9" = "P012028",
                                      "female_10-14" = "P012029",
                                      "female_15-17" = "P012030",
                                      "female_18-19" = "P012031",
                                      "female_20" = "P012032",
                                      "female_21" = "P012033",
                                      "female_22-24" = "P012034",
                                      "female_25-29" = "P012035",
                                      "female_30-34" = "P012036",
                                      "female_35-39" = "P012037",
                                      "female_40-44" = "P012038",
                                      "female_45-49" = "P012039",
                                      "female_50-54" = "P012040",
                                      "female_55-59" = "P012041",
                                      "female_60-61" = "P012042",
                                      "female_62-64" = "P012043",
                                      "female_65-66" = "P012044",
                                      "female_67-69" = "P012045",
                                      "female_70-74" = "P012046",
                                      "female_75-79" = "P012047",
                                      "female_80-84" = "P012048",
                                      "female_85+" = "P012049"
                                    ),
                                    cache_table = TRUE,
                                    year = 2010,
                                    state = "Arizona",
                                    county = "Coconino",
                                    geometry = FALSE
)

# recode age groups to match request 
# Ages broken out 0-5, 5-14, 15-19, 20-44, 45-64, 65-69, 70+
age_groups_grouped <- age_groups %>%
  mutate(age_group = case_when(
    str_detect(variable, "_<5") ~ "age_0-5",
    str_detect(variable, "_5-9") ~ "age_5-14",
    str_detect(variable, "_10-14") ~ "age_5-14",
    str_detect(variable, "_15-17") ~ "age_15-19",
    str_detect(variable, "_18-19") ~ "age_15-19",
    str_detect(variable, "_20") ~ "age_20-44",
    str_detect(variable, "_21") ~ "age_20-44",
    str_detect(variable, "_22-24") ~ "age_20-44",
    str_detect(variable, "_25-29") ~ "age_20-44",
    str_detect(variable, "_30-34") ~ "age_20-44",
    str_detect(variable, "_35-39") ~ "age_20-44",
    str_detect(variable, "_40-44") ~ "age_20-44",
    str_detect(variable, "_45-49") ~ "age_45-64",
    str_detect(variable, "_50-54") ~ "age_45-64",
    str_detect(variable, "_55-59") ~ "age_45-64",
    str_detect(variable, "_60-61") ~ "age_45-64",
    str_detect(variable, "_62-64") ~ "age_45-64",
    str_detect(variable, "_65-66") ~ "age_65-69",
    str_detect(variable, "_67-69") ~ "age_65-69",
    str_detect(variable, "_70-74") ~ "age_70+",
    str_detect(variable, "_75-79") ~ "age_70+",
    str_detect(variable, "_80-84") ~ "age_70+",
    str_detect(variable, "_85+") ~ "age_70+"
  ),
  sex = case_when(
    str_detect(variable, "female") ~ "female",
    str_detect(variable, "male") ~ "male"
  ))

# totals for each sex and age group in each census tract 
age_groups_summary_totals <- age_groups_grouped %>%
  drop_na(age_group) %>%
  group_by(GEOID, sex, age_group) %>%
  summarise(value = sum(value)) %>%
  ungroup()

# change to factor
age_groups_summary_totals <- age_groups_summary_totals %>%
  mutate(age_group = factor(age_group, levels = c("age_0-5",
                                                  "age_5-14",
                                                  "age_15-19",
                                                  "age_20-44",
                                                  "age_45-64",
                                                  "age_65-69",
                                                  "age_70+")))

# convirm levels exist in order 
levels(age_groups_summary_totals$age_group)

# view 
age_groups_summary_totals

# save to disk 
write_rds(age_groups_summary_totals, file = "data/age_group_summary_totals.rds")

# plot 
age_groups_summary_totals_plot <- age_groups_summary_totals %>%
  ggplot(aes(x = age_group, fill = sex,
                   y = ifelse(test = sex == "male",
                              yes = -value, no = value))) +
  geom_col() +
  scale_y_continuous(limits = c(-26000, 26000)) +
  scale_fill_brewer(palette = "Accent") +
  coord_flip() +
  theme_classic() +
  labs(title = "Population by Age Group",
       x = " Age group",
       y = "Count",
       fill = "Sex") +
  theme(aspect.ratio = 4/3)

# view
age_groups_summary_totals_plot

# save to disk 
write_rds(age_groups_summary_totals_plot, "data/age_group_summary_totals_plot.rds")

# totals by age group
age_groups_summary_totals_by_age <- age_groups_summary_totals %>%
  group_by(GEOID, age_group) %>%
  summarise(value = sum(value)) %>%
  ungroup()
  
# join totals to spatial data 
age_groups_spatial_summary <- geo_join(
  spatial_data = age_groups_spatial,
  data_frame = age_groups_summary_totals_by_age, 
  by_sp = "GEOID",
  by_df = "GEOID",
  how = "inner"
)

# plot spatial 
age_groups_spatial_summary_plot <- age_groups_spatial_summary %>%
  ggplot() +
  geom_sf(mapping = aes(fill = value.y)) +
  facet_wrap(~age_group) +
  scale_fill_viridis_b() +
  theme_void() +
  labs(title = "Population by Age Group",
       fill = "Count")

# view
age_groups_spatial_summary_plot

# save to disk 
write_rds(age_groups_spatial_summary_plot, "data/age_groups_spatial_summary_plot.rds")
