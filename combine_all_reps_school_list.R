library(tidyverse)
library(wisconsink12)
library(sf) 
library(scales)

# city limits

city_limits <- st_read("../Shapefiles/Milwaukee/City Limits/citylimit.shp")

# School list geocoded

geo_schools <- read_csv("data/geocoded_mke_schools.csv") %>%
  mutate(dpi_true_id = ifelse(dpi_true_id == "3619_0213",
                              "8152_8152", dpi_true_id)) |> 
  select(-student_count) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326) %>%
  left_join(., make_mke_schools() %>%
              filter(school_year == "2021-22") %>%
              select(dpi_true_id, school_name)) %>%
  left_join(., make_mke_rc() %>%
              filter(school_year == "2021-22") %>%
              select(-c(school_year, school_name))) %>%
  mutate(grade_band = case_when(dpi_true_id == "3619_0454" ~ "6-12",
                                dpi_true_id == "3619_0215" ~ "6-8",
                                dpi_true_id == "3619_0115" ~ "K-5",
                                dpi_true_id == "3619_1141" ~ "K-5",
                                dpi_true_id == "3619_0149" ~ "K-5",
                                TRUE ~ grade_band)) |> 
  st_transform(crs = st_crs(city_limits))

# Different district boundaries, transformed to city_limits crs
# note that leges includes both senate and assembly

ald <- st_read("../shapefiles/alderman2012/alderman.shp") |> 
  st_transform(crs = st_crs(city_limits))
leges <- st_read("../shapefiles/Wisconsin_Assembly_Districts_(2022)/Wisconsin_Assembly_Districts_(2022).shp") |> 
  st_transform(crs = st_crs(city_limits))
sbd <- st_read("../shapefiles/Milwaukee/MPS_B_Boundaries/MPS_B_Boundaries.shp") |> 
  st_transform(crs = st_crs(city_limits))

# proficiency data will be used throughout
prof <- read_rds("../report_cards_2021-22/data/all_school_prof.rda")
mke_rc <- make_mke_rc() |> 
  filter(school_year == "2021-22") |> 
  select(dpi_true_id,
         overall_score,
         overall_rating)

# handle common council (alderpeople)

council <- read_csv("data/mke_city_council_feb_2023.csv")

joined <- left_join(ald |> 
                      select(ALD, geometry),
                    council,
                    by = c("ALD" = "district"))

schools_in_dis <- st_intersection(geo_schools, joined)

slim <- schools_in_dis |> 
  as_tibble() |> 
  select(ALD,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         grade_band,
         school_enrollment) |> 
  left_join(mke_rc) |> 
  arrange(school_name)

pp <- prof |> 
  filter(school_year == "2021-22" & pa == "pa") |> 
  select(dpi_true_id,
         test_subject,
         perc) |> 
  pivot_wider(names_from = test_subject, values_from = perc)

tt <- left_join(slim, pp) |> 
  mutate_at(c("ELA", "Mathematics"), label_percent(1))

names(tt)[c(1, 9:10)] <- c("district",
                           "ela_proficiency",
                           "math_proficiency")

ttf <- tt |> 
  filter(!is.na(school_name)) |> 
  mutate(grade_band = paste0("'", grade_band)) |> 
  left_join(joined |> as_tibble() |> select(-geometry),
            by = c("district" = "ALD")) |> 
  mutate(district = as.character(district))

# handle leges
reps <- read_csv("data/state_legislature_2023.csv") |> 
  mutate(name = str_remove_all(name, " \\(i\\)"))

rep_skinny <- reps |> 
  # select(title:name) |> 
  filter(title == "Representative") 

cross <- read_csv("data/assembly_senate_dist_crosswalk.csv")

sen_skinny <- left_join(reps |> 
                          filter(title == "Senator"),
                        cross, by = c("district" = "SEN2021"))

schools_in_ass <- st_intersection(geo_schools, leges)

slim <- schools_in_ass |> 
  as_tibble() |> 
  select(ASM2021,
         SEN2021,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         grade_band,
         school_enrollment) |> 
  left_join(mke_rc) |> 
  arrange(school_name)


tt <- left_join(slim, pp) |> 
  mutate_at(c("ELA", "Mathematics"), label_percent(1))

names(tt)[c(1,2, 9:10)] <- c("ass_district",
                             "sen_district",
                           "ela_proficiency",
                           "math_proficiency")

ttf_rep <- tt |> 
  filter(!is.na(school_name)) |> 
  mutate(grade_band = paste0("'", grade_band)) |> 
  left_join(rep_skinny |> 
              mutate(district = as.character(district)),
            by = c("ass_district" = "district")) |> 
  select(-sen_district) |> 
  rename(district = ass_district)

ttf_sen <- tt |> 
  filter(!is.na(school_name)) |> 
  mutate(grade_band = paste0("'", grade_band)) |> 
  left_join(sen_skinny |> 
              select(-ASM2021) |> 
              unique() |> 
              mutate(district = as.character(district)),
            by = c("sen_district" = "district")) |> 
  select(-ass_district) |> 
  rename(district = sen_district)

# handle school board districts

sbd_members <- read_csv("data/mps_school_board_feb_2023.csv")

joined <- left_join(sbd |> 
                      select(district = DISTRICT, geometry) |> 
                      add_row(city_limits |> 
                                select(geometry) |> 
                                mutate(district = "At Large")
                      ),
                    sbd_members)

schools_in_dis <- st_intersection(geo_schools, joined)

slim <- schools_in_dis |> 
  as_tibble() |> 
  select(district,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         grade_band,
         school_enrollment) |> 
  left_join(mke_rc) |> 
  arrange(school_name)

tt <- left_join(slim, pp) |> 
  mutate_at(c("ELA", "Mathematics"), label_percent(1))

names(tt)[c(1, 9:10)] <- c("district",
                           "ela_proficiency",
                           "math_proficiency")

ttf_sbd <- tt |> 
  filter(!is.na(school_name)) |> 
  mutate(grade_band = paste0("'", grade_band)) |> 
  left_join(joined |> as_tibble() |> select(-geometry)) |> 
  mutate(district = as.character(district))

# combine

all <- bind_rows(ttf_sbd, ttf_rep) |> 
  bind_rows(ttf_sen) |> 
  bind_rows(ttf)

saveRDS(all, "../school_reps_shiny/full.rda")

