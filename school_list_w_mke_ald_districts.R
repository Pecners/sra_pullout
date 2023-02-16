library(tidyverse)
library(wisconsink12)
library(sf) 
library(scales)

ald <- st_read("../shapefiles/alderman2012/alderman.shp")

ald |> 
  ggplot() + 
  geom_sf()

council <- read_csv("data/mke_city_council_feb_2023.csv")

joined <- left_join(ald |> 
                      select(ALD, geometry),
                    council,
                    by = c("ALD" = "district"))



geo_schools <- read_csv("data/geocoded_mke_schools.csv") %>%
  mutate(dpi_true_id = ifelse(dpi_true_id == "3619_0213",
                              "8152_815 |> 2", dpi_true_id)) |> 
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
  st_transform(crs = st_crs(joined))


schools_in_dis <- st_intersection(geo_schools, joined)

slim <- schools_in_dis |> 
  as_tibble() |> 
  select(ALD,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         grade_band,
         school_enrollment) |> 
  left_join(make_mke_rc() |> 
              filter(school_year == "2021-22") |> 
              select(dpi_true_id,
                     overall_score,
                     overall_rating)) |> 
  arrange(school_name)

prof <- read_rds("../report_cards_2021-22/data/all_school_prof.rda")


pp <- prof |> 
  filter(school_year == "2021-22" & pa == "pa") |> 
  select(dpi_true_id,
         test_subject,
         perc) |> 
  pivot_wider(names_from = test_subject, values_from = perc)

tt <- left_join(slim, pp) |> 
  mutate_at(c("ELA", "Mathematics"), label_percent(1))

names(tt)[c(1, 9:10)] <- c("aldermanic_district",
                           "ela_proficiency",
                           "math_proficiency")

ttf <- tt |> 
  filter(!is.na(school_name)) |> 
  mutate(grade_band = paste0("'", grade_band)) |> 
  left_join(joined |> as_tibble() |> select(-geometry),
            by = c("aldermanic_district" = "ALD"))

glimpse(ttf)

saveRDS(ttf, "data/city_council.rda")
saveRDS(ttf, "../school_reps_shiny/city_council.rda")

ttf |> 
  mutate_at(c("assembly_district", "district"), as.numeric) |> 
  arrange(assembly_district) |> 
  select("Assembly District" = assembly_district,
         Representative,
         "Senate District" = district,
         "Senator" = name,
         "School Name" = school_name,
         "Sector" = accurate_agency_type,
         "Grade Band" = grade_band,
         "Enrollment" = school_enrollment,
         "RC Overall Score" = overall_score,
         "RC Overall Rating" = overall_rating,
         "ELA Proficiency" = ela_proficiency,
         "Math Proficiency" = math_proficiency) |> 
  write_csv("data/MKE School List with Leges.csv")

mke_schools <- make_mke_schools() |> 
  filter(school_year == "2021-22")

geo_schools |> 
  filter(!dpi_true_id %in% mke_schools$dpi_true_id) |> 
  as_tibble() |> 
  select(dpi_true_id) |> 
  left_join(make_mke_schools()) |> 
  select(school_name)

mke_schools |> 
  filter(!dpi_true_id %in% ttf$dpi_true_id)
