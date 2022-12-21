library(tidyverse)
library(wisconsink12)
library(sf) 
library(scales)

ass_with_reps <- read_rds("data/ass_with_reps.rda")
prof <- read_rds("../report_cards_2021-22/data/all_school_prof.rda")


geo_schools <- read_csv("../000_data_temp/geocoded_mke_schools.csv") %>%
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
  st_transform(crs = st_crs(ass_with_reps))


schools_in_ass <- st_intersection(geo_schools, ass_with_reps)

slim <- schools_in_ass |> 
  as_tibble() |> 
  select(ASM2021,
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


pp <- prof |> 
  filter(school_year == "2021-22" & pa == "pa") |> 
  select(dpi_true_id,
         test_subject,
         perc) |> 
  pivot_wider(names_from = test_subject, values_from = perc)

tt <- left_join(slim, pp) |> 
  mutate_at(c("ELA", "Mathematics"), label_percent(1))

names(tt)[c(1, 9:10)] <- c("assembly_district",
                          "ela_proficiency",
                          "math_proficiency")

ttf <- tt |> 
  select(assembly_district,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         grade_band,
         school_enrollment,
         rc_overall_score = overall_score,
         rc_overall_rating = overall_rating,
         ela_proficiency,
         math_proficiency) |> 
  filter(!is.na(school_name)) |> 
  mutate(grade_band = paste0("'", grade_band))

write_csv(ttf, "data/school_list_with_assembly_districts.csv")

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
