library(tidyverse)
library(cityforwardcollective)
library(knitr)
library(kableExtra)
library(RColorBrewer)
library(scales)
library(wisconsink12)
library(sf) 
library(showtext)
library(quarto)
library(glue)

reps <- read_csv("data/state_legislature.csv")
names(reps)[8] <- "next_election"
reps <- reps |> 
  mutate(district = as.character(district))

cfc_comp_file <- "../shapefiles/WI_Assembly_Districts_2022/Wisconsin_Assembly_Districts_(2022).shp"

if (file.exists(cfc_comp_file)) {
  assembly <- st_read(cfc_comp_file)
} else {
  assembly <- st_read("../shapefiles/Wisconsin_Assembly_Districts_(2022)/Wisconsin_Assembly_Districts_(2022).shp")
}

ass_percs <- read_rds("../strategic_regional_analysis/data/ass_perc.rds")

ass_lims <- ass_percs |> 
  filter(ass_in_city > .1)

geo_schools <- read_csv("../000_data_temp/geocoded_mke_schools.csv") %>%
  select(-student_count) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326) %>%
  left_join(., make_mke_schools() %>%
              filter(school_year == "2020-21") %>%
              select(dpi_true_id, school_name)) %>%
  left_join(., make_mke_rc() %>%
              filter(school_year == "2020-21") %>%
              select(-c(school_year, school_name))) %>%
  mutate(grade_band = case_when(dpi_true_id == "3619_0454" ~ "6-12",
                                dpi_true_id == "3619_0215" ~ "6-8",
                                dpi_true_id == "3619_0115" ~ "K-5",
                                dpi_true_id == "3619_1141" ~ "K-5",
                                dpi_true_id == "3619_0149" ~ "K-5",
                                TRUE ~ grade_band)) |> 
  st_transform(crs = st_crs(assembly))


city_limits <- st_read("../Shapefiles/Milwaukee/City Limits/citylimit.shp") |> 
  st_transform(crs = st_crs(assembly))

city_ass <- st_intersection(assembly, city_limits)

mke_ass <- assembly |> 
  filter(ASM2021 %in% ass_lims$ASM2021)


ass_with_reps <- left_join(mke_ass, 
                           reps |> 
                             filter(title == "Representative"),
                           by = c("ASM2021" = "district"))

ass_with_sens <- left_join(mke_ass, 
                           reps |> 
                             filter(title == "Senator"),
                           by = c("SEN2021" = "district"))


env

saveRDS(ass_with_reps, "data/ass_with_reps.rda")

saveRDS(ass_with_sens, "data/ass_with_sens.rda")

# Assembly

walk(1:nrow(ass_with_reps), function(i) {
  this <- ass_with_reps[i,]
  
  outfile <- glue("compiled_reports/{this$title} {this$name}-District {this$ASM2021}.pdf")
  
  quarto_render(input = "template_report/template_report.qmd", 
                execute_params = list("district" = this$ASM2021,
                                   "representative"  = this$name,
                                   "honorific" = this$title), 
                output_file = outfile)
})

# Senate 
walk(1:nrow(ass_with_sens), function(i) {
  this <- ass_with_sens[i,]
  
  outfile <- glue("compiled_reports/{this$title} {this$name}-District {this$SEN2021}.pdf")
  
  quarto_render(input = "template_report/template_report.qmd", 
                execute_params = list("district" = unique(this$SEN2021),
                                      "representative"  = unique(this$name),
                                      "honorific" = unique(this$title)), 
                output_file = outfile)
})
