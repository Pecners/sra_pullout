library(sf)
library(tidyverse)
library(cityforwardcollective)

assembly <- st_read("../Shapefiles/Wisconsin_Assembly_Districts_(2022)/Wisconsin_Assembly_Districts_(2022).shp")
ass_percs <- read_rds("../strategic_regional_analysis/data/ass_perc.rds")

ass_lims <- ass_percs |> 
  filter(ass_in_city > .1)

assembly |> 
  ggplot() +
  geom_sf(size = .5, fill = "#C5050C", color = "white") +
  theme_void()

city_limits <- st_read("../Shapefiles/Milwaukee/City Limits/citylimit.shp") |> 
  st_transform(crs = st_crs(assembly))

city_ass <- st_intersection(assembly, city_limits)

mke_ass <- assembly |> 
  filter(ASM2021 %in% ass_lims$ASM2021)

mke_ass |> 
  ggplot() +
  geom_sf() +
  geom_sf_text(aes(label = ASM2021)) +
  geom_sf(data = city_limits, fill = NA, color = "red") +
  theme_void() +
  theme(plot.title = element_text(family = "Georgia", hjust = .5,
                                  margin = margin(20, 0,0,0)),
        plot.title.position = "plot",
        text = element_text(family = "Verdana", size = 18),
        plot.caption = element_text(hjust = .5, color = "grey50",
                                    margin = margin(b = 5))) +
  labs(title = "Milwaukee State Assembly Districts",
       caption = "District boundaries reflect 2022 districts")

ggsave("../000_data_temp/wi_ass.png", bg = "white",
       width = 9, height = 11.5)


ass_with_reps <- mke_ass |> 
  mutate(rep = case_when(ASM2021 == 11 ~ "Drake",
                         ASM2021 %in% c(16, 17, 18) ~ "Johnson",
                         TRUE ~ ""))

ass_with_reps |> 
  ggplot() +
  geom_sf(aes(alpha = ifelse(rep == "", .25, 1))) +
  geom_sf_text(aes(label = ASM2021,
                   alpha = ifelse(rep == "", .25, 1))) +
  scale_alpha_identity() +
  theme_void()

