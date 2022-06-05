# Required libraries
library(tidyverse)
library(lubridate)
library(rnaturalearth) # provides simple features of natural earth
library(sf)
library(gganimate)
library(ggtext)

# library(showtext)
# Font families
windowsFonts("Lobster" = windowsFont("Lobster"),
             "Montserrat" = windowsFont("Montserrat"))

options(repr.plot.width = 16, repr.plot.height = 9)

# Read the dataset from TidyTuesday Github repository
tf = tempfile(fileext = ".xlsx")
curl::curl_download("https://github.com/rfordatascience/tidytuesday/raw/master/data/2018/2018-04-16/global_mortality.xlsx", tf)
mortality <- readxl::read_excel(tf)


top_mortality <- mortality %>%
  gather(key = disease, value = mortality, -c(country, country_code, year)) %>%
  mutate(disease = substr(disease, 1, nchar(disease)-4),
         year = year(as.Date.character(year, format = "%Y"))) %>%
  group_by(country, year) %>%
  top_n(1, mortality)

b <- mortality %>%
  gather(key = disease, value = mortality, -c(country, country_code, year)) %>%
  mutate(disease = substr(disease, 1, nchar(disease)-4),
         year = year(as.Date.character(year, format = "%Y"))) %>% 
  group_by(year) %>% 
  top_n(3, mortality)

# this will be our base world map for plot excluding Antarctica
world <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf') %>% 
  filter(!name %in% c("Fr. S. Antarctic Lands", "Antarctica"))
world_merged <- merge(world, top_mortality, by.x = "name", by.y = "country")


p1 <- ggplot() +
  geom_sf(data = world, colour = "azure4", fill = "grey60", size = .5) +
  geom_sf(data = world_merged, aes(fill = disease, group = interaction(year, disease))) +
  coord_sf(crs = st_crs(world), datum = NA) +
  #geom_label(data = b, aes(wc_location_long, wc_location_lat, label = Location, group = interaction(Year, wc_location_long)), 
  #           colour = "white", fill = "#00000040", nudge_y = 4) +
  #geom_label(data = b, aes(x = -Inf, y = -Inf, label = paste(" Countries with highest mortality rates: \n", b[b$year == "{closest_state}",]$country[1], ": ", b[b$year == "{closest_state}",]$mortality[1], "\n", group = interaction(year, mortality)), 
  #           hjust = 0, vjust = 0, color = "white")) +
  transition_states(year) +
  #enter_fade() +
  #exit_fade() +
  labs(title = "What do people <i>die</i> from most, in {closest_state}?<br>", 
       x = NULL, 
       y = NULL, 
       caption = "<span>Graphic by Can Aytöre <b>&middot;</b> Data from <i>ourworldindata.org</i></span>") +
  ggthemes::theme_fivethirtyeight() +
  theme(
    plot.margin = grid::unit(c(9,16,9,16), "mm"),
    aspect.ratio = 9/16,
    text = element_text(family = "Montserrat"),
    legend.position = "top",
    legend.text = element_text(size = 24),
    legend.title = element_blank(),
    plot.title = element_markdown(family = "Lobster", colour = "brown4", hjust = 0.5, size = 48),
    plot.subtitle = element_markdown(size = 24),
    plot.caption = element_markdown(colour = "dodgerblue4", size = 24)
    )

# set animation interval as 1 second by adjusting duration parameter
animate(p1, renderer=gifski_renderer(), width = 1920, height = 1080, duration = length(unique(top_mortality$year)))
anim_save("global_mortality.gif")


