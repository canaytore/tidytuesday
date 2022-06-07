# Required libraries
library(tidyverse)
library(ggtext)
library(maps)
library(fiftystater)

# library(showtext)
# Font families
windowsFonts("Lobster" = windowsFont("Lobster"),
             "Montserrat" = windowsFont("Montserrat"))

options(repr.plot.width = 16, repr.plot.height = 9)

# Read the dataset from TidyTuesday Github repository
asc <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2018/2018-04-30/week5_acs2015_county_data.csv")

county <- map_data("county") %>%
  as_tibble() %>%
  rename(State = region, County = subregion) %>%
  mutate_at(c("State", "County"), .funs=str_to_title) %>%
  mutate(County = str_replace(County, "Dona Ana", "Doña Ana")) 

asc_merged <- left_join(county, asc, by = c("State", "County"))

#Missing state&counties;
asc %>%
 filter(is.na(MeanCommute)) %>%
 group_by(State, County) %>%
 summarise(Missing = 1)

## Plot1: Unemployment Rate by Counties
p1 <- ggplot(data = county, 
              aes(long, lat, group = group)) + 
  geom_polygon(data = asc_merged, 
               aes(fill = Unemployment),
               color = "gray") + 
  coord_map() +
  scale_fill_distiller(palette = "RdYlGn", 
                       labels = function(l) {paste0(l,"%")},
                       breaks = seq(0,30,10),
                       limits = c(0,30)) +
  theme_void() +
  theme(
    plot.margin = grid::unit(c(9,16,9,16), "mm"),
    aspect.ratio = 9/16,
    text = element_text(family = "Montserrat"),
    legend.position = c('.89', '.25'),
    legend.key.size = unit(1.1, 'cm'),
    panel.background = element_blank(),
    plot.title = element_markdown(family = "Lobster", colour = "deeppink3", hjust = 0.5, size = 40),
    plot.caption = element_markdown(colour = "dodgerblue4", size = 21, hjust = 1),
    legend.text = element_text(size = 18),
    legend.title = element_blank()
  ) +
  labs(title = "US: Unemployment Rate by Counties in <i>2015</i>", x = NULL, y = NULL, 
       caption = "<span>Graphic by Can Aytöre <b>&middot;</b> Data from <i>census.gov</i></span>")

ggsave("us_census_unemployment.png", p1, width = 1920, height = 1080, units = "px", dpi = 300, device = "png")


## Plot2: Mean Commute Time by States
state_highest <- asc %>%
  group_by(State) %>%
  summarise(MeanCommute = sum(MeanCommute * TotalPop)/sum(TotalPop)) %>%
  top_n(1, MeanCommute) %>%
  unlist() %>%
  .[1]

state_lowest <- asc %>%
  group_by(State) %>%
  summarise(MeanCommute = sum(MeanCommute * TotalPop)/sum(TotalPop)) %>%
  top_n(-1, MeanCommute) %>%
  unlist() %>%
  .[1]

p2 <- ggplot(data = asc %>%
               group_by(State) %>%
               summarise(MeanCommute = sum(MeanCommute * TotalPop)/sum(TotalPop)) %>%
               mutate(State = tolower(State)), 
             aes(map_id = State)) +
  geom_map(aes(fill = MeanCommute), map = fifty_states) +
  coord_map() + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  geom_label(aes(label = state_highest, 
                 x = mean(county[county$State == state_highest,]$long), 
                 y = mean(county[county$State == state_highest,]$lat)), 
             size = 7, 
             label.size = .5,
             fill = "red3",
             color = "white") +
  geom_label(aes(label = state_lowest, 
                 x = mean(county[county$State == state_lowest,]$long), 
                 y = mean(county[county$State == state_lowest,]$lat)), 
             size = 7, 
             label.size = .5,
             fill = "green4",
             color = "white") +
  scale_fill_distiller(name = "Minutes", 
                       palette = "RdYlGn") +
  theme_void() +
  theme(
    plot.margin = grid::unit(c(9,16,9,16), "mm"),
    aspect.ratio = 9/16,
    text = element_text(family = "Montserrat"),
    legend.position = c('.9', '.3'),
    legend.key.size = unit(1.1, 'cm'),
    panel.background = element_blank(),
    plot.title = element_markdown(family = "Lobster", colour = "blue4", hjust = .5, size = 40),
    plot.caption = element_markdown(colour = "dodgerblue4", size = 21, hjust = 1),
    legend.text = element_text(size = 18),
    legend.title = element_text(face = "italic", size = 22)
  ) +
  labs(title = "Mean Commute Time by States in <i>2015</i>",
       caption = "<span>Graphic by Can Aytöre <b>&middot;</b> Data from <i>census.gov</i></span>")

ggsave("us_census_commutetime.png", p2, width = 1920, height = 1080, units = "px", dpi = 300, device = "png")


## Plot3: Working at Home

library(geojsonio)
library(broom)
library(rgeos)
spdf <- geojson_read("_R/2018/2018-04-30-Week5/us_states_hexgrid.geojson", what = "sp") #https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map
spdf@data <- spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name") #to turn on the license, see https://stackoverflow.com/questions/30790036/error-istruegpclibpermitstatus-is-not-true
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid = TRUE), id = spdf@data$iso3166_2))
workathome <- asc %>% 
  group_by(State) %>%
  summarise(WorkAtHome = sum(WorkAtHome * TotalPop)/sum(TotalPop))
spdf_fortified <- left_join(spdf_fortified, workathome, by = c("id" = "State")) 
spdf_fortified$bin <- cut(spdf_fortified$WorkAtHome,
                          breaks = c(seq(2, 7, by = 1), Inf),
                          labels = c(seq(2, 7, by = 1)),
                          include.lowest = TRUE)

p3 <- ggplot() +
  geom_polygon(data = spdf_fortified, 
               aes(fill = bin, x = long, y = lat, group = group)) +
  geom_text(data = centers, 
            aes(x = x, y = y, label = id), 
            color = "black", size = 8, alpha = 0.6) +
  coord_map() +
  scale_fill_manual(values = c("#B2182B", "#D6604D", "#FDDBC7", "#92C5DE", "#2166AC"), 
                    name = "(in percent)", 
                    guide = guide_legend(keyheight = unit(3, units = "mm"), 
                                         keywidth = unit(12, units = "mm"), 
                                         label.position = "bottom", 
                                         title.position = 'top', nrow=1)) +
  theme_void() +
  theme(
    plot.margin = grid::unit(c(9,16,9,16), "mm"),
    aspect.ratio = 9/16,
    text = element_text(family = "Montserrat"),
    legend.position = c(.5, .86),
    panel.background = element_blank(),
    plot.title = element_text(family = "Lobster", colour = "antiquewhite4", hjust = .5, size = 48),
    plot.subtitle = element_markdown(family = "Lobster", colour = "antiquewhite4", size = 32, hjust = .64),
    plot.caption = element_markdown(colour = "dodgerblue4", size = 21, hjust = 1),
    legend.text = element_text(size = 24),
    legend.title = element_text(face = "italic", size = 20)
  ) +
  labs(title = "US: Working at home",
       subtitle = "<i>in 2015</i>",
       caption = "<span>Graphic by Can Aytöre <b>&middot;</b> Data from <i>census.gov</i></span>")

ggsave("us_census_workingathome.png", p3, width = 1920, height = 1080, units = "px", dpi = 300, device = "png")


## Plot4: Income per Capita

library(choroplethr)
library(choroplethrMaps)

incomepercap <- asc %>%
  select(CensusId, IncomePerCap) %>%
  rename(region = CensusId, value = IncomePerCap)

county_choropleth(incomepercap,
                  legend = "in dollars in 2015",
                  state_zoom = "california") +
  theme(
    plot.margin = grid::unit(c(4.5,8,4.5,8), "mm"),
    text = element_text(family = "Montserrat"),
    legend.position = c(0.74, 0.78),
    panel.background = element_blank(),
    plot.title = element_markdown(family = "Lobster", colour = "deepskyblue4", hjust = .5, size = 48),
    plot.caption = element_markdown(colour = "dodgerblue4", size = 21),
    legend.text = element_text(size = 17),
    legend.title = element_text(size = 21, face = "italic", hjust = .1)
  ) +
  labs(title = "Income per Capita in <i>California</i>",
       caption = "<span>Graphic by Can Aytöre<b>&middot;</b>Data from <i>census.gov</i></span>")


## Plot5: Gender distribution

totalpop <- asc %>%
  group_by(State) %>%
  summarise(TotalPop = sum(TotalPop))

gender_filtered <- asc %>%
  gather(key = Gender, value = Pop, c("Men","Women")) %>%
  select(State, Gender, Pop) %>%
  group_by(State, Gender) %>%
  summarise(Pop = sum(Pop)) %>%
  ungroup() %>%
  left_join(totalpop, by = 'State') %>%
  mutate(Pop = ifelse(Gender == "Men", -Pop, Pop))

p5 <- ggplot(data = gender_filtered, 
       aes(x = Pop, y = fct_reorder(State, desc(TotalPop)), fill = Gender)) + 
  geom_col() +
  scale_x_continuous(labels = function(l) {ifelse(l!=0,paste0(abs(round(l/1e6,0)),"M"),"")},
                     breaks = seq(-40000000,40000000,5000000)) +
  scale_fill_manual(values = c("darkorange", "deeppink")) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    plot.margin = grid::unit(c(9,16,9,16), "mm"),
    aspect.ratio = 9/16,
    text = element_text(family = "Montserrat"),
    axis.text.x = element_text(size = 16),
    legend.position = c(.5, .1),
    legend.box.background = element_rect(color = "azure4", size = 2),
    panel.background = element_blank(),
    plot.title = element_markdown(family = "Lobster", colour = "cadetblue4", hjust = .5, size = 36),
    plot.subtitle = element_markdown(family = "Lobster", colour = "cadetblue4", hjust = .67, size = 28),
    plot.caption = element_markdown(colour = "dodgerblue4", size = 21, hjust = 1),
    legend.text = element_text(size = 20),
    legend.title = element_blank()
  ) +
  labs(title = "Gender Distribution by States",
       subtitle = "<i>US Census '15</i>",
       caption = "<br><span>Graphic by Can Aytöre<b>&middot;</b>Data from <i>census.gov</i></span>")

 
ggsave("us_census_genderdist.png", p5, width = 1920, height = 1080, units = "px", dpi = 300, device = "png")

