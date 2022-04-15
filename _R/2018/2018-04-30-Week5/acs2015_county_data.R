# Required libraries
library(tidyverse)
library(ggtext)
library(maps)
library(fiftystater)

# Read the dataset from TidyTuesday Github repository
asc <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2018/2018-04-30/week5_acs2015_county_data.csv")

county <- map_data("county") %>%
  as_tibble() %>%
  rename(State = region, County = subregion) %>%
  mutate_at(c("State", "County"), .funs=str_to_title) %>%
  mutate(County = str_replace(County, "Dona Ana", "Doña Ana")) 

asc_merged <- left_join(county, asc, by = c("State", "County"))

#Missing state&counties;
#asc %>%
#  filter(is.na(MeanCommute)) %>%
#  group_by(State, County) %>%
#  summarise(Missing = 1)

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
  theme(plot.title = element_text(size = 14, hjust = .5),
        plot.subtitle = element_markdown(size = 8, hjust = .95),
        plot.caption = element_markdown(size = 10, hjust = .9),
        legend.title = element_blank()) +
  labs(title = "US: Unemployment Rate by Counties", 
       subtitle = "<i>Created on: 04/05/2018</i>",
       caption = "<span style='color: blue4;'> Made by Can Aytöre <b>&middot;</b> Data from <i>census.gov</i></span>")

ggsave("us_census_unemployment.png", p1, dpi = 600)

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
             size = 3, 
             label.size = .4,
             fill = "red3",
             color = "white") +
  geom_label(aes(label = state_lowest, 
                 x = mean(county[county$State == state_lowest,]$long), 
                 y = mean(county[county$State == state_lowest,]$lat)), 
             size = 3, 
             label.size = .4,
             fill = "green4",
             color = "white") +
  scale_fill_distiller(name = "Minutes", 
                       palette = "RdYlGn") +
  theme_void() +
  theme(plot.title = element_text(size = 14, hjust = .5),
        plot.subtitle = element_markdown(size = 8, hjust = .95),
        plot.caption = element_markdown(size = 10, hjust = .9),
        legend.title = element_text(face = "italic", size = 10)) +
  labs(title = "US: Mean Commute Time by States", 
       subtitle = "<i>Created on: 04/05/2018</i>",
       caption = "<span style='color: blue4;'> Made by Can Aytöre <b>&middot;</b> Data from <i>census.gov</i></span>")

ggsave("us_census_commutetime.png", p2, dpi = 600)

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
            color = "black", size = 5, alpha = 0.6) +
  coord_map() +
  scale_fill_manual(values = c("#B2182B", "#D6604D", "#FDDBC7", "#92C5DE", "#2166AC"), 
                    name = "(in percent)", 
                    guide = guide_legend(keyheight = unit(3, units = "mm"), 
                                         keywidth = unit(12, units = "mm"), 
                                         label.position = "bottom", 
                                         title.position = 'top', nrow=1)) +
  theme_void() +
  theme(plot.title = element_text(size = 14, hjust = .5),
        plot.subtitle = element_markdown(size = 8, hjust = .95),
        plot.caption = element_markdown(size = 10, hjust = .9),
        legend.position = c(0.5, 0.9)) +
  labs(title = "US: Working at home", 
       subtitle = "<i>Created on: 04/05/2018</i>",
       caption = "<span style='color: blue4;'> Made by Can Aytöre <b>&middot;</b> Data from <i>census.gov</i></span>")

ggsave("us_census_workingathome.png", p3, dpi = 600)

## Plot4: Income per Capita

library(choroplethr)
library(choroplethrMaps)

incomepercap <- asc %>%
  select(CensusId, IncomePerCap) %>%
  rename(region = CensusId, value = IncomePerCap)

p4 <- county_choropleth(incomepercap,
                  title = "Income per Capita in California",
                  legend = "(in dollars)",
                  state_zoom = "california") +
  theme(plot.title = element_text(size = 14, hjust = .5),
        plot.subtitle = element_markdown(size = 8, hjust = .95),
        plot.caption = element_markdown(size = 10, hjust = .9),
        legend.title = element_text(face = "italic", hjust = .5),
        legend.position = c(0.8, 0.76)) +
  labs(subtitle = "<i>Created on: 04/05/2018</i>", 
       caption = "<span style='color: blue4;'> Made by Can Aytöre <b>&middot;</b> Data from <i>census.gov</i></span>")

ggsave("us_census_incomepercap_cal.png", p4, dpi = 600)

## Plot5: Gender distribution

gender_filtered <- asc %>%
  gather(key = Gender, value = Pop, c("Men","Women")) %>%
  select(State, Gender, Pop) %>%
  left_join(asc %>% select(State, TotalPop)) %>%
  group_by(State, Gender) %>%
  summarise(Pop = sum(Pop),
            TotalPop = sum(TotalPop)) %>%
  mutate(Pop = ifelse(Gender == "Men", -Pop, Pop))

p5 <- ggplot(data = gender_filtered, 
       aes(x = Pop, y = fct_reorder(State, desc(TotalPop)), fill = Gender)) + 
  geom_col() +
  scale_x_continuous(labels = function(l) {ifelse(l!=0,paste0(round(l/1e6,0),"M"),"")},
                     breaks = seq(-4000000000,4000000000,1000000000)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, hjust = .5),
        plot.subtitle = element_markdown(size = 8, hjust = .95),
        plot.caption = element_markdown(size = 10, hjust = .9),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 7, hjust = 1),
        legend.title = element_blank(),
        legend.position = "top") +
  labs(title = "US Census: Gender Distribution by States", 
       subtitle = "<i>Created on: 04/05/2018</i>", 
       caption = "<span style='color: blue4;'> Made by Can Aytöre <b>&middot;</b> Data from <i>census.gov</i></span>")

ggsave("us_census_genderdist.png", p5, dpi = 600)
