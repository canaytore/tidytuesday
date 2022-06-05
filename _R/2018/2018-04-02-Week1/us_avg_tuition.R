# Required libraries
library(tidyverse)
library(gganimate)
library(ggtext)
library(fiftystater)

# library(showtext)
# Font families
windowsFonts("Lobster" = windowsFont("Lobster"),
             "Montserrat" = windowsFont("Montserrat"))

# Read the dataset from TidyTuesday Github repository
tf = tempfile(fileext = ".xlsx")
curl::curl_download("https://github.com/rfordatascience/tidytuesday/raw/master/data/2018/2018-04-02/us_avg_tuition.xlsx", tf)
tuition <- readxl::read_excel(tf)

# Data Source: 
# https://onlinembapage.com/average-tuition-and-educational-attainment-in-the-united-states/

glimpse(tuition)
summary(tuition)

# Plot 1

tuition_byyear <- tuition %>%
  mutate(id = tolower(State)) %>%
  select(-State) %>%
  gather(key = year, value = cost, -id)
glimpse(tuition_byyear)

p1_subtitle <- "<br>One of the top concerns in the political arena today is student debt. That’s regardless of political affiliation, and 
<br>it’s a consistent concern for millennials across the board, with very few exceptions. It seems that no one is happy 
<br>about this, but there is very little one can currently do about it. People have claimed to owe as much as $44,000 
<br>in loans for a public school, but one usually assumes that those are select cases. 
<br><br>How much do Americans really pay for public college education? How much has tuition increased over the years?"

p1 <- ggplot(tuition_byyear, aes(frame = year, map_id = id, group = interaction(year, id))) +
  # map_id creates the aesthetic mapping to the state name column in your data
  geom_map(aes(fill = cost), color = "azure4", map = fifty_states) +
  # map points to the fifty_states shape data
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL, labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  labs(title = "Average Tuition {closest_state} in US", x = NULL, y = NULL, 
       subtitle = p1_subtitle,
       caption = "<span>Graphic by Can Aytöre <b>&middot;</b> Data from <i>onlinembapage.com</i></span>") +
  transition_states(year, transition_length = 1, state_length = 4) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    text = element_text(family = "Montserrat"),
    legend.position = c('.79', '.92'),
    legend.box.background = element_rect(color = "green4", size = 1),
    panel.background = element_blank(),
    plot.title = element_markdown(family = "Lobster", hjust = 0.5, size = 48),
    plot.subtitle = element_markdown(size = 24),
    plot.caption = element_markdown(colour = "dodgerblue4", size = 24),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18)
  ) +
  guides(fill = guide_legend(title = "Tuition Cost ($/year):")) +
  scale_fill_gradient(low = "#f7fcf5", high = "#005a32")

# set animation interval as 1 second by adjusting duration parameter
animate(p1, renderer=gifski_renderer(), width = 1920, height = 1080, duration = length(unique(tuition_byyear$year)))
anim_save("us_tuition.gif")


#Plot 2

tuition_cagr <- tuition %>%
  mutate(id = tolower(State)) %>%
  mutate(cagr = ((`2015-16`/`2004-05`)**(1/11)-1)*100) %>%
  select(id, cagr)
  
p2 <- ggplot(tuition_cagr, aes(map_id = id)) +
  geom_map(aes(fill = cagr), color = "azure4", map = fifty_states) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(title = "Average annual change in Tuition Fee in the US from 2004 to 2015", x = NULL, y = NULL,
       caption = "<span> Graphic by Can Aytöre <b>&middot;</b> Data from <i>onlinembapage.com</i></span>") +
  geom_text(x = -68.5, y = 31.5, label = "CAGR (%)", size = 8) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    text = element_text(family = "Montserrat"),
    legend.position = c('.88', '.2'),
    legend.key.size = unit(1.5, 'cm'),
    panel.background = element_blank(),
    plot.title = element_text(family = "Lobster", hjust = 0.5, size = 32),
    plot.caption = element_markdown(colour = "dodgerblue4", size = 22),
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 24)
  ) +
  scale_fill_continuous(name = "", low = "#f7fcf5", high = "#005a32")

# ggsave("us_tuition_cagr.png", p2, width = 1920, height = 1080, units = "px", dpi = 300, device = "png")

