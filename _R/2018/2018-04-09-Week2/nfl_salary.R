# Required libraries
library(tidyverse)
library(lubridate)
library(gganimate)
library(ggridges)
library(ggtext)
library(ggimage)
library(patchwork)
library(magick)

# library(showtext)
# Font families
windowsFonts("Lobster" = windowsFont("Lobster"),
             "Montserrat" = windowsFont("Montserrat"))

# Read the dataset from TidyTuesday Github repository
tf = tempfile(fileext = ".xlsx")
curl::curl_download("https://github.com/rfordatascience/tidytuesday/raw/master/data/2018/2018-04-09/nfl_salary.xlsx", tf)
nfl <- readxl::read_excel(tf)

nfl <- nfl %>% 
  gather(key = position, value = salary, -year) %>%
  mutate(salary = salary / 10**6,
         year = year(as.Date.character(year, format = "%Y")),
         status = case_when(
           position %in% 
             c("Cornerback", 
               "Defensive Lineman", 
               "Linebacker", 
               "Safety",
               "Special Teamer"
             ) ~ "Defense",
           position %in%
             c("Quarterback",
               "Offensive Lineman",
               "Running Back",
               "Tight End",
               "Wide Receiver"
             ) ~ "Offense")) %>%
  group_by(position, year) %>% 
  top_n(50, salary)

p1 <- ggplot(data = nfl %>% filter(status == "Defense"), 
             aes(x = salary, y = fct_reorder(position, salary, median), fill = position)) +
  geom_density_ridges(scale = .95, #less than 1 keeps ridges from overlapping
                    size = .25, 
                    na.rm = TRUE,  
                    alpha = .75,
                    rel_min_height = 0.05, #trims the tails
                    quantile_lines = TRUE,
                    show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 20, 5), 
                     limits = c(0, 20),
                     labels = function(l) {paste0("$",round(l,1),"M")} ) +
  scale_y_discrete(limits = rev) +
  transition_states(year,
                    transition_length = 3,
                    state_length = 1) +
  ease_aes('quadratic-in-out') +
  enter_fade() +
  exit_fade() +
  labs(title = "Salary distribution of the top",
       subtitle = "<br>Defensive Line", 
       x = NULL, 
       y = NULL, 
       caption = "<br><span>Graphic by Can Aytöre</span>") +
  ggthemes::theme_fivethirtyeight() +
  theme(
    text = element_text(family = "Montserrat"),
    legend.position = "none",
    plot.title = element_markdown(family = "Lobster", face = "bold", size = 48, hjust = 1),
    plot.subtitle = element_markdown(face = "italic", size = 28, colour = "azure4", hjust = .5),  
    plot.caption = element_markdown(colour = "dodgerblue4", size = 24, hjust = 1),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18))

p1_gif <- animate(p1, renderer=gifski_renderer(), width = 960, height = 1080, duration = length(unique(nfl$year)))


p2 <- ggplot(data = nfl %>% filter(status == "Offense"), 
             aes(x = salary, y = fct_reorder(position, salary, median), fill = position)) +
  geom_density_ridges(scale = .95, #less than 1 keeps ridges from overlapping
                      size = .25, 
                      na.rm = TRUE,  
                      alpha = .75,
                      rel_min_height = 0.05, #trims the tails
                      quantile_lines = TRUE,
                      show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0, 20, 5), 
                     limits= c(0, 20),
                     labels = function(l) {paste0("$",round(l,1),"M")} ) +
  scale_y_discrete(limits = rev, 
                   position ='right') +
  transition_states(year,
                    transition_length = 3,
                    state_length = 1) +
  ease_aes('quadratic-in-out') +
  enter_fade() +
  exit_fade() +
  geom_image(aes(x = 17.5, y = 5.3, 
                 image = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/16/American_football.svg/800px-American_football.svg.png"), 
             size = 0.08) +
  labs(title = "50 NFL players in {closest_state}", 
       subtitle = "<br>Offensive Line", 
       x = NULL, 
       y = NULL, 
       caption = "<br><span>Data from <i>spotrac.com</i></span>") +
  ggthemes::theme_fivethirtyeight() +
  theme(
    text = element_text(family = "Montserrat"),
    legend.position = "none",
    plot.title = element_text(family = "Lobster", face = "bold", size = 48, hjust = -0.04),
    plot.subtitle = element_markdown(face = "italic", size = 28, colour = "azure4", hjust = .5), 
    plot.caption = element_markdown(colour = "dodgerblue4", size = 24, hjust = 0),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18))

p2_gif <- animate(p2, renderer=gifski_renderer(), width = 960, height = 1080, duration = length(unique(nfl$year)))


p1_mgif <- image_read(p1_gif)
p2_mgif <- image_read(p2_gif)

new_gif <- image_append(c(p1_mgif[1], p2_mgif[1]))
for(i in 2:69){
  combined <- image_append(c(p1_mgif[i], p2_mgif[i]))
  new_gif <- c(new_gif, combined)
}
new_gif 
  

