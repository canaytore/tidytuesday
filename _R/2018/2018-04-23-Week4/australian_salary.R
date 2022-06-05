# Required libraries
library(tidyverse)
library(janitor)
library(ggtext)
library(ggimage)

# library(showtext)
# Font families
windowsFonts("Lobster" = windowsFont("Lobster"),
             "Montserrat" = windowsFont("Montserrat"))

# Read the dataset from TidyTuesday Github repository
salary <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2018/2018-04-23/week4_australian_salary.csv")

salary <- separate(salary, "occupation", "occupation", sep = ";") 
salary <- separate(salary, "occupation", "occupation", sep = "\x96")

salary_highest <- as_tibble(salary) %>% 
  mutate(total_income = individuals * average_taxable_income) %>%
  group_by(occupation) %>%
  mutate(average_income = sum(total_income) / sum(individuals)) %>%
  ungroup() %>%
  top_n(50, average_income)


cols <- c("Female" = "plum3", "Male" = "dodgerblue3", "Mean" = "grey50")

p1 <- ggplot(salary_highest, 
       aes(x = fct_reorder(occupation, average_income, .desc = FALSE), 
           y = average_taxable_income)) +
  geom_point(data = salary_highest %>% filter(gender == "Male"), aes(color = "Male"), size = 4, alpha = 0.9) +
  geom_point(data = salary_highest %>% filter(gender == "Female"), aes(color = "Female"), size = 4, alpha = 0.9) +
  geom_point(aes(y = average_income, color = "Mean"), size = 3.5, alpha = 0.3, shape = 17) +
  geom_line(data = salary_highest %>% filter(gender == "Male"), aes(group = 1, color = "Male")) +
  geom_line(data = salary_highest %>% filter(gender == "Female"), aes(group = 1, color = "Female")) +
  coord_flip() +
  geom_image(aes(x = 6.8, y = 503500, 
                 image = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/88/Flag_of_Australia_%28converted%29.svg/1920px-Flag_of_Australia_%28converted%29.svg.png"), 
             size = 0.26) +
  scale_y_continuous(limits = c(100000, 600000), 
                     breaks = seq(100000, 600000, 100000), 
                     minor_breaks = NULL,
                     labels = function(l) {paste0("$",round(l/1000,1),"k")}) +
  scale_color_manual(name = "", values = cols) +
  labs(title = "Australia’s 50 highest paying jobs", x = NULL,
       y = "Average Taxable Income",
       caption = "<br><span>Graphic by Can Aytöre <b>&middot;</b> Data from <i>data.gov.au</i></span>") +
  ggthemes::theme_fivethirtyeight() +
  theme(
    plot.margin = grid::unit(c(9,16,9,16), "mm"),
    aspect.ratio = 9/16,
    text = element_text(family = "Montserrat"),
    legend.position = c(.78, .16),
    legend.text = element_text(size = 18),
    legend.box.background = element_rect(color = "azure4", size = 2),
    plot.title = element_text(family = "Lobster", colour = "cornflowerblue", hjust = 0.4, size = 48),
    plot.subtitle = element_markdown(size = 24),
    plot.caption = element_markdown(colour = "dodgerblue4", size = 24),
    axis.text = element_text(size = 16)
  )

ggsave("australian_salary.png", p1, width = 1920, height = 1080, units = "px", dpi = 300, device = "png")


