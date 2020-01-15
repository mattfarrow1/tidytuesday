
# Setup -------------------------------------------------------------------

# load libraries
library(tidyverse)
library(scales)

# read data
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

# load data dictionary
url <- "https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-01-14/readme.md"
page <- xml2::read_html(url)
table <- rvest::html_table(page, fill = TRUE)
dictionary <- as_tibble(table[[1]])

# Plotting ----------------------------------------------------------------

passwords %>%
  group_by(category) %>%
  summarise(avg_strength = mean(strength)) %>%
  filter(!is.na(category)) %>%
  mutate(fill_flag = avg_strength > 10) %>%
  ggplot(aes(
    reorder(category, avg_strength),
    avg_strength,
    fill = fill_flag,
    label = number(avg_strength, accuracy = 0.1)
  )) +
  geom_col(color = "gray30") +
  geom_text(hjust = 1.5) +
  coord_flip() +
  labs(
    title = "Average Password Strength by Category",
    x = "Category",
    y = "Average Strength",
    caption = "source: Knowledge is Beautiful"
  ) +
  scale_fill_manual(values = c("gray90", "cornflowerblue")) +
  theme_minimal() +
  theme(legend.position = "none") +
  NULL
