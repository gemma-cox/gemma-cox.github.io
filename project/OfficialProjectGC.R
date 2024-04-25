install.packages("tidyverse")
install.packages("maps")
install.packages("plotly")

library(tidyverse)
library(maps)
library(plotly)
library(ggplot2)

unicef_metadata <- read_csv("unicef_metadata.csv")
unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
data_right_3 <- read_csv("data_right_3.csv")

# exercise 1
data_join <- full_join(unicef_metadata, unicef_indicator_1)
data_join <- full_join(unicef_metadata, unicef_indicator_1, 
                       by = c("country", "year" = "time_period"))

# exercise 2
unicef_indicator_2$time_period <- as.numeric(unicef_indicator_2$time_period)
data_join <- full_join(unicef_metadata, unicef_indicator_2)
data_join <- full_join(unicef_metadata, unicef_indicator_2,
                       by = c("country", "year" = "time_period"))

# exercise 3
data_join <- full_join(unicef_metadata, data_right_3)
data_join <- full_join(unicef_metadata, data_right_3, by = c("country"))

# final data object
data_join <- unicef_metadata %>%
  full_join(unicef_indicator_1, 
            by = c("country", "year" = "time_period")) %>%
  full_join(unicef_indicator_2, 
            by = c("country", "year" = "time_period")) %>%
  full_join(data_right_3, by = c("country"))

  options(scipen = 999)

map_world <- map_data("world")

# world map :)

data_join_2018 <- data_join %>%
  filter(year == 2018)

map_data_join_2018 <- full_join(map_world, by = c("country" = "region"))

ggplot(map_data_join_2018) +
  aes(x = long, y = lat, group = group, fill = `Life expectancy at birth, total (years)`) +
  geom_polygon() + labs(x = "", y = "", title = 
  "A World Map revealing the life expectancy at birth in the year 2018") +
  theme_classic() +
  theme(text = element_text(family = "helvetica"))

# bar chart :)

data_join_bar <- full_join(data_right_3, unicef_indicator_1, by = c("country"))
ggplot(subset(data_join_bar, time_period %in% c("2016", "2017", "2018")), 
       aes(x=continent, y=obs_value)) + geom_bar(stat="identity", width=0.5) + 
  facet_wrap(~ time_period, nrow = 1) + scale_x_discrete(limits = 
       c("Africa", "Europe", "Asia", "Americas", "Oceania")) + scale_y_continuous(limits = 
       c(0,100)) + 
  labs(x = "", y = "People who have limited sanitation", title = 
         "The Percentage of people who have limited sanitation services between 2016-2018") +
  theme_classic() +
  theme(text = element_text(family = "helvetica")) + 
  scale_fill_manual(values = c("#00AFBB", "#4FB05E", "#DB245E", "#FFE800"))

# time series :)

timeseries_plot_1 <- full_join(data_right_3, unicef_indicator_1, by = c("country"))

timeseries_plot_1 %>% select(-time_period, -obs_value, -continent, -country)

ggplot(timeseries_plot_1) + aes(x = time_period, y = obs_value, group = country, color = continent) + 
  geom_line(size = 1.2) + scale_color_manual(values = c("#00AFBB", "#4FB05E", "#DB245E", 
  "#FFE800", "#FFC0CB")) + scale_y_continuous(limits = c(0,80)) + 
  theme_minimal() + labs(x = "", y = "those who have limited sanitation",
  title = "The Percentage of People who have limited sanitation services") + theme_classic() +
  theme(text = element_text(family = "helvetica"))

# scatter plot :)
scatter_plot_1 <- unicef_metadata %>%
  full_join(unicef_indicator_1, 
            by = c("country", "year" = "time_period")) %>%
  full_join(data_right_3, by = c("country"))

ggplot(scatter_plot_1) + aes(x = obs_value, 
  y = 'Life expectancy at birth, total (years)', 
  group = year, color = continent) + geom_point(alpha = 0.2) +
  facet_wrap(obs_value ~ year, nrow = 1) + labs(x = "People who have limited sanitation", 
  y = "Life Expectancy at birth, total (years)", title = "Evolution of the 
  relationship between Life expectancy and people whp have limited sanitation services") +
  guides(color = "none", size = "none") + theme_classic() + theme(text = element_text(family = "helvetica"))









