#Load necessary libraries
library(ggplot2)
library(maps)
library(dplyr)

#Import data
df = read.csv("https://raw.githubusercontent.com/cc3569/Descriptive-Analytics_Visualization_and_Decision-Modeling-for-Business-Analytics/main/clean2019_2023.csv")

#Initializes state map data
states_map = map_data("state")

### 2019 ANALYSIS - PRIOR TO COVID19 PANDEMIC ###

#Creates a dataset averaging fares by state in 2019
#Adds a column of lowercase state names using state abbreviations
avgfare_data_2019 = df %>%
  group_by(statecode1) %>%
  filter(year == 2019 & quarter == 4) %>%
  summarize(avg = mean(average_fare)) %>%
  mutate(region = tolower(state.name[match(statecode1, state.abb)]))

#Joins state map data to state aggregated average fares
map_data_2019 = states_map %>%
  left_join(avgfare_data_2019, by = "region")

#Creates a map plot for 2019 state average fares
ggplot(map_data_2019, aes(x = long, y = lat, group = group, fill = avg)) +
  geom_polygon(color = "black") +
  coord_fixed(1.3) +
  labs(fill = "Average Fares", title = "2019 Q4 Average Fares by State",
       subtitle = "Source: U.S. Department of Transportation Consumer Airfare Report",
       caption = "Analysis and Visualization by Cole T. Catron & Ella Buxton, 2024") +
  theme_void() +
  scale_fill_gradient(low = "lightblue", high = "navy", na.value = "gray")

### 2020 ANALYSIS - START OF COVID19 PANDEMIC

#Creates a dataset averaging fares by state in 2020
#Adds a column of lowercase state names using state abbreviations
avgfare_data_2020 = df %>%
  group_by(statecode1) %>%
  filter(year == 2020 & quarter == 1) %>%
  summarize(avg = mean(average_fare)) %>%
  mutate(region = tolower(state.name[match(statecode1, state.abb)]))

#Joins state map data to state aggregated average fares
map_data_2020 = states_map %>%
  left_join(avgfare_data_2020, by = "region")

#Creates a map plot for 2020 state average fares
ggplot(map_data_2020, aes(x = long, y = lat, group = group, fill = avg)) +
  geom_polygon(color = "black") +
  coord_fixed(1.3) +
  labs(fill = "Average Fares", title = "2020 Q1 Average Fares by State",
       subtitle = "Source: U.S. Department of Transportation Consumer Airfare Report",
       caption = "Analysis and Visualization by Cole T. Catron & Ella Buxton, 2024") +
  theme_void() +
  scale_fill_gradient(low = "lightblue", high = "navy", na.value = "gray")

### 2023 ANALYSIS - POST-COVID19 PANDEMIC###

#Creates a dataset averaging fares by state in 2023
#Adds a column of lowercase state names using state abbreviations
avgfare_data_2023 = df %>%
  group_by(statecode1) %>%
  filter(year == 2023 & quarter == 2) %>%
  summarize(avg = mean(average_fare)) %>%
  mutate(region = tolower(state.name[match(statecode1, state.abb)]))

#Joins state map data to state aggregated average fares
map_data_2023 = states_map %>%
  left_join(avgfare_data_2023, by = "region")

#Creates a map plot for 2023 state average fares
ggplot(map_data_2023, aes(x = long, y = lat, group = group, fill = avg)) +
  geom_polygon(color = "black") +
  coord_fixed(1.3) +
  labs(fill = "Average Fares", title = "2023 Q2 Average Fares by State",
       subtitle = "Source: U.S. Department of Transportation Consumer Airfare Report",
       caption = "Analysis and Visualizations by Cole T. Catron & Ella Buxton, 2024") +
  theme_void() +
  scale_fill_gradient(low = "lightblue", high = "navy", na.value = "gray")
