#Libraries used
library(mosaic)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

#Import data
desc.stats.fare = read.csv("https://raw.githubusercontent.com/cc3569/Descriptive-Analytics_Visualization_and_Decision-Modeling-for-Business-Analytics/main/desc_stats_fare.csv")
fred.cpi.data = read.csv("https://raw.githubusercontent.com/cc3569/Descriptive-Analytics_Visualization_and_Decision-Modeling-for-Business-Analytics/main/q4_2023_adjusted_cpi_data_2018_2023.csv")

desc.stats.miles = read.csv("https://raw.githubusercontent.com/cc3569/Descriptive-Analytics_Visualization_and_Decision-Modeling-for-Business-Analytics/main/desc_stats_miles.csv")
desc.stats.passengers = read.csv("https://raw.githubusercontent.com/cc3569/Descriptive-Analytics_Visualization_and_Decision-Modeling-for-Business-Analytics/main/desc_stats_passengers.csv")
desc.stats.marketshare = read.csv("https://raw.githubusercontent.com/cc3569/Descriptive-Analytics_Visualization_and_Decision-Modeling-for-Business-Analytics/main/desc_stats_marketshare.csv")


### Create time series data sets for visualizations ###

#Real average fare data
#Creates mean and median fields, adjusting figures for inflation
fare.time.data = desc.stats.fare %>%
  mutate(date = make_date(year) + months((quarter - 1) * 3),
         mean.real.fare = (mean.fare / fred.cpi.data$cpi) *100,
         median.real.fare = (median.fare / fred.cpi.data$cpi) *100)

#Nonstop market miles
miles.time.data = desc.stats.miles %>%
  mutate(date = make_date(year) + months((quarter - 1) *3))

#Passengers per day
#Rounds mean and median values to the nearest integer
passengers.time.data = desc.stats.passengers %>%
  mutate(date = make_date(year) + months((quarter - 1) *3),
         mean.passengers.rounded = round(mean.passengers, 0),
         median.passengers.rounded = round(median.passengers, 0))

#Market share for primary airlines for city pair
marketshare.time.data = desc.stats.marketshare %>%
  mutate(date = make_date(year) + months((quarter - 1) *3))

#Function used to create labels for Year and Quarter on X-axis for time series visualizations
quarter_label <- function(x) {
  year <- year(x)
  quarter <- (month(x) - 1) / 3 + 1
  paste0(year, " Q", quarter)
}

#Time series plot for real average fares by year and quarter
ggplot(fare.time.data) +
  geom_line(aes(x = date, y = mean.real.fare, color = "Mean"), size = 1.5) +
  geom_line(aes(x = date, y = median.real.fare, color = "Median"), size = 1.5) +
  scale_x_date(labels = quarter_label, breaks = fare.time.data$date) +
  scale_y_continuous(labels = label_dollar(accuracy = 1), limits = c(0, NA),
                     breaks = seq(0, 360, by = 20)) +
  theme_bw() +
  labs(title = "Mean and Median Real Average Fares by Year and Quarter",
       subtitle = "Source: Department of Transportation Consumer Airfare Report\n(In 2023 Q4 Inflation Adjusted U.S. Dollars)",
       caption = "Analysis and Visualization by Cole T. Catron & Ella Buxton, 2024",
       x = "Year & Quarter", y = "Mean/Median Average Fares (in U.S. Dollars ($))") +
  scale_color_manual(values = c("Mean" = "navyblue", "Median" = "darkorange"),  # Custom colors for the lines
                     name = "Legend")

#Time series plot for mean and median nonstop market miles by year and quarter
ggplot(miles.time.data) +
  geom_line(aes(x = date, y = mean.miles, color = "Mean"), size = 2) +
  geom_line(aes(x = date, y = median.miles, color = "Median"), size = 2) +
  scale_x_date(labels = quarter_label, breaks = miles.time.data$date) +
  scale_y_continuous(limits = c(0, NA),
                     breaks = seq(0, 1500, by = 250)) +
  theme_bw() +
  labs(title = "Mean and Median Nonstop Market Miles by Year and Quarter",
       subtitle = "Source: Department of Transportation Consumer Airfare Report",
       caption = "Analysis and Visualization by Cole T. Catron & Ella Buxton, 2024",
       x = "Year & Quarter", y = "Mean/Median Non-Stop Market Miles") +
  scale_color_manual(values = c("Mean" = "navyblue", "Median" = "hotpink"),  # Custom colors for the lines
                     name = "Legend")

#Time series plot for mean and median daily passenger loads by Year and Quarter
ggplot(passengers.time.data) +
  geom_line(aes(x = date, y = mean.passengers.rounded, color = "Mean"), size = 2) +
  geom_line(aes(x = date, y = median.passengers.rounded, color = "Median"), size = 2) +
  scale_x_date(labels = quarter_label, breaks = passengers.time.data$date) +
  scale_y_continuous(limits = c(0, NA),
                     breaks = seq(0, 900, by = 100)) +
  theme_bw() +
  labs(title = "Mean and Median Daily Passenger Load by Year and Quarter",
       subtitle = "Source: Department of Transportation Consumer Airfare Report",
       caption = "Analysis and Visualization by Cole T. Catron & Ella Buxton, 2024",
       x = "Year & Quarter", y = "Mean/Median Number of Passengers Per Day") +
  scale_color_manual(values = c("Mean" = "forestgreen", "Median" = "darkred"),  # Custom colors for the lines
                     name = "Legend")

#Time series plot for the mean and median primary carrier market share
ggplot(marketshare.time.data) +
  geom_line(aes(x = date, y = mean.marketshare, color = "Mean"), size = 2) +
  geom_line(aes(x = date, y = median.marketshare, color = "Median"), size = 2) +
  scale_x_date(labels = quarter_label, breaks = marketshare.time.data$date) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     limits = c(0, NA),
                     breaks = seq(0, 1, by = 0.1)) +
  theme_bw() +
  labs(title = "Mean and Median Market Share (Primary Airline Carrier) by Year and Quarter",
       subtitle = "Source: Department of Transportation Consumer Airfare Report",
       caption = "Analysis and Visualization by Cole T. Catron & Ella Buxton, 2024",
       x = "Year & Quarter", y = "Mean/Median Market Share For Primary Carrier (%)") +
  scale_color_manual(values = c("Mean" = "cyan", "Median" = "navyblue"),  # Custom colors for the lines
                     name = "Legend")