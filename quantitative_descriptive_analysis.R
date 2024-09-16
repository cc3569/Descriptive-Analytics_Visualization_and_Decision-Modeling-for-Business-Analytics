#Necessary libraries
library(dplyr)
library(mosaic)

#Import data
data = read.csv("https://raw.githubusercontent.com/cc3569/Descriptive-Analytics_Visualization_and_Decision-Modeling-for-Business-Analytics/main/sample2019_2023.csv")

### QUANTITATIVE VARIABLE ANALYSIS ###

#Descriptive statistics for average fares
desc.stats.fare = data %>%
  group_by(year, quarter) %>%
  summarize(
    mean.fare = mean(average_fare, na.rm = TRUE),
    median.fare = median(average_fare, na.rm = TRUE),
    sd.fare = sd(average_fare, na.rm = TRUE),
    min.fare = min(average_fare, na.rm = TRUE),
    max.fare = max(average_fare, na.rm = TRUE),
    iqr.fare = iqr(average_fare, na.rm = TRUE),
    q1.fare = quantile(average_fare, 0.25, na.rm = TRUE),
    q3.fare = quantile(average_fare, 0.75, na.rm = TRUE),
    .groups = "drop"
    )

#Descriptive statistics for nonstop market miles
desc.stats.miles = data %>%
  group_by(year, quarter) %>%
  summarize(
    mean.fare = mean(nsmiles, na.rm = TRUE),
    median.fare = median(nsmiles, na.rm = TRUE),
    sd.fare = sd(nsmiles, na.rm = TRUE),
    min.fare = min(nsmiles, na.rm = TRUE),
    max.fare = max(nsmiles, na.rm = TRUE),
    iqr.fare = iqr(nsmiles, na.rm = TRUE),
    q1.fare = quantile(nsmiles, 0.25, na.rm = TRUE),
    q3.fare = quantile(nsmiles, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

#Descriptive statistics for passengers per day
desc.stats.passengers = data %>%
  group_by(year, quarter) %>%
  summarize(
    mean.fare = mean(passengers, na.rm = TRUE),
    median.fare = median(passengers, na.rm = TRUE),
    sd.fare = sd(passengers, na.rm = TRUE),
    min.fare = min(passengers, na.rm = TRUE),
    max.fare = max(passengers, na.rm = TRUE),
    iqr.fare = iqr(passengers, na.rm = TRUE),
    q1.fare = quantile(passengers, 0.25, na.rm = TRUE),
    q3.fare = quantile(passengers, 0.75, na.rm = TRUE),
    .groups = "drop"
  )