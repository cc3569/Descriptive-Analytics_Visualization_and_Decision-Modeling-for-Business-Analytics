#Necessary libraries
library(dplyr)
library(mosaic)

#Import data
data = read.csv("https://raw.githubusercontent.com/cc3569/Descriptive-Analytics_Visualization_and_Decision-Modeling-for-Business-Analytics/main/clean2019_2023.csv")

### QUANTITATIVE VARIABLE ANALYSIS - YEAR AND QUARTER ###

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
    mean.miles = mean(nsmiles, na.rm = TRUE),
    median.miles = median(nsmiles, na.rm = TRUE),
    sd.miles = sd(nsmiles, na.rm = TRUE),
    min.miles = min(nsmiles, na.rm = TRUE),
    max.miles = max(nsmiles, na.rm = TRUE),
    iqr.miles = iqr(nsmiles, na.rm = TRUE),
    q1.miles = quantile(nsmiles, 0.25, na.rm = TRUE),
    q3.miles = quantile(nsmiles, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

#Descriptive statistics for passengers per day
desc.stats.passengers = data %>%
  group_by(year, quarter) %>%
  summarize(
    mean.passengers = mean(passengers, na.rm = TRUE),
    median.passengers = median(passengers, na.rm = TRUE),
    sd.passengers = sd(passengers, na.rm = TRUE),
    min.passengers = min(passengers, na.rm = TRUE),
    max.passengers = max(passengers, na.rm = TRUE),
    iqr.passengers = iqr(passengers, na.rm = TRUE),
    q1.passengers = quantile(passengers, 0.25, na.rm = TRUE),
    q3.passengers = quantile(passengers, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

#Descriptive statistics for primary carrier market share
desc.stats.marketshare = data %>%
  group_by(year, quarter) %>%
  summarize(
    mean.marketshare = mean(mkt_share, na.rm = TRUE),
    median.marketshare = median(mkt_share, na.rm = TRUE),
    sd.marketshare = sd(mkt_share, na.rm = TRUE),
    min.marketshare = min(mkt_share, na.rm = TRUE),
    max.marketshare = max(mkt_share, na.rm = TRUE),
    iqr.marketshare = iqr(mkt_share, na.rm = TRUE),
    q1.marketshare = quantile(mkt_share, 0.25, na.rm = TRUE),
    q3.marketshare = quantile(mkt_share, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

### QUANTITATIVE VARIABLE ANALYSIS - YEAR, QUARTER, AND STATE OF DEPARTURE CITY ###

#Descriptive statistics for average fares
desc.stats.fare2 = data %>%
  group_by(year, quarter, statecode1) %>%
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
desc.stats.miles2 = data %>%
  group_by(year, quarter, statecode1) %>%
  summarize(
    mean.miles = mean(nsmiles, na.rm = TRUE),
    median.miles = median(nsmiles, na.rm = TRUE),
    sd.miles = sd(nsmiles, na.rm = TRUE),
    min.miles = min(nsmiles, na.rm = TRUE),
    max.miles = max(nsmiles, na.rm = TRUE),
    iqr.miles = iqr(nsmiles, na.rm = TRUE),
    q1.miles = quantile(nsmiles, 0.25, na.rm = TRUE),
    q3.miles = quantile(nsmiles, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

#Descriptive statistics for passengers per day
desc.stats.passengers2 = data %>%
  group_by(year, quarter, statecode1) %>%
  summarize(
    mean.passengers = mean(passengers, na.rm = TRUE),
    median.passengers = median(passengers, na.rm = TRUE),
    sd.passengers = sd(passengers, na.rm = TRUE),
    min.passengers = min(passengers, na.rm = TRUE),
    max.passengers = max(passengers, na.rm = TRUE),
    iqr.passengers = iqr(passengers, na.rm = TRUE),
    q1.passengers = quantile(passengers, 0.25, na.rm = TRUE),
    q3.passengers = quantile(passengers, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

#Descriptive statistics for primary carrier market share
desc.stats.marketshare2 = data %>%
  group_by(year, quarter, statecode1) %>%
  summarize(
    mean.marketshare = mean(mkt_share, na.rm = TRUE),
    median.marketshare = median(mkt_share, na.rm = TRUE),
    sd.marketshare = sd(mkt_share, na.rm = TRUE),
    min.marketshare = min(mkt_share, na.rm = TRUE),
    max.marketshare = max(mkt_share, na.rm = TRUE),
    iqr.marketshare = iqr(mkt_share, na.rm = TRUE),
    q1.marketshare = quantile(mkt_share, 0.25, na.rm = TRUE),
    q3.marketshare = quantile(mkt_share, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

### QUANTITATIVE VARIABLE ANALYSIS - YEAR, QUARTER, AND PRIMARY AIRLINE ###

#Descriptive statistics for average fares
desc.stats.fare3 = data %>%
  group_by(year, quarter, lms_carrier_name) %>%
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

#Remove nulls
desc.stats.fare3 = na.omit(desc.stats.fare3)

#Descriptive statistics for nonstop market miles
desc.stats.miles3 = data %>%
  group_by(year, quarter, lms_carrier_name) %>%
  summarize(
    mean.miles = mean(nsmiles, na.rm = TRUE),
    median.miles = median(nsmiles, na.rm = TRUE),
    sd.miles = sd(nsmiles, na.rm = TRUE),
    min.miles = min(nsmiles, na.rm = TRUE),
    max.miles = max(nsmiles, na.rm = TRUE),
    iqr.miles = iqr(nsmiles, na.rm = TRUE),
    q1.miles = quantile(nsmiles, 0.25, na.rm = TRUE),
    q3.miles = quantile(nsmiles, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

#Remove nulls
desc.stats.miles3 = na.omit(desc.stats.miles3)

#Descriptive statistics for passengers per day
desc.stats.passengers3 = data %>%
  group_by(year, quarter, lms_carrier_name) %>%
  summarize(
    mean.passengers = mean(passengers, na.rm = TRUE),
    median.passengers = median(passengers, na.rm = TRUE),
    sd.passengers = sd(passengers, na.rm = TRUE),
    min.passengers = min(passengers, na.rm = TRUE),
    max.passengers = max(passengers, na.rm = TRUE),
    iqr.passengers = iqr(passengers, na.rm = TRUE),
    q1.passengers = quantile(passengers, 0.25, na.rm = TRUE),
    q3.passengers = quantile(passengers, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

#Remove nulls
desc.stats.passengers3 = na.omit(desc.stats.passengers3)

#Descriptive statistics for primary carrier market share
desc.stats.marketshare3 = data %>%
  group_by(year, quarter, lms_carrier_name) %>%
  summarize(
    mean.marketshare = mean(mkt_share, na.rm = TRUE),
    median.marketshare = median(mkt_share, na.rm = TRUE),
    sd.marketshare = sd(mkt_share, na.rm = TRUE),
    min.marketshare = min(mkt_share, na.rm = TRUE),
    max.marketshare = max(mkt_share, na.rm = TRUE),
    iqr.marketshare = iqr(mkt_share, na.rm = TRUE),
    q1.marketshare = quantile(mkt_share, 0.25, na.rm = TRUE),
    q3.marketshare = quantile(mkt_share, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

#Remove nulls
desc.stats.marketshare3 = na.omit(desc.stats.marketshare3)
