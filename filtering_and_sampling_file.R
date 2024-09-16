#Load necessary libraries
library(mosaic)
library(dplyr)

#Import dataset
clean_data = read.csv("https://raw.githubusercontent.com/cc3569/Descriptive-Analytics_Visualization_and_Decision-Modeling-for-Business-Analytics/main/clean2019_2023.csv")

#filter data by year
data2019 = clean_data %>% filter(year == 2019)
data2020 = clean_data %>% filter(year == 2020)
data2021 = clean_data %>% filter(year == 2021)
data2022 = clean_data %>% filter(year == 2022)
data2023 = clean_data %>% filter(year == 2023)

#Takes samples for each year with 1000 observations (250 for each quarter)
sample_2019 = data2019 %>%
  group_by(quarter) %>%
  sample_n(250)
sample_2020 = data2020 %>%
  group_by(quarter) %>%
  sample_n(250)
sample_2021 = data2021 %>%
  group_by(quarter) %>%
  sample_n(250)
sample_2022 = data2022 %>%
  group_by(quarter) %>%
  sample_n(250)
sample_2023 = data2023 %>%
  group_by(quarter) %>%
  sample_n(250)

#Merge all samples into one dataframe
data_sample = 
  rbind(sample_2019, 
        sample_2020, 
        sample_2021, 
        sample_2022, 
        sample_2023)

#Write to CSV file
#The directory shown is my directory
#Modify code to fit your own directory
write.csv(data_sample, "C:/BAN 542/sample2019_2023.csv")