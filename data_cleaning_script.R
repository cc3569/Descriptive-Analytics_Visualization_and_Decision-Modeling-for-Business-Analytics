#imports mosaic library
library(mosaic)

#imports cleaned dataset for Consumer Airfare Report - Table 1 (2018 - Q1 2024)
d = read.csv("https://raw.githubusercontent.com/cc3569/Descriptive-Analytics_Visualization_and_Decision-Modeling-for-Business-Analytics/main/Consumer_Airfare_Report_Passenger_Table1_2019_2024q1.csv")

#Filters dataset for the year 2024 quarter 1
d_filter = d %>% filter(Year==2024 & quarter==1)

#Takes first sample of 250 observations and places it into a csv
set.seed(123)
d_sample = sample(d_filter, size = 250, replace = FALSE)
write.csv(d_sample, "C:/BAN 542/sample1_CAR_2024q1.csv")

#Takes second sample of 250 observations and places it into another csv
d_sample_2 = sample(d_filter, size = 250, replace = FALSE)
write.csv(d_sample_2, "C:/BAN 542/sample2_CAR_2024q1.csv")

#Takes third sample of 250 observations and places it into another csv
d_sample_3 = sample(d_filter, size = 250, replace = FALSE)
write.csv(d_sample_3, "C:/BAN 542/sample3_CAR_2024q1.csv")
