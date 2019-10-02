library(dplyr)
library(ggplot2)
library(knitr)
library(utils)
library(tidytext)
library(textdata)
library(ggplot2)
load("AmazonFinal2.RData")

# Lew's file shows us how to put a score on each important word
mydata <- inner_join(Amazon2A, Amazon2B, by = "review_id") %>%
  select(-c(customer_id.y))
# mydata <- mydata[, -9] 
colnames(mydata)[1] <- "customer_id"
print(summary(mydata))

unique(mydata$customer_id) %>% length() 
# 47,933 unique customer id's. This means some customers have reviewed several different books

# Question 1A
mydata %>% group_by(product_parent) %>% filter(n() >= 50) %>%
  summarise(mean = mean(star_rating), count = n()) %>%
  arrange(desc(mean))

# Books with 50+ reviews
fiftyplus <- mydata %>% group_by(product_parent) %>% 
  filter(n() >= 50)
fiftyplus <- fiftyplus %>% group_by(product_parent) %>% 
  mutate(avgRating = mean(star_rating))

View(fiftyplus %>% group_by(product_parent) %>% 
       arrange(desc(avgRating)))
View(fiftyplus %>% group_by(product_parent) %>%
       arrange(avgRating))

# Highest Rated Books with >= 50 Reviews
fiftyplus %>% group_by(product_parent, avgRating) %>% 
  summarise(count = n()) %>% 
  arrange(desc(avgRating))
# Lowest Rated Books with >= 50 Reviews
fiftyplus %>% group_by(product_parent, avgRating) %>% 
  summarise(count = n()) %>% 
  arrange(avgRating)

dataWithYears <- as.Date(mydata$review_date, "%Y/%m/%d")
mydata$year <- as.numeric(format(dataWithYears, "%Y"))