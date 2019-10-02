# 504914505_stat20_final.Rmd
# save(mydata, file = "Lab2.RData")
library(dplyr)
library(ggplot2)
library(knitr)
library(utils)
library(tidytext)
library(textdata)
library(extrafont)
library(ggplot2)
library(lubridate)
load("AmazonFinal2.RData")
save(mydata, vine_sentiment, noVines, all_review_counts, vineOnly,
     merged, fiftyplus, cuatro, cuatro_sentiment, file = "allRelevantObjects.RData")

# Lew's file shows us how to put a score on each important word
mydata <- inner_join(Amazon2A, Amazon2B, by = "review_id") %>%
  select(-c(customer_id.y))
mydata <- mydata %>% na.omit() # remove problematic rows 

colnames(mydata)[1] <- "customer_id"
print(summary(mydata))

mydata$RDateTime <- strptime(mydata$review_date, format = "%Y-%m-%d") %>% 
  as.POSIXct()
mydata$RYEAR <- year(mydata$RDateTime)
mydata$RWEEK <- week(mydata$RDateTime)
mydata$RMONTH <- month(mydata$RDateTime) %>% 
  factor(labels =  c("January","February", "March", "April", "May", 
                     "June", "July", "August", "September",
                     "October", "November", "December"))
mydata$RDOW <- weekdays(mydata$RDateTime)
mydata$RMONTH <- months(mydata$RDateTime)
mydata$RQUART <- quarters(mydata$RDateTime)
mydata$RDAY <- day(mydata$RDateTime)
mydata$WEEKDAY_NUM <- wday(mydata$RDateTime)
mydata <- mydata %>% group_by(review_date) %>% mutate(reviewsToday = n())
all_review_counts <- distinct(mydata, reviewsToday, .keep_all=TRUE) 

# Add a years variable
dataWithYears <- as.Date(mydata$review_date, "%Y/%m/%d")
mydata$year <- as.numeric(format(dataWithYears, "%Y"))

save(all_review_counts, file = "all_review_counts.RData")

# Question 1A
mydata <- mydata %>%
  group_by(product_title) %>%
  mutate(totalRevsOfThisBook = n())
mydata <- mydata %>% group_by(customer_id) %>%
  mutate(totalRevsByCustomer = n())
mydata <- mydata %>% group_by(product_title) %>%
  mutate(avgRating = mean(star_rating))

save(mydata, file = "mydata.RData")
# Books with 50+ reviews, create fiftyplus variable
fiftyplus <- mydata %>% group_by(product_title) %>% 
  filter(n() >= 50)

# Highest Rated Books with >= 50 Reviews
fiftyplus %>% group_by(product_title, avgRating) %>% 
  summarise(count = n()) %>% 
  arrange(desc(avgRating)) %>% head()
# Lowest Rated Books with >= 50 Reviews
fiftyplus %>% group_by(product_title, avgRating) %>% 
  summarise(count = n()) %>% 
  arrange(avgRating) %>% head()

# First and second best, then first and second worst rated
book_order <- c("Fearless: The Undaunted Courage and Ultimate Sacrifice of Navy SEAL Team SIX Operator Adam Brown",
                "Goodnight, Goodnight Construction Site",
                "It Could Happen To Anyone: Why Battered Women Stay",
                "To Train Up a Child")
# Isolate the 4 books
cuatro <- fiftyplus %>% 
  filter(product_title == book_order[1] || 
           product_title == book_order[2] ||
           product_title == book_order[3] ||
           product_title == book_order[4])
test <- cuatro %>%
  unnest_tokens(word, review_body) %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  mutate(contribution = value*n) %>%
  arrange(desc(abs(contribution)))
sentiment1 <- cuatro %>% unnest_tokens(word, review_body) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(review_id) %>% summarize(sentiment = mean(value), words = n())
# sentiment %>% arrange(desc(sentiment))
cuatro_sentiment <- left_join(sentiment1, cuatro, by = "review_id")
# 17 rows removed because review body wasn't enough to couldn't determine a sentiment score
cuatro_sentiment <- cuatro_sentiment %>% 
  mutate(ranking = ifelse(product_title == book_order[1] || product_title == book_order[2], "Highest", "Lowest"))

ordered_titles <- factor(cuatro_sentiment$product_title, book_order)

# Main 1B Answer (Edited)
violin <- ggplot(data = cuatro_sentiment, aes(y = sentiment, x = ordered_titles, fill = ordered_titles)) +
  geom_violin() + 
  ylim(c(-5, 5)) + 
  geom_boxplot(outlier.shape = NA, color="black", width = 0.2, position = position_dodge(width = 0.7))  +
  labs(title = "Sentiment Ranges of the Top 2 Best and Worst Books", y = "Sentiment Score", x = "Book Title", fill = "Rank") +
  scale_x_discrete(breaks = book_order, 
                   labels = c("Fearless: The Undaunted Courage\nand Ultimate Sacrifice of Navy SEAL\nTeam SIX Operator Adam Brown", 
                              "Goodnight, Goodnight\nConstruction Site",
                              "It Could Happen To Anyone:\nWhy Battered Women Stay",
                              "To Train Up \na Child")) +
  scale_fill_discrete(labels = c("1st Highest", "2nd Highest", "1st Lowest", "2nd Lowest")) + 
  theme(axis.text.x = element_text(size = 8))
plot(violin)

# jitter plot of the four books
ggplot(cuatro_sentiment, aes(y = words, x = sentiment, 
                             shape = product_title)) +
  aes(size = helpful_votes, show_guide = FALSE) +
  scale_size_continuous(range = c(3, 6), guide = FALSE) +
  geom_jitter(aes(color = star_rating)) + 
  scale_shape_manual(values=c(15, 16, 17, 18), labels = c("Fearless", "Goodnight", "It Could\nHappen To Anyone", "To Train Up\n a Child")) +
  labs(title = "Sentiment and Rating by Length of Review", subtitle = "Size of Points is Proportional to Helpful Votes",
       y = "Word Count", x = "Sentiment Score", shape = "Product", color = "Star Rating") +
  scale_color_gradient(low = "red", high = "green") +
  theme_classic()

# Question 2
summary(mydata %>% na.omit())
sentimentThis <- function(x) {
  result <- x %>% unnest_tokens(word, review_body) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(review_id) %>% summarize(sentiment = mean(value), words = n())
  result
}

noVines <- tibble() 
for (i in 2012:2015) {  
  annual_data <- mydata %>% filter(RYEAR == i & vine == "N")
  annual_sentiment <- sentimentThis(annual_data)
  annual_data <- left_join(annual_sentiment, annual_data, by = "review_id")
  noVines <- bind_rows(noVines, annual_data)
  }
noVines <- noVines %>% na.omit()

save(noVines, file = "vineless.RData")
load("vineless.RData")
noVines <- noVines

# Keep this below code
vineOnly <- mydata %>% filter(vine == "Y")
vine_scores <- vineOnly %>% unnest_tokens(word, review_body) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(review_id) %>% summarize(sentiment = mean(value), words = n())
vine_sentiment <- left_join(vine_scores, vineOnly)

save(vine_sentiment, file = "vine_sentiment.RData")

merged <- bind_rows(vine_sentiment, noVines)
save(merged, file = "merged.RData")
theme_set(theme_minimal(base_family = "Myanmar Text"))
cool_graph <- ggplot(merged, aes(x = sentiment, fill = vine)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.5, breaks=seq(-5, 5, by=0.5),
                 position = "dodge", color = "black", alpha = 0.7) +
  xlim(c(-3.5, 5)) +
  labs(title = "Sentiment Distribution of Amazon Book Reviews", 
                       y = "Frequency Density", 
                       x = "Sentiment Score", 
                       fill = "Vine Member") +
  scale_fill_discrete(name = "Vine Member", breaks=c("Y", "N"), labels = c("Yes", "No")) +
  theme(plot.background = element_rect(fill="lightgray"), 
        plot.title=element_text(size=18, face="bold", hjust=0.5, lineheight=1.2),
        axis.title.x=element_text(size=14, face="bold"),
        axis.title.y=element_text(size=14, face="bold"),
        axis.text.x=element_text(size=11, color="black", face="bold"),
        axis.text.y=element_text(size=11, color="black", face="bold"),
        legend.title=element_text(size=14, face="bold"), 
        legend.text=element_text(size=12, face="bold")) 
plot(cool_graph)

theme_set(theme_minimal(base_family = "Myanmar Text"))
alt_graph <- ggplot(merged, aes(x = sentiment, fill = vine, color = vine)) + 
  geom_histogram(aes(y = ..density..), color = "black", binwidth = 0.5, alpha = 0.7) + 
  xlim(c(-5,5)) + labs(title = "Sentiment Histogram of Amazon Book Reviews", 
                       y = "Frequency Density", 
                       x = "Sentiment Score", 
                       fill = "Vine Member") +
  scale_fill_discrete(name = "Vine Member", breaks=c("Y", "N"), labels = c("Yes", "No")) +
  scale_color_discrete(guide = FALSE) + 
  theme(panel.background = element_rect(fill = 'lightgray'))
plot(alt_graph)

# Vine Member Reviews
theme_set(theme_minimal(base_family = "Myanmar Text"))
vine_plot <- ggplot(data = vine_sentiment, mapping = aes(x = sentiment)) +
  geom_histogram(aes(y = ..count../sum(..count..)),  color = "black", 
                 breaks=seq(-3.5, 5, by=.5),
                 fill = "dark green",
                 binwidth = 0.5, alpha = 0.7) +
  geom_vline(aes(xintercept=mean(sentiment)),
             color="black", linetype="dashed", size=1) +
  labs(title = "Sentiment Distribution of Vine Voice Reviews", 
       x = "Sentiment Score", y = "Proportion", fill = "Vine Member") +
  theme(plot.background = element_rect(fill="lightgray"), 
        plot.title=element_text(size=18, face="bold", hjust=0.5, lineheight=1.2),
        axis.title.x=element_text(size=14, face="bold"),
        axis.title.y=element_text(size=14, face="bold"),
        axis.text.x=element_text(size=11, color="black", face="bold"),
        axis.text.y=element_text(size=11, color="black", face="bold")) +
  scale_x_continuous(breaks = -5:5)
plot(vine_plot)

# Reviews from non-Vine
theme_set(theme_minimal(base_family = "Myanmar Text"))
vineless_plot <- ggplot(noVines, aes(x = sentiment)) +
  geom_histogram(aes(y = ..count../sum(..count..)), breaks=seq(-3.5, 5, by=.5), 
                 fill = "orange", color = "black", binwidth = 0.5, alpha = 0.7) +
  geom_vline(aes(xintercept=mean(sentiment)),
             color="black", linetype="dashed", size=1) +
  labs(title = "Sentiment Distribution of Non-Vine Reviews", 
       x = "Sentiment Score", y = "Proportion", fill = "Vine Member") +
  theme(plot.background = element_rect(fill="lightgray"), 
        plot.title=element_text(size=18, face="bold", hjust=0.5, lineheight=1.2),
        axis.title.x=element_text(size=14, face="bold"),
        axis.title.y=element_text(size=14, face="bold"),
        axis.text.x=element_text(size=11, color="black", face="bold"),
        axis.text.y=element_text(size=11, color="black", face="bold"))
plot(vineless_plot)


vineOnly$RDateTime <- strptime(vineOnly$review_date, format = "%Y-%m-%d") %>% 
  as.POSIXct()
vineOnly$RYEAR <- year(vineOnly$RDateTime)
vineOnly$RWEEK <- week(vineOnly$RDateTime)
vineOnly$RMONTH <- month(vineOnly$RDateTime) %>% 
  factor(labels =  c("January","February", "March", "April", "May", 
                     "June", "July", "August", "September",
                     "October", "November", "December"))
vineOnly$RDOW <- weekdays(vineOnly$RDateTime)
vineOnly$RMONTH <- months(vineOnly$RDateTime)
vineOnly$RQUART <- quarters(vineOnly$RDateTime)
vineOnly$RHOUR <- hour(vineOnly$RDateTime)

vineOnly$RDAY <- day(vineOnly$RDateTime)

vineOnly <- vineOnly %>% group_by(review_date) %>% mutate(reviewsToday = n())
unique_review_days <- distinct(vineOnly, reviewsToday, .keep_all=TRUE)
save(vineOnly, file = "vineOnly.RData")
# The Good Graph
day_order <- factor(all_review_counts$RDOW, levels = c("Sunday, Monday, Tuesday",
                                                       "Wednesday", "Thursday", "Friday",
                                                       "Saturday"))
theme_set(theme_classic(base_family = "Myanmar Text"))
myheatmap <- ggplot(all_review_counts, aes(RWEEK, WEEKDAY_NUM, fill = reviewsToday)) +
  geom_tile(color = "white") +
  facet_wrap(~ RYEAR, ncol = 1) + scale_y_continuous(breaks = 1:7) +
  scale_fill_gradientn(colours = c("#B5E384", "#FFFFBD", "#FFAE63", "#D61818")) +
  scale_x_continuous(name = "Week of the Year", breaks = 1:52, labels = 1:52) +
  labs(title = "Activity Level of Amazon Reviewers", fill = "Reviews Today", y = "Day of the Week") +
  theme(panel.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold"), 
        axis.text = element_text(size = 5, face = "bold"), 
        axis.title = element_text(size = 8, face = "bold"), 
        legend.title = element_text(face = "bold"), 
        legend.text = element_text(face="bold"),
        strip.background = element_rect(color = "white", fill="lightgray"), 
        strip.text = element_text(face = "bold"), 
        axis.line.x.bottom = element_blank(),
        axis.line.y.left = element_blank(),
        panel.grid.major = element_line())
plot(myheatmap)


# All Amazon Reviews 
# Process and clean the data
theme_set(theme_classic(base_family = "Myanmar Text"))
myheatmap <- ggplot(all_review_counts, aes(RWEEK, WEEKDAY_NUM, fill = reviewsToday)) +
  geom_tile(color = "white") +
  facet_wrap(~ RYEAR, ncol = 1) + scale_y_continuous(breaks = 1:7) +
  scale_fill_gradientn(colours=c("#B5E384", "#FFFFBD", "#FFAE63", "#D61818")) +
  scale_x_continuous(name = "Week of the Year", breaks = 1:52, labels = 1:52) +
  labs(title = "Daily Activity Level of Amazon Book Reviews", fill = "Reviews Today", y = "Day of the Week") +
  theme(panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 8, face = "bold"), 
        axis.title = element_text(size = 13, face = "bold"), 
        legend.title = element_text(face = "bold"), 
        legend.text = element_text(face="bold"),
        strip.background = element_rect(color = "white", fill="lightgray"), 
        strip.text = element_text(face = "bold"), 
        axis.line.x.bottom = element_blank(),
        axis.line.y.left = element_blank(),
        panel.grid.major = element_line())
plot(myheatmap)