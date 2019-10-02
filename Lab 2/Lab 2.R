library(readr)
library(dplyr)
extrafont::loadfonts(device="pdf")
extrafont::loadfonts(device="postscript")
library(extrafont)
library(ggplot2)
PERM <- read_csv("PERM2018.csv")
mydata <- PERM %>% select(c(CASE_STATUS, 
                            COUNTRY_OF_CITIZENSHIP, 
                            PW_AMOUNT_9089, 
                            FOREIGN_WORKER_INFO_EDUCATION)) %>% filter(CASE_STATUS == "Certified")
save(mydata, file = "Lab2.RData")
load("Lab2.RData")
nations <- mydata[[2]] # all nations listed in the doc, there will be some duplicates
# class(nations) would return "factor"

distinct_nations <- as.character(nations[!duplicated(nations) & !is.na(nations)]) # character vector of the nations, remove dups

# find the top 5 nations with the most certified applicants:
sort(table(as.character(nations)), decreasing = TRUE)[1:5]
# india, china, south korea, canada, mexico

TOP5 <- mydata %>% filter(COUNTRY_OF_CITIZENSHIP == "INDIA" |
                          COUNTRY_OF_CITIZENSHIP == "CHINA" |
                          COUNTRY_OF_CITIZENSHIP == "SOUTH KOREA" |
                          COUNTRY_OF_CITIZENSHIP == "CANADA" |
                          COUNTRY_OF_CITIZENSHIP == "MEXICO") %>% 
  filter(FOREIGN_WORKER_INFO_EDUCATION != "Other" &
            FOREIGN_WORKER_INFO_EDUCATION != "NA")

# Generating the Graphic:

# Ordering the graph's legend for the grouping variable
edu_levels <- c("None", "High School", "Associate's", "Bachelor's", "Master's", "Doctorate")
# edu_order <- factor(TOP5[[4]], levels = edu_levels)
nation_levels <- c("INDIA", "CHINA", "SOUTH KOREA", "CANADA", "MEXICO")

# How do we get the graph to display education from lowest to highest, and countries on x-axis descending order?
# New Variable Method
graph_data <- TOP5
graph_data$NATION_ORDER <- factor(graph_data$COUNTRY_OF_CITIZENSHIP, levels = nation_levels)
graph_data$EDUCATION_ORDER <- factor(graph_data$FOREIGN_WORKER_INFO_EDUCATION, levels = edu_levels)
# Existing Variable Method
order_nations <- factor(TOP5[[2]], levels = nation_levels)
order_edu <- factor(TOP5[[4]], levels = edu_levels)

options(scipen = 100000)
theme_set(theme_minimal(base_family = "Myanmar Text"))
my_graph <- ggplot(data = graph_data, aes(x = NATION_ORDER, y = PW_AMOUNT_9089, fill=EDUCATION_ORDER)) + 
  ylim(c(0, 200000)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_brewer(palette="Set3") + 
  labs(title = "Wages of Certified Immigrants in the U.S.", 
       x = "Country of Citizenship\nTop 5 Most Certified in Descending Order", y = "Prevailing Wage", 
       fill = "Highest Education Level Completed") +
  theme(plot.background=element_rect(fill = "lightgray"),
        plot.title=element_text(size=20,
                                face="bold",
                                hjust=0.5,
                                lineheight=1.2),
        axis.title.x=element_text(size=19, face="bold"),
        axis.title.y=element_text(size=19, face="bold"),
        axis.text.x=element_text(size=12, color="black", face="bold"),
        axis.text.y=element_text(size=12, color="black", face="bold"),
        legend.title=element_text(size=15, face="bold"), 
        legend.text=element_text(size=14, face="bold"))
plot(my_graph)

