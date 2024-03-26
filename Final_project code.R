library(dplyr)
library(ggplot2)
library(reshape2)
library(fmsb)
library(tidyr)
library(car)

d <- read.csv("/Users/jenni/Desktop/STAT_318/Final_project/youtube_data_test.csv", header = TRUE)
summaryStats <- summary(d)

# Replace "Wellesley" with "XXX" (case-insensitive)
d$title <- gsub("(?i)\\bWellesley\\b", "XXX", d$title, perl = TRUE)

# Create binary variable for offical channel
d$official_channel <- as.integer(d$channel_title == "WellesleyCollege")
count <- table(d$official_channel) #23/100 are published by offcial account

# Combine less meaningful categories
d$category_id[d$category_id != "Education" & d$category_id != "News & Politics" & d$category_id != "People & Blogs"] <- "Other"

##Visualization	
# Create histogram for pre-cleaned categories
ggplot(d, aes(x =  category_id )) +
  geom_bar(fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Frequency of Categories", x = "Category", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
