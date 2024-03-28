#----------------------------#
#  STAT 318 - Final Project  #
#----------------------------#

# load packages + data ---------------
packages = c('tidyverse', 'janitor', 'car')
lapply(packages, library, character.only = TRUE)

# data scraped on March 27, 2024
youtube.dat_raw = read.csv(file.choose(),header = TRUE)
youtube.dat = youtube.dat_raw

attach(youtube.dat)
names(youtube.dat)

# data management ---------------
# remove index column
youtube.dat <- youtube.dat[, -1]

# check classifications 
sapply(youtube.dat, class)

# create binary variable for official channel
youtube.dat$official_channel <- factor(ifelse(youtube.dat$channel_title == "AAACollege", "Official", "Not Official"))
#youtube.dat$official_channel <- factor(ifelse(youtube.dat$channel_title == "WellesleyCollege", "Official", "Not Official"))
official_channel_count <- table(youtube.dat$official_channel); official_channel_count
# 36/300 are published by official account



# histogram of video categories
ggplot(youtube.dat, aes(x = factor(category_id))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Category ID", y = "Frequency", title = "Histogram of Video Categories") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# collapse levels for video category
youtube.dat <- youtube.dat %>%
  mutate(category_id = ifelse(category_id %in% c("Education", "Entertainment", "People & Blogs"), 
                              category_id, "Other"))

ggplot(youtube.dat, aes(x = factor(category_id))) +
  geom_bar(fill = "lightpink", color = "black") +
  labs(x = "Category ID", y = "Frequency", title = "Histogram of Video Categories - Collapsed") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# create proportions
youtube.dat$like_proportion <- as.numeric(youtube.dat$like_count / youtube.dat$view_count)
youtube.dat$comment_proportion <- as.numeric(youtube.dat$comment_count / youtube.dat$view_count)
attach(youtube.dat)

# summary stats ----------------

# get standard deviations
youtube.dat %>%
  summarise(SD_duration = sd(duration),
            SD_view_count = sd(view_count),
            SD_like_count = sd(like_count),
            SD_comment_count = sd(comment_count),
            SD_channel_subscriber_count = sd(channel_subscriber_count),
            SD_like_proportion = sd(like_proportion),
            SD_comment_proportion = sd(comment_proportion),
            Count = n())

summaryStats <- summary(youtube.dat); summaryStats

# get unique categories
youtube.dat %>% tabyl(category_id)%>% adorn_totals(c('row', 'col'))

# linearity checks (with outliers) -----------------
youtube.dat %>% ggplot(aes(x=duration, y=like_proportion)) + 
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  labs(title = 'like_proportion vs duration',
       x='duration',
       y='like_proportion \n')

youtube.dat %>% ggplot(aes(x=duration, y=comment_proportion)) + 
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  labs(title = 'comment_proportion vs duration',
       x='duration',
       y='comment_proportion \n')

youtube.dat %>% ggplot(aes(x=duration, y=view_count)) + 
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  labs(title = 'view_count vs duration',
       x='duration',
       y='view_count \n')

# removing outliers --------------

# define a function to remove outliers using IQR method
remove_outliers <- function(data) {
  q1 <- quantile(data, 0.25)
  q3 <- quantile(data, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  cleaned_data <- ifelse(data < lower_bound | data > upper_bound, NA, data)
  return(cleaned_data)
}

youtube_data_cleaned <- lapply(youtube.dat, function(x) if(is.numeric(x)) remove_outliers(x) else x)
# convert the list back to a data frame
youtube_data_cleaned <- as.data.frame(youtube_data_cleaned)

# linearity checks (with outliers) -----------------

youtube_data_cleaned %>% ggplot(aes(x=duration, y=view_count)) + 
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  labs(title = 'view_count vs duration (w/o outliers)',
       x='duration',
       y='view_count \n')

youtube_data_cleaned %>% ggplot(aes(x=duration, y=like_proportion)) + 
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  labs(title = 'like_proportion vs duration (w/o outliers)',
       x='duration',
       y='like_proportion \n')

youtube_data_cleaned %>% ggplot(aes(x=duration, y=comment_proportion)) + 
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  labs(title = 'comment_proportion vs duration (w/o outliers)',
       x='duration',
       y='comment_proportion \n')

youtube_data_cleaned %>% ggplot(aes(x=channel_subscriber_count, y=like_proportion)) + 
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  labs(title = 'like_proportion vs channel_subscriber_count (w/o outliers)',
       x='channel_subscriber_count',
       y='comment_proportion \n')

youtube_data_cleaned %>% ggplot(aes(x=official_channel, y=like_proportion)) + 
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  labs(title = 'like_proportion vs official_channel (w/o outliers)',
       x='official_channel',
       y='comment_proportion \n')

# Visualization (without outliers) -----------------

#------------duration versus like/comment proportion-----------------
#### group by official_channel
# Scatter plot exploring the relationship between like_proportion and duration, colored by official_channel
ggplot(youtube_data_cleaned, aes(x = duration, y = like_proportion, color = official_channel)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = 'Relationship between Like Proportion and Duration',
       x = 'Duration',
       y = 'Like Proportion') +
  scale_color_manual(values = c('Official' = 'blue', 'Not Official' = 'red'))

# Scatter plot exploring the relationship between comment_proportion and duration, colored by official_channel
ggplot(youtube_data_cleaned, aes(x = duration, y = comment_proportion, color = official_channel)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = 'Relationship between Comment Proportion and Duration',
       x = 'Duration',
       y = 'Comment Proportion') +
  scale_color_manual(values = c('Official' = 'blue', 'Not Official' = 'red'))

#### group by category
# Scatter plot exploring the relationship between like_proportion and duration, colored by category
ggplot(youtube_data_cleaned, aes(x = duration, y = like_proportion, color = category_id)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = 'Relationship between Like Proportion and Duration',
       x = 'Duration',
       y = 'Like Proportion')

# Scatter plot exploring the relationship between comment_proportion and duration, colored by category
ggplot(youtube_data_cleaned, aes(x = duration, y = comment_proportion, color = category_id)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = 'Relationship between Comment Proportion and Duration',
       x = 'Duration',
       y = 'Comment Proportion')

#------------subscriber count versus like/comment proportion-----------------
#### group by category
# Scatter plot exploring the relationship between like_proportion and subscriber count, colored by category
ggplot(youtube_data_cleaned, aes(x = channel_subscriber_count, y = like_proportion, color = category_id)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = 'Relationship between Like Proportion and Channel Subscriber Count',
       x = 'Duration',
       y = 'Like Proportion')

# Scatter plot exploring the relationship between comment_proportion and subscriber count, colored by category
ggplot(youtube_data_cleaned, aes(x = channel_subscriber_count, y = comment_proportion, color = category_id)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = 'Relationship between Comment Proportion and Channel Subscriber Count',
       x = 'Duration',
       y = 'Comment Proportion')
       
       
# Boxplot exploring the distribution of subscriber_count by official_channel
ggplot(youtube_data_cleaned, aes(x = official_channel, y = channel_subscriber_count, fill = official_channel)) +
  geom_boxplot() +
  labs(title = 'Distribution of Subscriber Count by Official Channel',
       x = 'Official Channel',
       y = 'Subscriber Count') +
  scale_fill_manual(values = c('blue', 'red'))
                            



