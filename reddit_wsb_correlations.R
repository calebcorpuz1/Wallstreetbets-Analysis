
library(tidyverse)


rm(list=ls())

reddit_df <- read.csv('/Users/ccorpuz/Desktop/DSC 680/reddit_stock_df.csv')

df <- reddit_df %>%
  transmute(
    date = as.Date(Date),
    Daily_Avg_Sia = Daily_Avg_Sia,
    close = Close,
    close_lag1 = lag(close, 1),
    close_lag2 = lag(close, 2),
    close_lag3 = lag(close, 3),
    close_lag4 = lag(close, 4),
    close_lag5 = lag(close, 5),
    sia_diff = sia_diff,
    sia_diff_Lag1 = lag(sia_diff, 1),
    sia_diff_Lag2 = lag(sia_diff, 2),
    sia_diff_Lag3 = lag(sia_diff, 3),
    sia_diff_Lag4 = lag(sia_diff, 4),
    sia_diff_Lag5 = lag(sia_diff, 5),
    price_change = price_change,
    price_change_Lag1 = lag(price_change, 1),
    price_change_Lag2 = lag(price_change, 2),
    price_change_Lag3 = lag(price_change, 3),
    price_change_Lag4 = lag(price_change, 4),
    price_change_Lag5 = lag(price_change, 5),
    Volume = Volume,
    Volume_lag1 = lag(Volume, 1),
    Volume_lag2 = lag(Volume, 2),
    Volume_lag3 = lag(Volume, 3),
    Volume_lag4 = lag(Volume, 4),
    Volume_lag5 = lag(Volume, 5),
    num_posts = num_posts
    
  )

ggplot(df, aes(x = date, y = Daily_Avg_Sia)) + 
  geom_point() + 
  geom_line() + 
  ggtitle("Daily Average Sentiment Score")

ggplot(df, aes(x = date, y = price_change)) + 
  geom_point() + 
  geom_line() + 
  ggtitle("Daily Price Change")

### Correlation between sentiment and price #######################################################
cor(df$Daily_Avg_Sia, df$close, use = "pairwise.complete.obs", method = "pearson")
cor(df$Daily_Avg_Sia, df$close_lag1, use = "pairwise.complete.obs", method = "pearson")
cor(df$Daily_Avg_Sia, df$close_lag2, use = "pairwise.complete.obs", method = "pearson")
cor(df$Daily_Avg_Sia, df$close_lag3, use = "pairwise.complete.obs", method = "pearson")
cor(df$Daily_Avg_Sia, df$close_lag4, use = "pairwise.complete.obs", method = "pearson")
cor(df$Daily_Avg_Sia, df$close_lag5, use = "pairwise.complete.obs", method = "pearson")

### Correlation between sentiment and price change ################################################

cor(df$Daily_Avg_Sia, df$price_change, use = "pairwise.complete.obs", method = "pearson")
cor(df$Daily_Avg_Sia, df$price_change_Lag1, use = "pairwise.complete.obs", method = "pearson")
cor(df$Daily_Avg_Sia, df$price_change_Lag2, use = "pairwise.complete.obs", method = "pearson")
cor(df$Daily_Avg_Sia, df$price_change_Lag3, use = "pairwise.complete.obs", method = "pearson")
cor(df$Daily_Avg_Sia, df$price_change_Lag4, use = "pairwise.complete.obs", method = "pearson")
cor(df$Daily_Avg_Sia, df$price_change_Lag5, use = "pairwise.complete.obs", method = "pearson")

### Correlation between sentiment change and price change #########################################

cor(df$sia_diff, df$price_change, use = "pairwise.complete.obs", method = "pearson")
cor(df$sia_diff, df$price_change_Lag1, use = "pairwise.complete.obs", method = "pearson")
cor(df$sia_diff, df$price_change_Lag2, use = "pairwise.complete.obs", method = "pearson")
cor(df$sia_diff, df$price_change_Lag3, use = "pairwise.complete.obs", method = "pearson")
cor(df$sia_diff, df$price_change_Lag4, use = "pairwise.complete.obs", method = "pearson")
cor(df$sia_diff, df$price_change_Lag5, use = "pairwise.complete.obs", method = "pearson")


### Correlation between number of posts and volume ################################################

cor(df$num_posts, df$Volume, use = "pairwise.complete.obs", method = "pearson")
cor(df$num_posts, df$Volume_lag1, use = "pairwise.complete.obs", method = "pearson")
cor(df$num_posts, df$Volume_lag2, use = "pairwise.complete.obs", method = "pearson")
cor(df$num_posts, df$Volume_lag3, use = "pairwise.complete.obs", method = "pearson")
cor(df$num_posts, df$Volume_lag4, use = "pairwise.complete.obs", method = "pearson")
cor(df$num_posts, df$Volume_lag5, use = "pairwise.complete.obs", method = "pearson")

ggplot(df, aes(Daily_Avg_Sia, price_change_Lag3)) + 
  geom_point() + 
  labs(y = "Daily Price Change (Lag 3)", x = "Daily Avg Sentiment") + 
  ggtitle("Price Change with 3 Period Lag vs Daily Average Sentiment Score") +
  theme_classic()





