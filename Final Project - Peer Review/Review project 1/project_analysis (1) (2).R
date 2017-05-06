library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
setwd("D:/EDAV-Project2/Folder2")
full_data <- read_csv('amazon_ratings_full.csv')

full_data$date <- dt<-as.POSIXct(full_data$timestamp,origin="1970-01-01",tz="GMT")
full_data$day <- weekdays(as.Date(full_data$date))
total_rows <- sapply(full_data,function(x) length(x))
missing_values <- sapply(full_data, function(x) sum(is.na(x)))

# Histogram of Ratings
count_by_ratings <- full_data %>%
  group_by(rating) %>%
  summarize(count = n())

count_by_cat <- full_data %>%
  group_by(Category) %>%
  summarize(count = n())


# Average Rating by product category
avg_rating_by_cat <- full_data %>%
  group_by(Category) %>%
  summarize(count = n(), avg_rating = mean(rating)) %>%
  arrange(desc(avg_rating)) %>%
  collect()
avg_rating_by_cat$Category <- factor(avg_rating_by_cat$Category, levels=rev(avg_rating_by_cat$Category))

# Number of Reviews by day of week
reviews_by_dayofweek <- full_data %>%
  group_by(day) %>%
  summarise(count = n())

# Average Price by product category

#unique_data <- full_data %>% select(asin,price, Category) %>% group_by(asin) %>% filter(row_number(asin) ==1)
#write.csv(unique_data,'unique_items.csv')

unique_data <- read_csv('unique_items.csv')
ggplot(data=unique_data, aes(x=price)) + geom_density() +
  xlab("Price of Item") +
  ylab("Density") +
  ggtitle(" Density Plot of Prices of Products")


avg_price_by_cat <- unique_data %>%
  filter(!is.na(price)) %>%
  group_by(Category) %>%
  summarize(count = n(), avg_price = mean(price)) %>%
  arrange(desc(avg_price)) %>%
  collect()
avg_price_by_cat$Category <- factor(avg_price_by_cat$Category, levels=rev(avg_price_by_cat$Category))

# Number of Reviews by Month
reviews_by_month <- full_data %>%
  mutate(month=month(date)) %>%
  group_by(month) %>%
  summarise(count = n())

# Number of Reviews by Year
reviews_by_year <- full_data %>%
  mutate(year=year(date)) %>%
  group_by(year) %>%
  summarise(count = n())

# Avg Ratings by Month

avg_ratings_by_month <- full_data %>%
  mutate(month=month(date)) %>%
  group_by(month) %>%
  summarise(avg_rating = mean(rating))

# Avg Ratings by Year
avg_ratings_by_year <- full_data %>%
  mutate(year=year(date)) %>%
  group_by(year) %>%
  summarise(avg_rating = mean(rating))

# Review Analysis by User

reviews_by_user <- full_data %>% group_by(uid) %>% summarize(reviews=n())
reviews_by_user <- reviews_by_user %>% arrange(desc(reviews))
reviews_by_user_top_20 = reviews_by_user[1:20,]

review_counts <- reviews_by_user %>% group_by(reviews) %>%
  summarize(total=n()) %>%
  arrange(reviews) %>%
  collect()

cum_reviews <- review_counts %>% mutate(norm = total/sum(total), prop = cumsum(norm)) %>% filter((reviews <= 50))


#Connection between Books  and Electronics
book_prices <- unique_data %>% filter(Category=='Books')
book_prices <- book_prices$price


electronic_prices <- unique_data %>% filter(Category=='Electronics')
electronic_prices <- electronic_prices$price


#Average rating of highly priced item
avg_rating_highly_priced <- full_data %>% filter(price > 500) %>%
  group_by(Category) %>%
  summarize(count = n(), avg_rating = mean(rating)) %>%
  arrange(desc(avg_rating)) %>%
  collect()
avg_rating_highly_priced$Category <- factor(avg_rating_highly_priced$Category, levels=rev(avg_rating_highly_priced$Category))

# Avg Ratings by day of week

avg_ratings_by_dayofweek <- full_data %>%
  group_by(day) %>%
  summarise(avg_rating = mean(rating))

# Avg price by day of week
unique_data <- full_data %>% select(asin,price, Category, day) %>% group_by(asin) %>% filter(row_number(asin) ==1)
avg_price_by_dayofweek <- unique_data %>%
  filter(!is.na(price)) %>%
  group_by(day) %>%
  summarise(avg_price = mean(price))

# Top 5 star products
sorted_prod_by_rating_5star <- full_data%>%select(asin, title, rating, price, Category) %>% filter(rating==5, !is.na(price), !is.na(title))
sorted_prod_by_rating_5star <- sorted_prod_by_rating_1star%>% group_by(title, Category)%>%summarise(count=n(), ave_price=mean(price)) %>% arrange(desc(count))

# Top 1 star products
sorted_prod_by_rating_1star <- full_data%>%select(asin, title, rating, price, Category) %>% filter(rating==1, !is.na(price), !is.na(title))
sorted_prod_by_rating_1star <- sorted_prod_by_rating_1star%>% group_by(title, Category)%>%summarise(count=n(), ave_price=mean(price)) %>% arrange(desc(count))


# Save summary results for notebook & plotting
saveRDS(total_rows,"notebook/total_rows.rds")
saveRDS(missing_values, "notebook/missing_values.rds")
saveRDS(count_by_ratings,"notebook/count_by_ratings.rds")
saveRDS(count_by_cat,"notebook/count_by_categories.rds")
saveRDS(unique_data$price,"notebook/price_data.rds")
saveRDS(review_counts, "notebook/review_counts.rds")
saveRDS(reviews_by_user[1:10,], 'notebook/reviews_by_user_top10.rds')
saveRDS(avg_rating_by_cat, "notebook/avg_rating_by_cat.rds")
saveRDS(avg_price_by_cat, "notebook/avg_price_by_cat.rds")
saveRDS(reviews_by_month,"notebook/reviews_by_month.rds")
saveRDS(avg_ratings_by_month,"notebook/avg_ratings_by_month.rds")
saveRDS(reviews_by_year,"notebook/reviews_by_year.rds")
saveRDS(avg_ratings_by_year,"notebook/avg_ratings_by_year.rds")

#Added by Mike
saveRDS(book_prices,"notebook/book_prices.rds")
saveRDS(electronic_prices,"notebook/electronic_prices.rds")
saveRDS(avg_rating_highly_priced,'notebook/highly_priced.rds')

#Added by Amir
saveRDS(reviews_by_dayofweek, 'reviews_by_dayofweek')
saveRDS(avg_ratings_by_dayofweek, 'avg_ratings_by_dayofweek')
saveRDS(avg_price_by_dayofweek, 'avg_price_by_dayofweek')
saveRDS(sorted_prod_by_rating_1star, 'sorted_prod_by_rating_1star')
saveRDS(sorted_prod_by_rating_5star, 'sorted_prod_by_rating_5star')

