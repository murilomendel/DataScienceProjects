##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(ggplot2)
library(knitr)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# 1.0 Downloading the data-set
zip_file <- "~/ml-10m.zip"
if(file.exists(zip_file)) {dl <- zip_file} else {
  dl <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
}

# Unzip and read files  
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


#---------------------------------------------------------------------
# 2.0 Data Exploring
load("~/_projects/DataScience/R/HarvardX - DataScience Program/Capstone/data/downloaded.RData")

# Dimensions of edx dataset
dim(edx)

# Data format
head(edx) %>% as.tibble()

# Descriptive statistics
summary(edx)

# Number of unique users and movies
edx %>% summarise(unique_Users = n_distinct(userId),
                  unique_Movies = n_distinct(movieId))

# By str_detect
genres = c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})

# Greatest number of ratings
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Top 5 Most given ratings
edx %>% group_by(rating) %>%
  summarize(count = n()) %>%
  top_n(5) %>%
  arrange(desc(count))

# Getting the max number of genres in the data_set
edx %>% mutate(genres_quantity = str_count(genres, "\\|") + 1) %>%
  arrange(desc(genres_quantity)) %>%
  select(genres, genres_quantity)
# Conclusion: max number of genres for a movie = 8 sub-genres

# Top 30 most rated movies and number of votes each of them received
edx %>% group_by(movieId) %>%
  summarise(stars = mean(rating),
            votes = n()) %>%
  left_join(edx, by = "movieId") %>%
  select(movieId, title, votes, stars) %>%
  unique() %>%
  filter(title != "NA") %>%
  arrange(desc(stars)) %>%
  slice(1:30) %>%
  mutate(ranking = 1:n()) %>%
  select(ranking, everything()) %>%
  kable()
# Observation: Most of best rated movies has low votes (mostly less than 10)

# Rating histogram
y_cuts <- c(0.5, 1.0, 1.5, 2.0, 2.5)
edx %>%
  ggplot(aes(rating, fill = cut(rating, 100))) +
  geom_histogram(bins = 30, show.legend = F) +
  theme_update() +
  labs(x = "Rating", y = "Movies") +
  ggtitle("Rating distribution over movies quantity") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_x_discrete(limits = c(seq(min(edx$rating),
                                  max(edx$rating),
                                  max(edx$rating)/n_distinct(edx$rating))))+
  scale_y_continuous(labels = paste0(y_cuts, "M"),
                     breaks = y_cuts * 10^6)

# Quantity of ratings users mostly made
edx %>% count(userId) %>%
  ggplot(aes(n, fill = cut(n, 100))) +
  geom_histogram(bins = 30, show.legend = F) +
  theme_update() +
  labs(x = "Quantity of ratings (log scale)", y = "Qa=uantity of Users") +
  ggtitle("Quantity of Ratings over Quantity of users") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_x_log10()

# Rating users mostly give
edx %>% group_by(userId) %>%
  summarise(m = mean(rating)) %>%
  ggplot(aes(m, fill = cut(m, 100))) +
  geom_histogram(bins = 30, show.legend = F) +
  theme_update() +
  labs(x = "Mean rating", y = "Users") +
  ggtitle("Figure 2.3: Mean rating per users") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white")) +
  scale_x_discrete(limits = c(seq(min(edx$rating),
                                  max(edx$rating),
                                  max(edx$rating)/n_distinct(edx$rating))))

# Feature Engineering
# 1 - Separating movies sub-genres (Max 8 groups)
# 2 - Separating movie title and movie year from title column
# 3 - Creating rating date (Year, Month and Day) from timestamp column
# 4 - Converting genres to numerical factors

edx <- edx %>%
  separate(genres, c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5", "genre_6", "genre_7", "genre_8"), "\\|", remove = FALSE) %>%
  mutate(movie_release = as.integer(str_replace_all(str_extract(title,"\\s\\(\\d{4}\\)"), "\\(|\\)", "")),
         title = str_replace(title, "\\s\\(\\d{4}\\)", ""),
         rating_date = as.Date(as.POSIXct(timestamp, origin="1970-01-01")),
         rating_wday = weekdays(rating_date)) %>%
  separate(rating_date, c("rating_year", "rating_month", "rating_mday"))

validation <- validation %>%
  separate(genres, c("genre_1", "genre_2", "genre_3", "genre_4", "genre_5", "genre_6", "genre_7", "genre_8"), "\\|", remove = FALSE) %>%
  mutate(movie_release = as.integer(str_replace_all(str_extract(title,"\\s\\(\\d{4}\\)"), "\\(|\\)", "")),
         title = str_replace(title, "\\s\\(\\d{4}\\)", ""),
         rating_date = as.Date(as.POSIXct(timestamp, origin="1970-01-01")),
         rating_wday = weekdays(rating_date)) %>%
  separate(rating_date, c("rating_year", "rating_month", "rating_mday"))

sum(!is.na(edx$genre_1))
# --------------------------------------------------------------------------------------------- #
# SUB_GENRES STUDY
# Checking the occurrence of each sub-group in the entire data set
edx %>% summarise(occurence_1 = sum(!is.na(edx$genre_1)) / length(edx$genre_1),
                  occurence_2 = sum(!is.na(edx$genre_2)) / length(edx$genre_2),
                  occurence_3 = sum(!is.na(edx$genre_3)) / length(edx$genre_3),
                  occurence_4 = sum(!is.na(edx$genre_4)) / length(edx$genre_4),
                  occurence_5 = sum(!is.na(edx$genre_5)) / length(edx$genre_5),
                  occurence_6 = sum(!is.na(edx$genre_6)) / length(edx$genre_6),
                  occurence_7 = sum(!is.na(edx$genre_7)) / length(edx$genre_7),
                  occurence_8 = sum(!is.na(edx$genre_8)) / length(edx$genre_8))

# Checking correlation between genres and ratings
edx %>% ggplot(aes(genre_1, rating)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 80),
        plot.title = element_text(hjust = 0.5, size = 16)) +
  labs(title = "Movie ratings over genres",
       caption = "Data from: http://files.grouplens.org/datasets/movielens/ml-10m.zip") +
  xlab("Ratings)") +
  ylab("Movies Genres")

gen <- c("Comedy", "Action", "Children", "Adventure", "Animation", "Drama", "Crime",
         "Sci-Fi", "Horror", "Thriller", "Film-Noir", "Mystery", "Western", "Documentary",
         "Romance", "Fantasy", "Musical", "War", "IMAX", "(no genres listed)")

# mean_rating vs genres matrix
genre_rate <- sapply(gen, function(g){
  r1 <- edx %>% filter(genre_1 == g) %>% summarise(mean_rating_genre_1 = mean(rating))
  r2 <- edx %>% filter(genre_2 == g) %>% summarise(mean_rating_genre_2 = mean(rating))
  r3 <- edx %>% filter(genre_3 == g) %>% summarise(mean_rating_genre_3 = mean(rating))
  r4 <- edx %>% filter(genre_4 == g) %>% summarise(mean_rating_genre_4 = mean(rating))
  r5 <- edx %>% filter(genre_5 == g) %>% summarise(mean_rating_genre_5 = mean(rating))
  r6 <- edx %>% filter(genre_6 == g) %>% summarise(mean_rating_genre_6 = mean(rating))
  r7 <- edx %>% filter(genre_7 == g) %>% summarise(mean_rating_genre_7 = mean(rating))
  r8 <- edx %>% filter(genre_8 == g) %>% summarise(mean_rating_genre_8 = mean(rating))
  as.double(c(r1, r2, r3, r4, r5, r6, r7, r8))
})

# Transpose Matrix
genre_rate <- t(genre_rate)
rowMeans(genre_rate, na.rm = TRUE)

# Filling the missing observations over genre_2 to genre_8
# The filling process replicate the existing genres for the movie to the missing genres
edx_filled <- edx %>% mutate(genre_2 = ifelse(is.na(genre_2), genre_1, genre_2),
                             genre_3 = ifelse(is.na(genre_3), genre_1, genre_3),
                             genre_4 = ifelse(is.na(genre_4), genre_2, genre_4),
                             genre_5 = ifelse(is.na(genre_5), genre_1, genre_5),
                             genre_6 = ifelse(is.na(genre_6), genre_2, genre_6),
                             genre_7 = ifelse(is.na(genre_7), genre_3, genre_7),
                             genre_8 = ifelse(is.na(genre_8), genre_4, genre_8))

# mean_rating vs genres matrix
genre_rate_filled <- sapply(gen, function(g){
  r1 <- edx_filled %>% filter(genre_1 == g) %>% summarise(mean_rating_genre_1 = mean(rating))
  r2 <- edx_filled %>% filter(genre_2 == g) %>% summarise(mean_rating_genre_2 = mean(rating))
  r3 <- edx_filled %>% filter(genre_3 == g) %>% summarise(mean_rating_genre_3 = mean(rating))
  r4 <- edx_filled %>% filter(genre_4 == g) %>% summarise(mean_rating_genre_4 = mean(rating))
  r5 <- edx_filled %>% filter(genre_5 == g) %>% summarise(mean_rating_genre_5 = mean(rating))
  r6 <- edx_filled %>% filter(genre_6 == g) %>% summarise(mean_rating_genre_6 = mean(rating))
  r7 <- edx_filled %>% filter(genre_7 == g) %>% summarise(mean_rating_genre_7 = mean(rating))
  r8 <- edx_filled %>% filter(genre_8 == g) %>% summarise(mean_rating_genre_8 = mean(rating))
  as.double(c(r1, r2, r3, r4, r5, r6, r7, r8))
})

# Transpose Matrix
genre_rate_filled <- t(genre_rate_filled)
rowMeans(genre_rate_filled, na.rm = TRUE)

# Mean difference between rates is 9.7% between original model and filled model 
mean(abs(rowMeans(genre_rate, na.rm = TRUE) - rowMeans(genre_rate_filled, na.rm = TRUE)))


# mean_rating vs genres matrix
genre_rate_4_filled <- sapply(gen, function(g){
  r1 <- edx_filled %>% filter(genre_1 == g) %>% summarise(mean_rating_genre_1 = mean(rating))
  r2 <- edx_filled %>% filter(genre_2 == g) %>% summarise(mean_rating_genre_2 = mean(rating))
  r3 <- edx_filled %>% filter(genre_3 == g) %>% summarise(mean_rating_genre_3 = mean(rating))
  r4 <- edx_filled %>% filter(genre_4 == g) %>% summarise(mean_rating_genre_4 = mean(rating))
  as.double(c(r1, r2, r3, r4))
})

# Transpose Matrix
genre_rate_4_filled <- t(genre_rate_4_filled)
rowMeans(genre_rate_4_filled, na.rm = TRUE)
rowSds(genre_rate_4_filled)

# Mean difference between rates is 9.8% between original model and 4 sub-genres filled model 
mean(abs(rowMeans(genre_rate, na.rm = TRUE) - rowMeans(genre_rate_4_filled, na.rm = TRUE)))


# mean_rating vs genres matrix
genre_rate_4 <- sapply(gen, function(g){
  r1 <- edx %>% filter(genre_1 == g) %>% summarise(mean_rating_genre_1 = mean(rating))
  r2 <- edx %>% filter(genre_2 == g) %>% summarise(mean_rating_genre_2 = mean(rating))
  r3 <- edx %>% filter(genre_3 == g) %>% summarise(mean_rating_genre_3 = mean(rating))
  r4 <- edx %>% filter(genre_4 == g) %>% summarise(mean_rating_genre_4 = mean(rating))
  as.double(c(r1, r2, r3, r4))
})

# Transpose Matrix
genre_rate_4 <- t(genre_rate_4)
rowMeans(genre_rate_4, na.rm = TRUE)
rowSds(genre_rate_4)

# Mean difference between rates is 4.7% between original model and 4 sub-genres model 
mean(abs(rowMeans(genre_rate, na.rm = TRUE) - rowMeans(genre_rate_4, na.rm = TRUE)))

# Conclusion:
# 1. Fill the model isn't a good approach
# 2. Build a model considering every sub-genres, filling NA's by 0 
# 2. Build a model with only genre_1  

# MOVIES RELEASE YEAR STUDY
edx %>% group_by(movie_release) %>%
  filter(movie_release > 2000) %>%
  summarise(mean = mean(rating)) %>%
  arrange(desc(mean))


# Plotting Quantity of ratings over movies release year
edx %>% group_by(movie_release) %>%
  summarize(count = n()) %>%
  ggplot() +
  geom_col(aes(movie_release, count)) +
  geom_hline(aes(yintercept = mean(count)), size = 1, colour = 'blue', na.rm = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  labs(title = "Ratings Quantity Vs. Release Year",
       caption = "Data from: http://files.grouplens.org/datasets/movielens/ml-10m.zip") +
  xlab("Movie Release Year") +
  ylab("Ratings Quantity")


# Very low negative correlation, showing that newest movies in general have lower ratings
cor(edx$rating, edx$movie_release)

# TIMESTAMP FEATURE STUDY
# Quantity of ratings per Year
edx %>% group_by(rating_year) %>%
  summarize(count = n()) %>%
  ggplot() +
  geom_col(aes(rating_year, count)) +
  geom_hline(aes(yintercept = mean(count)), size = 1, colour = 'blue', na.rm = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  labs(title = "Quantity of User Ratings per Year",
       caption = "Data from: http://files.grouplens.org/datasets/movielens/ml-10m.zip") +
  xlab("Year") +
  ylab("Quantity of User Ratings")

# Quantity of ratings per Month
edx %>% group_by(rating_month) %>%
  summarize(count = n()) %>%
  ggplot() +
  geom_col(aes(rating_month, count)) +
  geom_hline(aes(yintercept = mean(count)), size = 1, colour = 'blue', na.rm = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16)) +
  labs(title = "Quantity of User Ratings per Month",
       caption = "Data from: http://files.grouplens.org/datasets/movielens/ml-10m.zip") +
  xlab("Month") +
  ylab("Quantity of User Ratings")

# Quantity of ratings per month-day
edx %>% group_by(rating_mday) %>%
  summarize(count = n()) %>%
  ggplot() +
  geom_col(aes(rating_mday, count)) +
  geom_hline(aes(yintercept = mean(count)), size = 1, colour = 'blue', na.rm = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90)) +
  labs(title = "Quantity of User Ratings per Day",
       caption = "Data from: http://files.grouplens.org/datasets/movielens/ml-10m.zip") +
  xlab("Day") +
  ylab("Quantity of User Ratings")

# Quantity of ratings per week-day
edx %>% group_by(rating_wday) %>%
  summarize(count = n()) %>%
  ggplot() +
  geom_col(aes(rating_wday, count)) +
  geom_hline(aes(yintercept = mean(count)), size = 1, colour = 'blue', na.rm = TRUE) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 50)) +
  labs(title = "Quantity of User Ratings per Day",
       caption = "Data from: http://files.grouplens.org/datasets/movielens/ml-10m.zip") +
  xlab("Day") +
  ylab("Quantity of User Ratings")

# Removing title, timestamp and genres columns
edx <- subset(edx, select = -c(4,5,6,11,12,13,14))
validation <- subset(validation, select = -c(4,5,6,11,12,13,14))

# Data preparing for Linear modelling
edx.numeric <- edx
validation.numeric <- validation

# Changing week day from character to numerical factors
edx.numeric$rating_wday <- as.numeric(as.factor(edx.numeric$rating_wday))
edx.numeric$rating_mday <- as.numeric(edx.numeric$rating_mday)
edx.numeric$rating_month <- as.numeric(edx.numeric$rating_month)
edx.numeric$rating_year <- as.numeric(edx.numeric$rating_year)

validation.numeric$rating_wday <- as.numeric(as.factor(validation.numeric$rating_wday))
validation.numeric$rating_mday <- as.numeric(validation.numeric$rating_mday)
validation.numeric$rating_month <- as.numeric(validation.numeric$rating_month)
validation.numeric$rating_year <- as.numeric(validation.numeric$rating_year)

# Changing sub_genres to integer factors
edx.numeric <- edx.numeric %>%
  mutate(genre_1 = as.numeric(as.factor(genre_1)),
         genre_2 = as.numeric(as.factor(genre_2)),
         genre_3 = as.numeric(as.factor(genre_3)),
         genre_4 = as.numeric(as.factor(genre_4)))

validation.numeric <- validation.numeric %>%
  mutate(genre_1 = as.numeric(as.factor(genre_1)),
         genre_2 = as.numeric(as.factor(genre_2)),
         genre_3 = as.numeric(as.factor(genre_3)),
         genre_4 = as.numeric(as.factor(genre_4)))

# Converting NA to -1
edx.numeric$genre_2[is.na(edx.numeric$genre_2)] <- -1
edx.numeric$genre_3[is.na(edx.numeric$genre_3)] <- -1
edx.numeric$genre_4[is.na(edx.numeric$genre_4)] <- -1

validation.numeric$genre_2[is.na(validation.numeric$genre_2)] <- -1
validation.numeric$genre_3[is.na(validation.numeric$genre_3)] <- -1
validation.numeric$genre_4[is.na(validation.numeric$genre_4)] <- -1


# Variable selection for linear regression
regfit <- regsubsets(rating ~ ., data = edx.numeric, nvmax = 15)
summary(regfit)
plot(regfit, scale = "adjr2")
plot(regfit, scale = "bic")

# Defining null and full model
null = lm(rating ~ 1, data = edx.numeric)
full = lm(rating ~ ., data = edx.numeric)

# Forward variable selection
step(null, scope = list(lower = null, upper = full), direction = "forward")

# Backward variable selection
step(full, data = edx.numeric, direction = "backward")

# Stepwise variable selection
step(null, scope = list(upper = full), data = edx.numeric, direction = "both")


# Machine Learning modeling

# Linear Regression Fitting
linearRegression.fit <- lm(rating ~ ., data = edx.numeric)
#linearRegression.fit <- lm(rating ~ . -genre_8 -rating_wday, data = edx.numeric) # RMSE = 1.0497
#linearRegression.fit <- lm(rating ~ movieId + genre_1 + genre_3 + movie_release + rating_year + genre_4, data = edx.numeric) # RMSE = 1.0501

linearRegression.predict <- predict(linearRegression.fit, newdata = validation.numeric)
linearRegression.rmse <- RMSE(linearRegression.predict, validation.numeric$rating)

linearRegression.predict
linearRegression.rmse

# Full Model:
# - rating_wday and genre_8: p-value near to 0. Drop these features
# - F-static: 12450: Far from 1 for large n, still provide evidence against H0.

# 6 best feature model:
# - Low p-values. -> OK
# - R squared = 0.0198, Adjusted R-squared = 0.0198 -> NOK
# - R-Squared Near to zero: indicates that the regression did not explain much of the variable in the response
# - Low VIF (Variance Inflation Factor), which indicates low ammount of collinearity

rmse.results <- tibble(Model = "01. Full Linear Model",
                       RMSE = linearRegression.rmse,
                       Goal = ifelse(linearRegression.rmse < 0.8649, "Under", "Over"))

kable(rmse.results)

# Naive approach
mu = mean(edx$rating)

naive.rmse = RMSE(validation$rating, mu)
rmse.results <- rmse.results %>%
  bind_rows(tibble(Model = "Mean Rating Approach(Naive)",
                   RMSE = naive.rmse,
                   Goal = ifelse(naive.rmse < 0.8649, "Under", "Over")))

# Adding movie effect to the model
# Rating = mean_rating + beta_movie_effect(movieId)
movie_avg <- edx %>%
  group_by(movieId) %>%
  summarise(b_Movie = mean(rating-mu))

head(movie_avg)

movie_avg %>%
  ggplot(aes(b_Movie, fill = cut(b_Movie, 100))) +
  geom_histogram(bins = 30, show.legend = F) +
  theme_update() +
  labs(x = "beta_Movie", y = "Movies quantity") +
  ggtitle("Movies Quantity for each computed beta_Movie") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))

mean_movie.predict <- mu + validation %>%
  left_join(movie_avg, by = "movieId") %>%
  pull(b_Movie)

mean_movie.rmse <- RMSE(mean_movie.predict, validation$rating)

rmse.results <- rmse.results %>%
  bind_rows(tibble(Model = "03. Mean Rating + Movie Effect",
                   RMSE = mean_movie.rmse,
                   Goal = ifelse(mean_movie.rmse < 0.8649, "Under", "Over")))

kable(rmse.results)

# # Adding user effect to the previous model
# Rating = mean_rating + beta_movie_effect(movieId) + beta_user_effect(userId)
user_avg <- edx %>%
  left_join(movie_avg, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_User = mean(rating - b_Movie - mu))

head(user_avg)

user_avg %>%
  ggplot(aes(b_User, fill = cut(b_User, 100))) +
  geom_histogram(bins = 30, show.legend = F) +
  theme_update() +
  labs(x = "beta_User", y = "Movies quantity") +
  ggtitle("Movies Quantity for each computed beta_User") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))

mean_movie_user.predict <- validation %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  mutate(b_User = mu + b_Movie + b_User) %>%
  pull(b_User)

mean_movie_user.rmse <- RMSE(mean_movie_user.predict, validation$rating)

rmse.results <- rmse.results %>%
  bind_rows(tibble(Model = "04. Mean Rating + Movie Effect + User Effect",
                   RMSE = mean_movie_user.rmse,
                   Goal = ifelse(mean_movie_user.rmse < 0.8649, "Under", "Over")))
kable(rmse.results)


# Adding release year effect to the previous model
# Rating = mean_rating + beta_movie_effect(movieId) + beta_user_effect(userId) + beta_release_year(movie_release)

MovieReleaseYear_avg <- edx %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  group_by(movie_release) %>%
  summarise(b_MRY = mean(rating - b_Movie - b_User - mu))

head(MovieReleaseYear_avg)

MovieReleaseYear_avg %>%
  ggplot(aes(b_MRY, fill = cut(b_MRY, 100))) +
  geom_histogram(bins = 30, show.legend = F) +
  theme_update() +
  labs(x = "beta_MovieReleaseYear", y = "Movies quantity") +
  ggtitle("Movies Quantity for each computed beta_User") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))

MovieReleaseYear.predict <- validation %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  left_join(MovieReleaseYear_avg, by = "movie_release") %>%
  mutate(b_MRY = mu + b_Movie + b_User + b_MRY) %>%
  pull(b_MRY)

MovieReleaseYear.rmse <- RMSE(MovieReleaseYear.predict, validation$rating)

rmse.results <- rmse.results %>%
  bind_rows(tibble(Model = "05. Mean Rating + Movie Effect + User Effect + Movie Release Year",
                   RMSE = MovieReleaseYear.rmse,
                   Goal = ifelse(MovieReleaseYear.rmse < 0.8649, "Under", "Over")))
kable(rmse.results)

# # Adding main genre effect to the previous model
# Rating = mean_rating + beta_movie_effect(movieId) + beta_user_effect(userId) + beta_release_year(movie_release) + beta_main_genre

mainGenre_avg <- edx %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  left_join(MovieReleaseYear_avg, by = "movie_release") %>%
  group_by(genre_1) %>%
  summarise(b_genre = mean(rating - b_Movie - b_User - b_MRY - mu))

head(mainGenre_avg)

mainGenre_avg %>%
  ggplot(aes(b_genre, fill = cut(b_genre, 100))) +
  geom_histogram(bins = 30, show.legend = F) +
  theme_update() +
  labs(x = "beta_mainGenre", y = "Movies quantity") +
  ggtitle("Movies Quantity for each computed beta_genre") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))

mainGenre.predict <- validation %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  left_join(MovieReleaseYear_avg, by = "movie_release") %>%
  left_join(mainGenre_avg, by = "genre_1") %>%
  mutate(b_genre = mu + b_Movie + b_User + b_MRY + b_genre) %>%
  pull(b_genre)

mainGenre.rmse <- RMSE(mainGenre.predict, validation$rating)

rmse.results <- rmse.results %>%
  bind_rows(tibble(Model = "06. Mean Rating + Movie Effect + User Effect + Movie Release Year + Genre Effecf",
                   RMSE = mainGenre.rmse,
                   Goal = ifelse(mainGenre.rmse < 0.8649, "Under", "Over")))
kable(rmse.results)
