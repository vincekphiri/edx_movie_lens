#Clear memory and objects from workspace
rm(list=ls())
gc()

#Load Required libraries
if(!require(pacman)) {
  install.packages("pacman", repos = "https://cran.r-project.org")
  library(pacman)
}

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

pacman::p_load(tidyverse, lubridate, caret, ggplot2, knitr, data.table)

#LOAD MOVIELENS DATASET
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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


##Exploration
# Plot the distribution of ratings
ggplot(edx, aes(x = rating)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Distribution of Movie Ratings", x = "Rating", y = "Count")


sampled_data <- edx %>% 
  sample_n(1000)



# Step 1: Split the genres into separate rows
genres_ratings <- edx %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(avg_rating = mean(rating), .groups = 'drop')

#Clean the timestamp 
edx <- edx %>%
  mutate(timestamp = as_date(timestamp)) %>% 
  mutate(year = year(timestamp))


ggplot(edx) +
  aes(x = year) +
  geom_density(fill = "#228B22", alpha = 0.6) +  # Adjust alpha for transparency
  theme_minimal() +
  labs(title = "Density Plot of Movie Ratings by Year",
       x = "Year",
       y = "Density") +
  scale_x_continuous(breaks = seq(min(edx$year), max(edx$year), by = 1))  # Ensure years are labeled clearly



# Create a line graph of the number of ratings by year
ggplot(edx %>%
         group_by(year) %>%
         summarise(count_ratings = n()), aes(x = year, y = count_ratings)) +
  geom_line(color = "#228B22", size = 1) +  # Line color and size
  geom_point(color = "#228B22", size = 2) +  # Add points for clarity
  theme_minimal() +
  labs(title = "Number of Ratings by Year",
       x = "Year",
       y = "Number of Ratings") +
  scale_x_continuous(breaks = seq(min(edx$year), max(edx$year), by = 1))  # Ensure years are labeled clearly


edx <- edx %>%
  mutate(
    year_released = str_extract(title, "\\(\\d{4}\\)"), 
    year_released = ifelse(!is.na(movieyear), as.numeric(str_extract(year_released, "\\d{4}")), NA) 
  ) 



# Count the number of ratings per movie
most_rated <- sampled_data %>%
  group_by(title) %>%
  summarise(rating = n()) %>%
  arrange(desc(rating))  # Sort by the number of ratings

# Plot the top 10 most-rated movies
top_movies <- most_rated %>% top_n(50, wt = rating)

ggplot(top_movies, aes(x = reorder(title, rating), y = rating)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip coordinates for better readability
  labs(
    title = "Top 10 Most Rated Movies in MovieLens",
    x = "Movie Title",
    y = "Number of Ratings"
  ) +
  theme_minimal()




# Count the number of ratings per movie
most_rated2 <- sampled_data %>%
  group_by(userId) %>%
  summarise(rating = n()) %>%
  arrange(desc(rating))  # Sort by the number of ratings


