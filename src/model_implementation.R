library(skimr)
library(readr)
library(tidyverse)
library(purrr)
library(ggplot2)
source("utils.R")

# Import the data
df <- read_csv(
  "../data/preprocessed-spotify-2023.csv",
  locale = locale(encoding = "ISO-8859-2", grouping_mark = ","), 
  col_types = cols(streams = col_double())
) 
glimpse(df)

# Steps
# 1. Scaling
# 2. Train-test split
# 3. Looking for the most optimal number of clusters
# 4. Model Implementation
# 5. Model evaluation

# 1
# Leave binary columns, drop unique identifier, scale the rest
binary_cols <- sapply(df, function(col) all(unique(col) %in% c(0,1)))

cont_cols <- setdiff(names(df)[sapply(df, is.numeric)], names(binary_cols)[binary_cols])

scaled_cont <- scale(df[cont_cols])

df <- cbind(as.data.frame(scaled_cont), df[binary_cols])

# 2
# Split 80:20
set.seed(123)

sample_indices <- sample(seq_len(nrow(scaled_data)), size = 0.8 * nrow(scaled_data))

train_data <- scaled_data[sample_indices, ]
test_data <- scaled_data[-sample_indices, ]

# 3
# Find the optimal number of clusters using the Elbow Method
wss <- map_dbl(1:10, function(k) {
  kmeans(final_data, centers = k, nstart = 25)$tot.withinss
})

elbow_df <- data.frame(
  k = 1:10,
  wss = wss
)

ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Elbow Method to Determine Optimal Number of Clusters",
       x = "Number of Clusters (k)",
       y = "Total Within-Clusters Sum of Squares (WSS)")
# The "elbow" point: 4

# 4
# Implement kmeans with k = 4
kmeans_model <- kmeans(train_data, centers = 4, nstart = 25)

# 5


 