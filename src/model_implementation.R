library(skimr)
library(readr)
library(tidyverse)
library(cluster)
library(clusterCrit)
source("utils.R")

# Import the data
df <- read_csv(
  "../data/preprocessed-spotify-2023.csv",
  locale = locale(encoding = "ISO-8859-2", grouping_mark = ","), 
  col_types = cols(streams = col_double())
) 
glimpse(df)

# Steps
# 1. Scaling and pca
# 2. Looking for the most optimal number of clusters
# 3. Model Implementation
# 4. Model evaluation

# 1
# Reduce dimensionality, drop unique identifier, scale the rest of continous columns
#pca
playlist_chart_cols <- df %>% select(matches("in_.*_(playlists|charts)")) %>% colnames()
reduced_playlists <- reduce_with_optimal_pca(df, playlist_chart_cols)
colnames(reduced_playlists) <- "playlists_and_charts"

artist_cols <- df %>% select(starts_with("artist_encoded")) %>% colnames()
reduced_artists <- reduce_with_optimal_pca(df, artist_cols)
colnames(reduced_artists) <- c("artists1", "artists2", "artists3")

key_cols <- df %>% select(starts_with("key")) %>% colnames()
reduced_keys <- reduce_with_optimal_pca(df, key_cols)
colnames(reduced_keys) <- c("keys1", "keys2")

#scaling
excluded_cols <- c(playlist_chart_cols, artist_cols, key_cols, "track_name")

df_for_scaling <- df %>% select(-any_of(excluded_cols))

robust_scale <- function(x) {
  iqr <- IQR(x, na.rm = TRUE)
  if (iqr == 0 || is.na(iqr)) return(NULL)
  (x - median(x, na.rm = TRUE)) / iqr
}

scaled_list <- lapply(df_for_scaling, robust_scale)

scaled_list_filtered <- scaled_list[!sapply(scaled_list, is.null)]

(scaled_df <- cbind(as.data.frame(scaled_list_filtered), reduced_artists, reduced_keys, reduced_playlists))


# 2
# Find the optimal number of clusters using the Elbow Method
wss <- map_dbl(1:10, function(k) {
  kmeans(scaled_df, centers = k, nstart = 25)$tot.withinss
})

elbow_df <- data.frame(k = 1:10, wss = wss)

ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Elbow Method to Determine Optimal Number of Clusters",
       x = "Number of Clusters (k)",
       y = "Total Within-Clusters Sum of Squares (WSS)")

# The "elbow" point: 2

# 3
# Implement kmeans with k = 2
set.seed(123)
kmeans_model <- kmeans(scaled_df, centers = 2, nstart = 10)

# 4
# Evaluation using different metrics
# Silhouette Score
sil <- silhouette(kmeans_model$cluster, dist(scaled_df))
avg_silhouette <- mean(sil[, 3])
cat("Average Silhouette Score: ", avg_silhouette)

# Calinski-Harabasz Index
scaled_matrix <- as.matrix(scaled_df)
ch_index <- intCriteria(traj = scaled_matrix, part = kmeans_model$cluster, crit = "Calinski_Harabasz")
cat("Calinski-Harabasz Index: ", ch_index$calinski_harabasz, "\n")

# Davies-Bouldin Index
db_index <- intCriteria(traj = scaled_matrix, part = kmeans_model$cluster, crit = "Davies_Bouldin")
cat("Davies-Bouldin Index: ", db_index$davies_bouldin, "\n")
