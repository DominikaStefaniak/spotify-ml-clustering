# Plot the distribution of categorical variables
plot_categorical_distribution <- function(df, column_name) {
  freq_table <- table(df[[column_name]])
  
  plot_df <- as.data.frame(freq_table)
  colnames(plot_df) <- c("Category", "Count")
  
  ggplot(plot_df, aes(x = Category, y = Count)) +
    geom_bar(stat = "identity", fill = "#69b3a2") +
    labs(
      title = paste("Distribution of", column_name),
      x = column_name,
      y = "Count"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# Counting occurrences of all categories
count_individual_categories_df <- function(df, col_name) {
  raw <- df[[col_name]]
  
  individual_categories <- unlist(strsplit(raw, ","))
  
  individual_categories <- trimws(individual_categories)
  
  counts <- table(individual_categories)
  
  count_df <- as.data.frame(counts, stringsAsFactors = FALSE)
  colnames(count_df) <- c("category", "count")
  count_df <- count_df[order(-count_df$count), ]
  
  return(count_df)
}

# Encoding categories
one_hot_encode <- function(df, column_name) {
  df[[column_name]] <- as.factor(df[[column_name]])
  
  dummies <- model.matrix(~ . - 1, data = df[column_name])
  dummies <- as.data.frame(dummies)
  
  df <- df[, !(names(df) %in% column_name)]
  df <- cbind(df, dummies)
  
  return(df)
}

# PCA implementation
reduce_with_optimal_pca <- function(data, cols, n_clusters = 4, max_pc = min(length(cols), 10)) {
  data_subset <- data[, cols, drop = FALSE]
  pca <- prcomp(data_subset, scale. = TRUE)
  
  scores <- sapply(1:max_pc, function(k) {
    pca_data <- pca$x[, 1:k, drop = FALSE]
    km <- kmeans(pca_data, centers = n_clusters, nstart = 25)
    sil <- silhouette(km$cluster, dist(pca_data))
    mean(sil[, 3])
  })
  
  best_k <- which.max(scores)
  reduced_data <- as.data.frame(pca$x[, 1:best_k, drop = FALSE])
  colnames(reduced_data) <- paste0("PC", 1:best_k)
  return(reduced_data)
}

# Robust scaling
robust_scale <- function(x) {
  iqr <- IQR(x, na.rm = TRUE)
  if (iqr == 0 || is.na(iqr)) return(NULL)
  (x - median(x, na.rm = TRUE)) / iqr
}