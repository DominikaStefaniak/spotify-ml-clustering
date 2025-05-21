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
