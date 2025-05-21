library(skimr)
library(readr)
library(tidyverse)
source("utils.R")

# Import the data
df <- read_csv(
  "../data/spotify-2023.csv",
  locale = locale(encoding = "ISO-8859-2", grouping_mark = ","), 
  col_types = cols(streams = col_double())
)

# Check if the data import was correct
glimpse(df)

# Check for missing values
colSums(is.na(df))

# Delete missing values to continue EDA
(clean_df <- na.omit(df))

# Check for duplicated rows
(duplicates <- df[duplicated(df), ])

# Distribution of categorical variables
plot_categorical_distribution(df, "key")

plot_categorical_distribution(df, "mode")

# Distribution of artist
artist_counts_df <- count_individual_categories_df(df, "artist(s)_name")

ggplot(head(artist_counts_df, 20), aes(x = count, y = reorder(artist, count))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 20 Most Frequent Artists", x = "Artist", y = "Count")

# Box plots to check basic statistics
(numeric_df <- df[, sapply(df, is.numeric)])

numeric_df_long <- numeric_df %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

ggplot(numeric_df_long, aes(x = "", y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +  
  labs(title = "Boxplots for Numeric Columns", x = "", y = "Values")

# Heatmap to see all of the correlations
cor_matrix <- cor(numeric_df, use = "complete.obs")

cor_data <- as.data.frame(as.table(cor_matrix))

ggplot(cor_data, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = round(Freq, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Correlation Heatmap", x = "Variable", y = "Variable", fill = "Correlation")

# Summary
# 1. Track name is a unique identifier.
# 2. Categorical columns - "key" and "mode" need to be encoded.
# 3. Encode only artists with the most occurrences for example above 10.
# 4. There are no duplicates.
# 5. There are some missing values:
#       - in "key" create a new category - unknown, 
#       - in shazam charts set the values to 0,
#       - the 1 one missing observation in "streams" may be deleted.
# 6. Correlations are all logical. Biggest correlations are between columns refering to music streaming platforms.
