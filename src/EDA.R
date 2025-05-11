library(skimr)
library(readr)
library(tidyverse)

# Import the data
df <- readr::read_csv(
  "data/spotify-2023.csv",
  locale = readr::locale(encoding = "Windows-1252", grouping_mark = ","),
  col_types = cols(
    streams = col_double()
  )
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
(categorical_df <- df[, sapply(df, is.character)])

categorical_df_long <- categorical_df %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

ggplot(categorical_df_long, aes(x = value)) +
  geom_bar() +
  facet_wrap(~ variable, scales = "free_y") +  
  labs(title = "Distribution of Categorical Columns", x = "Category", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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
# 1. Categorical columns need to be encoded.
# 2. There are duplicates.
# ...
