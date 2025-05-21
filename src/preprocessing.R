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

glimpse(df)

# Steps

# 1. In "key" create a new category - unknown for missing values, 
# 2. In shazam charts set the missing values to 0,
# 3. Delete 1 observation with missing "streams" value.
# 4. Encode categorical "key" and "mode".
# 5. Encode artists with above 10 occurrences.
# 6. Save preprocessed dataframe.

# 1
df$key[is.na(df$key) | df$key == ""] <- "unknown"

# 2
df$in_shazam_charts[is.na(df$in_shazam_charts)] <- 0

# 3
df <- df %>% filter(!is.na(streams))

# Check if handling missing values worked
colSums(is.na(df))

# 4
df <- one_hot_encode(df, "key")
df <- df %>%
  mutate(mode = ifelse(mode == "Major", 1, 0))

# 5
artist_counts <- df %>%
  count(`artist(s)_name`) %>%
  filter(n > 10)

df <- df %>%
  mutate(artist_encoded = ifelse(`artist(s)_name` %in% artist_counts$`artist(s)_name`,
                                 `artist(s)_name`, "Other"))

df <- one_hot_encode(df, "artist_encoded")

df <- df %>% select(-`artist(s)_name`)

# 6
write.csv(df, "../data/preprocessed-spotify-2023.csv", row.names = FALSE)
