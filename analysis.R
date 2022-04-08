
library(tidyverse)
library(spotifyr)
library(lubridate)
library(glue)

# get median values for top-level features

top_level_features <- c("danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo")

df %>% select(all_of(top_level_features)) %>%
  summarise(across(everything(),
                   ~median(.x)))
