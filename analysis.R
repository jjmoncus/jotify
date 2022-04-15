
library(tidyverse)
library(spotifyr)
library(lubridate)
library(glue)


# load data
load(file = "state/final_data.RData")

# get median values for top-level features
top_level_features <- c("danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo")

top_level_feature_values <- final_data %>% 
  select(all_of(top_level_features)) %>%
  summarise(across(everything(),
                   ~median(.x)))



# visualize each feature in a bar chart
# danceability

feature <- "danceability"

median_ft <- final_data %>% 
  select(all_of(feature)) %>%
  summarise(across(everything(),
                   ~median(.x)))

mean_ft <- final_data %>% 
  select(all_of(feature)) %>%
  summarise(across(everything(),
                   ~mean(.x)))

var_ft <- final_data %>%
  select(all_of(feature)) %>%
  summarise(across(everything(),
                   ~var(.x)))

# when reporting top-level findings, filter out duplicates?
# or does the fact that I listened to the same song multiple times have meaning here?
# it def does in the median/mean, so likely should be represented here
# can differentiate duplicates by played_at time


top_level_features %>%
  purrr::set_names() %>%
  map(function(x) {
    
    sym_ft <- sym(x)
    
    final_data %>%
      jotify_hbar(!!sym_ft)
  }) -> top_level_bars

final_data %>%
  jotify_hbar(danceability)

final_data %>%
  jotify_hbar(valence)
  
final_data %>%
  jotify_hbar(energy)

# chart for loudness looks dumb cuz all values are negative
final_data %>%
  jotify_hbar(loudness,
              boundaries = c(-40, 0),
              midpoint = -15)

final_data %>%
  jotify_hbar(acousticness)

final_data %>%
  jotify_hbar(tempo,
              boundaries = c(min(final_data$tempo), max(final_data$tempo)),
              nudge_x = 20,
              body_size = 8)





# when do I tend to listen?
# 
# 
# final_data$played_at




final_data %>%
  group_by(track.id) %>%
  summarise(n = n(),
            track.name = track.name,
            artist.name = artist.name) %>%
  distinct() %>%
  arrange(desc(n)) %>%
  filter(n > 1)

final_data %>% names()



