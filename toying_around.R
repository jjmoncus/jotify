

library(spotifyr)
library(geniusr)
library(tidyverse)
library(lubridate)
library(glue)

# Get the Weeknd's albums
get_artist_albums('1Xyo4u8uXC1ZmMpatF05PJ')

# all time top artists
top_artists <- get_my_top_artists_or_tracks(type = "artists", time_range = "long_term", limit = 50)

top_artists %>%
  select(name, genres) %>% 
  rowwise %>% 
  mutate(genres = paste(genres, collapse = ', ')) %>% 
  ungroup()




# favorite songs at the moment

top_tracks <- get_my_top_artists_or_tracks(type = 'tracks', time_range = 'short_term', limit = 50)

View(top_tracks)

top_tracks %>%
  mutate(artist.name = map_chr(artists, function(x) x$name[1])) %>% 
  select(name, artist.name, album.name)


top_tracks %>%
  mutate(artist.name = map_chr(artists, function(x) x$name[1])) %>% 
  select(name, artist.name) %>%
  mutate(song_search_string = glue("'{name}' by {artist.name}")) %>%
  pull(song_search_string) -> song_search_strings


top_tracks %>%
  mutate(artist.name = map_chr(artists, function(x) x$name[1])) %>% 
  pull(name)

top_tracks %>%
  mutate(artist.name = map_chr(artists, function(x) x$name[1])) %>% 
  slice(43) %>% as_tibble()



# most joyful Weeknd song
?get_artist_audio_features
the_weeknd <- "1Xyo4u8uXC1ZmMpatF05PJ"
the_wknd_features <- get_artist_audio_features(artist = the_weeknd)
View(the_wknd_features)
the_wknd_features <- the_wknd_features %>% as_tibble()

the_wknd_features %>% filter(track_name == "Sacrifice") %>% View()



the_wknd_features %>%
  group_by(key_mode) %>%
  summarize(key = n()) %>%
  arrange(desc(key))
# obv most common modes are minor modes
# weirdly C major is common, what songs are those?
# 
# 
# 

the_wknd_features %>%
  filter(key_mode == "C major") %>%
  select(track_name, album_name) %>%
  unique()





# ---------------------------------------------------------------------------------------------------------