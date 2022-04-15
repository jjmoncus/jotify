
library(tidyverse)
library(spotifyr)
library(lubridate)
library(glue)

# client ID and client secret defined elsewhere
# same with spotify_id
access_token <- get_spotify_access_token()
auth_code <- get_spotify_authorization_code()

# pipline: most recent plays from spotify passed to genius search

# recently played songs
recently_played <- get_my_recently_played(limit = 50) %>% as_tibble()

# preprocess Spotify data - coerce all weird apostrophes to normal apostrophes in coluns we care about
recently_played_small <- recently_played %>%
  mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
         played_at = as_datetime(played_at)) %>% 
  select(track.id, track.name, artist.name, track.album.name, played_at, track.album.images) %>%
  mutate(track.name = gsub("[\u2018\u2019\u201A\u201B\u2032\u2035]", "'", track.name)) %>%
  mutate(artist.name = gsub("[\u2018\u2019\u201A\u201B\u2032\u2035]", "'", artist.name))

features <- recently_played_small$track.id %>%
  set_names(nm = ~recently_played_small %>% filter(track.id == .x) %>% pull(track.name)) %>%
  map_dfr(~get_track_audio_features(id = .x),
          .id = "track.name")

analysis <- recently_played_small$track.id %>%
  set_names(nm = ~recently_played_small %>% filter(track.id == .x) %>% pull(track.name)) %>%
  map(~get_track_audio_analysis(id = .x))

# more human-readable display
analysis <- analysis %>% 
  map(.f = function(x) {
    map(x, .f = function(y) {
      as_tibble(y)
      })
    })

# did analysis go okay for all songs?
all_went_well <- analysis %>%
  map_dbl(~.x %>%
        pluck("meta") %>%
          #status_code of 0 = no problems
        pull(status_code)
      ) %>%
  as.logical() %>%
  any() %>%
  `!`(.)

# squashing list items into list columns for merge
analysis_for_merge <- analysis %>% 
  map_dfr(~tibble(meta = list(.x$meta),
              track = list(.x$track),
              bars = list(.x$bars),
              beats = list(.x$beats),
              sections = list(.x$sections),
              segments = list(.x$segments),
              tatums = list(.x$tatums)),
          .id = "track.name")

# analysis_for_merge$track.name
# 
# # I have listened to LA FAMA three times
# recently_played_small$track.name %>% Filter(f = function(x) str_detect(x, "LA FAMA")) %>% length()
# check <- recently_played_small %>% filter(str_detect(track.name, "LA FAMA")) %>% select(track.name, played_at)
# 
# 
# inter <- recently_played_small %>%
#   left_join(features, by = "track.name")
# 
# # the rows groups are indeed the same
# inter %>% filter(str_detect(track.name, "LA FAMA")) %>%
#   split(~played_at) %>%
#   map(~identical(slice(.x,1), slice(.x, 2)) & 
#         identical(slice(.x,1), slice(.x, 3)))
# 
# inter %>% distinct()


final_data <- recently_played_small %>%
  left_join(features, by = "track.name") %>%
  left_join(analysis_for_merge, by = "track.name") %>%
  # for reasons I dont understand, we get duplicate rows here - cutting them resolves
  distinct()

save(final_data, file = "state/final_data.RData")

 # this is the df we will use for analysis
