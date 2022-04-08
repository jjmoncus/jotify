
library(spotifyr)
library(geniusr)
library(tidyverse)
library(lubridate)
library(glue)
library(polite)
library(rvest)

# client ID and client secret defined elsewhere
# same with spotify_id
access_token <- get_spotify_access_token()
auth_code <- get_spotify_authorization_code()

# pipline: most recent plays from spotify passed to genius search

# recently played songs
recently_played <- get_my_recently_played(limit = 50) %>% as_tibble()

View(recently_played)

# preprocess Spotify data - coerce all weird apostrophes to normal apostrophes in coluns we care about
recently_played_small <- recently_played %>%
  mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
         played_at = as_datetime(played_at)) %>% 
  select(track.name, artist.name, track.album.name, played_at) %>%
  mutate(track.name = gsub("[\u2018\u2019\u201A\u201B\u2032\u2035]", "'", track.name)) %>%
  mutate(artist.name = gsub("[\u2018\u2019\u201A\u201B\u2032\u2035]", "'", artist.name))

# set up a search string to pass to Genius API
song_search_strings <- recently_played_small %>%
  mutate(song_search_string = glue("'{track.name}' by {artist.name}")) %>%
  pull(song_search_string)

# pass search string to Genius API
song_info <- song_search_strings %>%
  purrr::set_names() %>%
  imap(~.x %>%
        search_song(search_term = .,
                    n = 50)
  )


searches_with_no_genius_data <- song_info %>% map_lgl(~is_empty(.x)) %>% Filter(f = function(x) x) %>% names()
searches_with_genius_data <- base::setdiff(names(song_info), searches_with_no_genius_data)

# preprocessing Genius data - coerce all weird apostrophes to normal apostrophes in coluns we care about
song_info <- song_info %>%
  .[c(searches_with_genius_data)] %>%
  map(~.x %>%
        mutate(song_name = gsub("[\u2018\u2019\u201A\u201B\u2032\u2035]", "'", song_name)) %>%
        mutate(artist_name = gsub("[\u2018\u2019\u201A\u201B\u2032\u2035]", "'", artist_name)))

# filtering just on artist name
song_info %>%
  imap(~.x %>%
         select(-song_lyrics_url) %>%
        # filter down to searches where artist name in Genius data matches artist name from Spotify
        filter(
          str_detect(artist_name,
                     regex(pattern = str_extract(.y,
                                           pattern = regex("(?<=by\\s).*",
                                                           ignore_case = TRUE)),
                           ignore_case = TRUE))
          )
       )


# song titles from Spotify frequently include metadata about the song in parenthesis
# e.g. "Song name (ft. featured artist)
# e.g. "Song name (Remix) (feat. featured artist)
# e.g. "Song name (with featured artist)
# we want to flter on song name, but only based on root song name and not attached metadata

# filtering on both artist name and song name, accounting for metadata
good_results <- song_info %>%
  imap(~.x %>%
         select(-song_lyrics_url) %>%
         # filter down to searches where artist name in Genius data matches artist name from Spotify
         filter(
           str_detect(artist_name,
                      regex(pattern = str_extract(.y,
                                                  pattern = regex("(?<=by\\s).*",
                                                                  ignore_case = TRUE)),
                            ignore_case = TRUE))
         ) %>%
         filter(
           str_detect(song_name,
                      regex(pattern = str_extract(.y,
                                            pattern = "(?<=').*(?=')") %>%
                              str_extract(pattern = "^[^\\(|\\-]+") %>%
                              str_trim(side = "right"),
                            ignore_case = TRUE))
           )
       )

# some searches include poor results, i.e. results with no obviously good matches
songs_with_no_good_results <- good_results %>% map_lgl(~.x %>% nrow() == 0) %>% which() %>% names()
songs_to_get_lyrics <- base::setdiff(names(good_results), songs_with_no_good_results)

# get ids for top result
ids_for_lyrics <- good_results %>%
  .[songs_to_get_lyrics] %>%
  map(~.x %>%
        # presume best result is the top one
        slice(1) %>%
        pull(song_id)
      )

# next steps - try to pull lyrics using confirmed song_ids


# well, it looks like `geniusr::get_lyrics_id` is essenially
# id %>% get_song() %>% pull song_lyrics_url %>% read_html(song_lyrics_url) without first asking if you open a session with Genius
# gonna check this with polite
# 
# 
# 

site <- song_info[[1]] %>% slice(1) %>% pull(song_lyrics_url)

session <- bow(site,
               user_agent = "J.J. Moncus")
site_html <- scrape(session)

html_nodes(site_html, ".lyrics p")
html_nodes(site_html, ".header_with_cover_art-primary_info-title") %>% html_text()
# I def do not know HTML/CSS well enough to parse this
# back to the API
# 
# well, the Genius API does not allow lyrics to be displayed, but you search by them
# 
# and scraping lyrics from websites seems to violate licensing agreements
# ugh.



