#' Most played songs
#'
#' @param data A tibble, usually the final_data
most_played_df <- function(data) {
  data %>%
    group_by(track.id) %>%
    summarise(n = n(),
              track.name = track.name,
              artist.name = artist.name,
              .groups = "drop") %>%
    distinct() %>%
    ungroup() %>%
    arrange(desc(n)) %>%
    filter(n > 1)
}

#'
#'
most_played_song_name <- function(data) {

  df <- most_played_df(data)
  max_n <- df$n %>% max()

  df %>%
    filter(n == max_n) %>%
    pull(track.name)
}

#'
#'
most_played_song_artist <- function(data) {

  df <- most_played_df(data)
  max_n <- df$n %>% max()

  df %>%
    filter(n == max_n) %>%
    pull(artist.name)
}


#'
#'
pull_image_url <- function(data, song_name, size = c("small, medium", "large")) {

  # checking arges
  size <- match.arg(size,
                    choices = c("small", "medium", "large"),
                    several.ok = FALSE)

  # largest is always top row, then medium, then small
  size <- switch(size,
                 large = 1,
                 medium = 2,
                 small = 3)

  data %>%
    filter(track.name == song_name) %>%
    pull(track.album.images) %>%
    pluck(1) %>%
    slice(size) %>%
    pull(url)
}


#'
#'
deep_cuts_df <- function(data) {

  data %>%
    select(track.popularity,
           track.name,
           artist.name) %>%
    distinct() %>%
    arrange(track.popularity) %>%
    head(n = 5)
}

