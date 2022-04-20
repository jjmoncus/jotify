#'ggplot theme for jotify graphics
#'
#'
#'

theme_jotify <- function(head_size = 12, 
                         body_size = 9, 
                         family = "Franklin",
                         x_axis = TRUE,
                         y_axis = TRUE,
                         legend = TRUE) {
  
  basic_line <- element_line(color = "#A4A4A4", size = .2)
  
  theme_bw(base_size = body_size, base_family = family) %+replace%
    theme(title = element_text(family = family,
                                    size = head_size,
                                    margin =  margin(b=4, unit = "pt"),
                                    hjust = 0,
                                    vjust = 0),
          plot.subtitle =  element_text(family = "Georgia",
                                        size = body_size,
                                        face = "italic",
                                        colour = "#595959",
                                        margin =  margin(b = 6, unit = "pt"),
                                        hjust = 0, 
                                        vjust = 0
                                        ),
          text =  element_text(family = family,
                                       size = body_size,
                                       color = "#7F7F7F",
                                       margin = margin(t = unit(5, "line")),
                                       hjust = 0, 
                                       vjust = 0),
          # Set up legends
          legend.title =  element_blank(),
          legend.margin=  if (legend) margin(0,0,0,0) else element_blank(),
          legend.position = if (legend) "bottom" else "none",
          legend.box.margin = if (legend) margin(-5,-10, 0,-10) else element_blank(),
          
          # Set up facet labels
          strip.background =  element_blank(),
        
          # Set up axes
          axis.line.x = if (x_axis) basic_line else element_blank(),
          axis.ticks.x = if (x_axis) basic_line else element_blank(),
          axis.line.y = if (y_axis) basic_line else element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.length =  unit(6, "pt"),
          axis.text.x = if (x_axis) {
            element_text(family = family,  size = body_size, hjust = .5, vjust = .5)
            } else {element_blank()},
          axis.text.y = if (y_axis) {
            element_text(family = family, size = body_size, hjust = 1, vjust = 0.5)
            } else {element_blank()},
          axis.title =  element_blank(),
    
          # Adjust panel attributes
          panel.grid =  element_blank(),
          panel.border =  element_blank(),
          complete = TRUE)
}




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


most_played_song_name <- function(data) {
  
  df <- most_played_df(data)
  max_n <- df$n %>% max()
  
  df %>%
    filter(n == max_n) %>%
    pull(track.name)
}

most_played_song_artist <- function(data) {
  
  df <- most_played_df(data)
  max_n <- df$n %>% max()
  
  df %>%
    filter(n == max_n) %>%
    pull(artist.name)
}



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



deep_cuts_df <- function(data) {
  
  data %>%
    select(track.popularity,
           track.name,
           artist.name) %>%
    distinct() %>%
    arrange(track.popularity) %>%
    head(n = 5)
}

