
#' Standard horizontal bar chart
jotify_hbar <- function(data, 
                        var,
                        nudge_x = 0.05,
                        body_size = 9) {
  
  quo_var <- enquo(var)
  chr_var <- as_name(quo_var)
  boundaries <- c(min(data[[chr_var]]), max(data[[chr_var]]))
  midpoint <- mean(boundaries)
  
  final_data %>%
    mutate(y_label = str_c(track.name," ", played_at)) %>%
    ggplot(mapping = aes(x = !!quo_var,
                         y = reorder(y_label, !!quo_var),
                         fill = !!quo_var)) +
    geom_bar(stat = "identity",
             width = 0.7) +
    geom_text(mapping = aes(x = 0, 
                            label = round(!!quo_var, digits = 2)),
              size = body_size/(.pt),
              color = "white",
              nudge_x = nudge_x) +
    scale_fill_gradient2(limits = boundaries,
                         midpoint = midpoint,
                         low = "darkblue",
                         mid = "lightgrey",
                         high = "red") +
    theme_jotify(family = "Times New Roman",
                 x_axis = FALSE,
                 body_size = body_size)
}


#' Horizontal bar chart - binned y values
jotify_vbar_binned <- function(data, 
                               var,
                               breaks) {
  
  quo_var <- enquo(var)
  
  final_data %>%
    mutate(y_label = str_c(track.name," ", played_at)) %>%
    ggplot(mapping = aes(x = !!quo_var,
                         fill = !!quo_var)) +
    geom_histogram(breaks = breaks) +
    scale_fill_manual(values = c("darkblue", "red")) +
    theme_jotify(family = "Times New Roman",
                 x_axis = FALSE)
}



# geom_text(mapping = aes(x = 0, 
#                         label = round(!!quo_var, digits = 2)),
#           color = "white",
#           nudge_x = nudge_x) +


jotify_loudness_overtime <- function(data, song_name) {
  
  # the pluck(1) both gets us down from a list item to a data frame
  # while also ensuring we only pick one data frame, in the casee the song_name
  # has duplicate entries
  reference <- data %>% filter(track.name == song_name) %>% .$sections %>% pluck(1)
  if (is.null(reference)) return("No data to display")
  
  # get loudness for a particular second, with reference to a reference dataset
  get_loudness_at_second <- function(x) {
    map_dbl(x,~(.x >= reference$start) %>% which() %>% max() %>% reference[[., "loudness"]])
  }
  
  plot_data <- tibble(
    second = 1:trunc(max(reference$start)),
    loudness = get_loudness_at_second(second)
    )
  
  plot_data %>%
    ggplot() +
    geom_ribbon(mapping = aes(x = second,
                              ymin = -30,
                              ymax = loudness)) +
    ylim(-30, 0)
}

