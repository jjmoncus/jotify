
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

