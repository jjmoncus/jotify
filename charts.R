


#' Standard horizontal bar chart
jotify_hbar <- function(data, var,
                        boundaries = c(0,1),
                        midpoint = mean(boundaries)) {
  
  quo_var <- enquo(var)
  
  final_data %>%
    mutate(y_label = str_c(track.name," ", played_at)) %>%
    ggplot(mapping = aes(x = !!quo_var,
                         y = reorder(y_label, !!quo_var),
                         fill = !!quo_var)) +
    geom_bar(stat = "identity",
             width = 0.7) +
    geom_text(mapping = aes(x = 0, 
                            label = round(!!quo_var, digits = 2)),
              color = "white",
              nudge_x = .1) +
    scale_fill_gradient2(limits = boundaries,
                         midpoint = midpoint,
                         low = "darkblue",
                         mid = "lightgrey",
                         high = "red") +
    theme_jotify(family = "Times New Roman",
                 x_axis = FALSE)
}
