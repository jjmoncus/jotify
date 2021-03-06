#' ggplot theme for jotify graphics
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

