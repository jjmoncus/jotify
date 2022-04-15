#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application
ui <- fluidPage(
    
    # navbar at the top
    navbarPage("Real-time analysis of J.J.'s Spotify listening data",
               
               tabPanel("Recent highlights",
                        
                        fluidRow(
                            column(width = 2),
                            column(width = 8,
                                   # Analysis description
                                   titlePanel("The recent fav:")
                                   ),
                            column(width = 2)
                            ),
                        fluidRow(
                            column(width = 2),
                            column(width = 8,
                                   imageOutput("recent_fav_image")
                                   ),
                            column(width = 2),
                            ),
                        fluidRow(
                            column(width = 2),
                            column(width = 8,
                            textOutput("recent_fav_text")
                            ),
                            column(width = 2)  
                            )
                        ),
               
               tabPanel("Toplines features",
                        
                        # Page description
                        titlePanel("Average features across an entire song"),
                         
                        sidebarLayout(
                             # Sidebar with a multiple choice input for the feature to visualize 
                             sidebarPanel(
                                 selectInput("feature",
                                             label = "Feature:",
                                             choices = c("danceability", "energy", "loudness", 
                                                         "speechiness", "acousticness", "instrumentalness", 
                                                         "liveness", "valence", "tempo"),
                                             selected = "danceability",
                                             multiple = FALSE),
                                 numericInput("body_size",
                                              label = "Data label font size",
                                              value = 9,
                                              min = 1,
                                              max = 20)
                             ),
                             # Show a plot of the generated distribution
                             mainPanel(
                                 plotOutput("bars")
                                 )
                             )
                         ),
                tabPanel("Overtime",
                         
                         # Page description
                         titlePanel("How does 'loudness' change over the length of the song?"),
                         
                         sidebarLayout(
                             sidebarPanel(
                                 # song names
                                 selectInput("song",
                                             label = "Song:",
                                             choices = unique(final_data$track.name),
                                             multiple = FALSE)
                                 ),
                             # Show a plot of the generated distribution
                             mainPanel(
                                 plotOutput("loudness_overtime")
                                 )
                             )
                         ),
                tabPanel("About the author",
                         
                         htmlOutput("author_description")
                         )
        )
)

# Define server logic 
server <- function(input, output) {
    
    most_played_song <- final_data %>% most_played_song_name()
    most_played_song_artist <- final_data %>% most_played_song_artist()
    
    output$recent_fav_text <- renderText({
        
        glue("{double_quote(most_played_song)} by {most_played_song_artist}")
    })
    
    output$recent_fav_image <- renderImage({
        
        # grab pic url
        url <- most_played_song %>% pull_image_url(final_data,
                                                   song_name = ., 
                                                   size = "medium")
        # render pic to jpg file
        outfile <- tempfile(fileext = "jpg")
        png(outfile)
        pic <- image_read(url)
        print(pic)
        dev.off()
        
        # return a list
        list(src = outfile)
    }, deleteFile = TRUE)

    output$bars <- renderPlot({
        
        sym_feature <- sym(input$feature)
        final_data %>%
            jotify_hbar(!!sym_feature,
                        body_size = input$body_size)
    })
    
    output$loudness_overtime <- renderPlot({
        
        final_data %>%
            jotify_loudness_overtime(., song_name = input$song)
        })
    
    output$author_description <- renderPrint({
        
        tags$body(
            h5("This app was developed by:"),
            br(),
            br(),
            h2("J.J. Moncus"),
            h4("Research Software Dev @ The Pew Research Center"),
            h4("jmoncus@pewresearch.org"),
            h5("Feel free to read my recent",
               a('publications', href = 'https://www.pewresearch.org/staff/jerry-joseph-moncus/'),
               "or check out my",
               a('Github', href = 'https://github.com/jjmoncus')
               )
            )
    })
}



# Run the application 
shinyApp(ui = ui, server = server)



# aerae
# a('publications', href = 'https://www.pewresearch.org/staff/jerry-joseph-moncus/'),
# a('Github', href = 'https://github.com/jjmoncus')





