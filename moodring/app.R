#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # navbar at the top
    navbarPage("Real-time analysis of J.J.'s Spotify listening data",
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

# Define server logic required to draw a histogram
server <- function(input, output) {

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





