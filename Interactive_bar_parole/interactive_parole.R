#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)

# Define server logic required to draw a xxx
server <- function(input, output, session) {
  
#  setwd("/Users/ricky/Desktop/myproject/MPAC_Project1")
  
  parole1 <- read_csv(file = "https://raw.githubusercontent.com/Rundstedtzz/MPAC_Project1/main/data/parole_final.csv")
  output$plot <- renderPlot({
    
    #Plot

    parole_final %>%
      filter(pubtitle %in% c(input$press)) %>%
      filter(pos %in% c(input$pos)) %>%
      filter(year > year(input$year[1]) & year < year(input$year[2])) %>%
      filter(sentiment %in% c(input$sentiment)) %>%
      count(word, sort = TRUE) %>%
      slice_max(n, n = input$top_words) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(n, word)) +
      geom_col() +
      ggtitle("Most common words in Maine news around parole") +
      labs(subtitle = "from texts", y = "word", x = "count/frequency") +
      theme_minimal()
    
  })
}


# Define UI for application that draws xxx
ui <- fluidPage(
    titlePanel("Interactive Visualization of Parole Sentiment"),
    sidebarLayout(
      sidebarPanel(width = 5, 
        
    # press select
    checkboxGroupInput("press", label = h3("Newspaper"), 
                       choices = list("Sun Journal" = "Sun Journal", 
                                      "Bangor Daily News" = "Bangor Daily News", 
                                      "Portland Press Herald" = "Portland Press Herald")),

    #pos
    checkboxGroupInput("pos", label = h3("Part of speech"),
                        choices = list("Noun" = "Noun", 
                                       "Adjective" = "Adjective",
                                       "Adverb" = "Adverb")),

    #sentiment
    checkboxGroupInput("sentiment", label = h3("Sentiment"),
                        choices = list("negative" = "negative", 
                                       "neutral" = "neutral", 
                                       "positive" = "positive")),

    #Top words
    numericInput("top_words", label = h3("Top words"), value = 1),


    #year

    sliderInput(inputId = "year",
                 label = "Year",
                 min = lubridate::ymd("20000101"),
                 max = lubridate::ymd("20220501"),
                 value = lubridate::ymd(c("20000101","20220501")),
                 step = 1,
                 timeFormat = "%Y")

    ),
      
    mainPanel(width = 7, 
              plotOutput("plot"))
    )
)




# Run the application 
shinyApp(ui = ui, server = server)
