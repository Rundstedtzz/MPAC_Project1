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

# Define server logic required to draw a xxx
server <- function(input, output, session) {
  
  setwd("/Users/ricky/Desktop/myproject/MPAC_Project1")
  parole1 <- read_csv(file = "../data/parole_final.csv")
  parole2 <- read_csv(file = "../data/parole_final2.csv")
  output$plot <- renderPlot({
    
    #Plot
    parole2 %>%
      filter(pubtitle %in% c(input$press)) %>%
      count(word, sort = TRUE) %>%
      slice_max(n, n = 15) %>%
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
      sidebarPanel(
        
    # press select
    checkboxGroupInput("press", label = h3("Newspaper"), 
                       choices = list("Sun Journal" = 1, "Bangor Daily News" = 2, "Portland Press Herald" = 3)),
    hr(),
    fluidRow(column(3, verbatimTextOutput("value")))
    
      ),
    mainPanel(
              plotOutput("plot"))
    
)
)



# Run the application 
shinyApp(ui = ui, server = server)
