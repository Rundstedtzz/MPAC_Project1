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
  
#  setwd("/Users/ricky/Desktop/myproject/MPAC_Project1")
  parole1 <- read_csv(file = "../data/parole_final.csv")
  output$plot <- renderPlot({
    
    #Plot
    parole1 %>%
      group_by(year, pubtitle) %>%
      mutate(percentage_negative = negative/(negative + positive + neutral)) %>%
      arrange(desc(percentage_negative)) %>%
      filter(pubtitle %in% c(input$press)) %>%
      #filter(pos %in% c(input$pos)) %>%
      #filter(year %in% c(input$year)) %>%
      #filter(Title %in% c(input$title)) %>%
      ggplot(aes(x=year, y=input$y_axis, fill=pubtitle)) +
      geom_area(stat="identity", position=position_dodge()) +
      ggtitle("ratio of negative adjectives/adverbs occurance in Maine news around parole") +
      labs(subtitle = "by press") +
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
    fluidRow(column(3, verbatimTextOutput("value"))),
    
    #pos
    checkboxGroupInput("pos", label = h3("Part of speech"), 
                       choices = list("Noun" = 1, "Verb" = 2, "Adjective" = 3, 
                                      "Adverb" = 4)),
    hr(),
    fluidRow(column(3, verbatimTextOutput("value"))),
    
    #sentiment
    checkboxGroupInput("sentiment", label = h3("Sentiment"), 
                       choices = list("Negative" = 1, "Neutral" = 2, "Positive" = 3)),
    hr(),
    fluidRow(column(3, verbatimTextOutput("value"))),

    
    fluidRow(
      column(4, verbatimTextOutput("range"))
    ),
    
    #year
    fluidRow(
      column(4,
             sliderInput("year", label = h3("Year"), min = 2000, 
                         max = 2022, value = c(2000, 2022))
      )
    ),
    
    #count or percentage
    checkboxGroupInput("y_axis", label = h3("Y-axis"), 
                       choices = list("count" = 1, "percentage_negative" = 2)),
    hr(),
    fluidRow(column(3, verbatimTextOutput("value"))),
    
    hr()
    
      ),
    mainPanel(
              plotOutput("plot"))
    
))



# Run the application 
shinyApp(ui = ui, server = server)
