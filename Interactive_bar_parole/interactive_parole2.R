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
  parole1 <- read_csv(file = "../data/parole_final.csv")
  parole2 <- read_csv(file = "../data/parole_final2.csv")
  output$plot <- renderPlot({
    
    #Plot

    parole_sub11 %>%
      mutate(percentage_negative = negative/(negative + positive + neutral)) %>%
      mutate(percentage_positive = positive/(negative + positive + neutral)) %>%
      group_by(year, pubtitle) %>%
      arrange(desc(input$axis)) %>%
      na.omit() %>%
      filter(pubtitle %in% c(input$press)) %>%
      filter(year > year(input$year[1]) & year < year(input$year[2])) %>%
      
      ggplot(aes(x=year, y=input$axis, fill=pubtitle)) +
      geom_bar() +
      ggtitle("frequency of negative words in Maine news around parole") +
      labs(subtitle = "by press")
    
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

    #Y-axis
    radioButtons("axis", label = h3("Y-axis"),
                       choices = list("negative" = "negative count", 
                                      "positive" = "positive count",
                                      "percentage nagetive" = "percentage_negative",
                                      "percentage positive" = "percentage_positive")),
    
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
