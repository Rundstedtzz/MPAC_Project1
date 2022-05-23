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
    
    # Application title
    titlePanel("NeCSA Coastal Water Temperature at Every Station"),
    
    # Sidebar with a slider input for water temp min and max to filter 
    sidebarLayout(
        sidebarPanel(
            sliderInput("maxtemp",
                        "Maximum temperature to filter (Celsius):",
                        min = 1,
                        max = 40,
                        value = 25),
            sliderInput("mintemp",
                        "Minimum temperature to filter (Celsius):",
                        min = -20,
                        max = 5,
                        value = 0),
            
            checkboxGroupInput("checkStation", label = h3("Station"), 
                               choices = list("Shoals Marine Laboratory", "Edward McC. Blair Marine Research Station (MDR)",
                                              "Darling Marine Center", "Schoodic Institute at Acadia National Park", "Bates-Morse Mountain Conservation Area",
                                              "Bigelow Laboratory for Ocean Sciences"),
                               selected = list("Shoals Marine Laboratory", "Edward McC. Blair Marine Research Station (MDR)",
                                               "Darling Marine Center", "Schoodic Institute at Acadia National Park", "Bates-Morse Mountain Conservation Area",
                                               "Bigelow Laboratory for Ocean Sciences")),
            
            dateRangeInput("dates", label = h3("Date range"), start = "2016-09-04", end = "2018-08-11"),
            hr(),
            fluidRow(column(4, verbatimTextOutput("value"))),
            
            
            # Button
            downloadButton("downloadData", "Download")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("dataTable"),
            plotOutput("tempPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    library(dplyr)
    necsatemp <- read.csv(file = "https://raw.githubusercontent.com/LaurieLBaker/NeSCA-Field-Stations/main/data/necsatemp.csv")
    
    df1 = necsatemp
    library(zoo) # moving averages        
    library(tidyverse) # all tidyverse packages
    library(hrbrthemes) # themes for graphs
    library(socviz) # %nin%
    library(geofacet) # maps
    library(usmap) # lat and long
    library(socviz) # for %nin%
    library(ggmap) # mapping
    library(readr) # reading and writing data
    
    df2 <- reactive({df1[!(df1$Water.Temperature <= input$mintemp | df1$Water.Temperature >= input$maxtemp),]
    })
    
    hrbrthemes::import_roboto_condensed()
    
    
    df3 <- reactive({df2() %>% 
            mutate(Date = lubridate::parse_date_time(Date, orders = c("dmy", "mdy"))) %>%
            select(Date, Water.Temperature, Field.Station) %>%
            filter(Field.Station %in% input$checkStation) %>%
            mutate(Roll.Mean = c(rep(NA, 167), rollmean(Water.Temperature, k = 168))) %>%
            # after first collection
            dplyr::filter(Date >= lubridate::ymd(input$dates[1]) &
                              # # before last collection
                              Date <= lubridate::ymd(input$dates[2]))})
    #retrieved from https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
    
    
    output$tempPlot <- renderPlot({
        
        
        df3() %>%
            ggplot2::ggplot() +
            ggplot2::geom_line(aes(x = Date, y = Roll.Mean,
                                   color = Field.Station)) +
            #ggplot2::geom_line(aes(x = Date,
            # color = Field.Station, y = Water.Temperature)) +  
            ggplot2::labs(title = "Rolling Average Temperature at each Coastal Station", 
                          subtitle = "Between 09-04-2016 and 08-11-2018",
                          y = "Water Temperature", 
                          color = "Metric",
                          x = "Date") + 
            # hrbrthemes::theme_ipsum_rc() +
            theme(legend.position = "bottom")
        
        
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = "NeCSA_Temperature.csv",
        content = function(file) {
            write.csv(df3(), file, row.names = FALSE)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)