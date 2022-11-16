#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(dplyr)
library(leaflet)
library(sf)
library(fontawesome)

temp_data0 <- read.csv("ubr_MWMT_all_sites_2017-2021.csv")  

alias <- unique(temp_data0$Site)

colours <- scales::hue_pal()(14)

colour_df <- cbind("Col" = colours, "Site" = alias) %>%
  as.data.frame()

df <- read_sf("UBR-WaterTemp-monitoring-sites_2021-12-08.shp") %>%
  st_transform(4326) %>%
  right_join(., colour_df, by=c("Alias"="Site"))

ws <- read_sf("copy ubr_subwatersheds_egis_ubw_2021-06-10.shp") %>%
  st_transform(4326) 

temp_data <- right_join(temp_data0, as.data.frame(df), by=c("Site"="Alias")) 

sites <- unique(temp_data$STATION_NA)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Water Temperature Monitoring Data for the Upper Bulkley River Watershed"),
    
    #verbatimTextOutput("text"),

    # Sidebar with a slider input for number of bins 
    sidebarPanel( 
      "This page displays water temperature monitoring data and locations from Fisheries and Oceans Canada's Upper Bulkley Sockeye and Chinook Habitat 
      Restoration Feasibility Study. For more information please visit the project data page on the ",
      tags$a(href="https://data.skeenasalmon.info/dataset/upper-bulkley-water-temperature-monitoring-data", 
             "Skeena Salmon Data Centre"),    
      br(),
      br(),
      
      dateRangeInput("daterange", "Select date range:", start = "2017-01-01", end = "2021-12-05", min = "2017-01-01",
                         max = "2021-12-05", format = "yyyy-mm-dd", separator = "-"),
        
               checkboxGroupInput("checkGroup", 
                                  "Select sites to show on graph and map", 
                                  choices = c(sites), selected=c(sites))),
          


        # Show a plot of the generated distribution
        mainPanel(
          #h4("Water temperature monitoring data for the Upper Bulkley River Watershed"), 
          plotOutput("distPlot"),
           leafletOutput("map", height = 500)
           #tableOutput("Table")
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
filtered_data <- reactive({ 
   req(input$checkGroup) 
   filter(temp_data, STATION_NA %in% c(input$checkGroup))
  
   })
   
df_filtered <- reactive({ 
  req(input$checkGroup) 
  filter(df, STATION_NA %in% c(input$checkGroup))
  
})
   
  output$map <- renderLeaflet({

    
    filtered_data_map <- df_filtered()

    m <- leaflet() %>%
      addTiles() %>%
      addMarkers(data = filtered_data_map, 
                               label = paste(filtered_data_map$STATION_NA)) %>%
      addPolygons(data = ws, label = paste(ws$WTSD_NAME, " Subwatershed"), color = "#2A6CC5", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.1,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = FALSE)) %>%
      addLegend("bottomright", color = "#2A6CC5", labels = "Subwatershed Boundary",
                opacity = 0.5)
      
    m
  })
  
  
  output$distPlot <- renderPlot({
    
      ggplot(filtered_data(), aes(as.Date(Date), MWMT, colour=STATION_NA)) +
      scale_colour_discrete(name="Site") +
        geom_line() +
        theme_bw() +
        labs(x="Date", y=expression("Mean Weekly Maximum Temperature "~"("^o~"C)")) +
        #xlim("2017-01-01", "2021-12-05")
        xlim(as.Date(input$daterange[1]), as.Date(input$daterange[2])) + 
        theme(text = element_text(size = 15))

    
    
  })
  
  #output$Table <- renderTable({head(filtered_data(), n=10)})

    
 
  #output$text <- renderText({
  #  paste("input$checkGroup is", paste(as.character(input$checkGroup)))
  #})
  
  

  

  
}

# Run the application 
shinyApp(ui = ui, server = server)
