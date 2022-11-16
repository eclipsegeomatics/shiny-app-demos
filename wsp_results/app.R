#Based on code from https://gist.github.com/nathancday/18f570dd89e71f4f88b11ef79cdd94ca#file-inputmap-r


library(shiny)
library(sf)
library(leaflet)
library(DT)
library(dplyr)

fwa <- read_sf("fwa-road-density_py_EGIS_kispiox_3005_2020-11-04 .shp") %>%
  st_transform(4326) %>%
  rename("ID" = GROUP_C, "Watershed" = name, "FWA_FID" = WTRSHD_, "Risk"=risk) %>%
  mutate(Area_km2 = round(area, 2), Road_length_km = round(rd_lngt, 2), Road_density_km_per_km2 = round((rds_pr_*100), 2)) %>%
  select(ID, Watershed, FWA_FID, Area_km2, Road_length_km, Road_density_km_per_km2, Risk)
iwap <- read_sf("iwap-road-density_py_EGIS_kispiox_3005_2020-11-04 .shp") %>%
  st_transform(4326) %>%
  rename("ID" = GROUP_C, "Risk"=risk) %>%
  mutate(Area_km2 = round(area, 2), Road_length_km = round(rd_lngt, 2), Road_density_km_per_km2 = round((rds_pr_*100), 2)) %>%
  select(ID, Area_km2, Road_length_km, Road_density_km_per_km2, Risk)

nc <- st_read(system.file("shape/nc.shp", package="sf"))

ui <- fluidPage(
  titlePanel("Road Density for the Kispiox Timber Supply Area"),
  
  #verbatimTextOutput("text"),
  
  sidebarLayout(
    sidebarPanel(
      "This page displays the 2019 road density analysis results for Freshwater Atlas assessment watersheds and
      Interior Watershed Assessment Protocol watersheds within the Kispiox Timber Supply Area (TSA). 
      For more information please visit the project data page on the ",
      tags$a(href="https://data.skeenasalmon.info/dataset/wild-salmon-policy-indicator-analysis-for-the-kispiox-tsa", 
             "Skeena Salmon Data Centre"),    
      br(),
      br(),
      
      selectInput(inputId = "AU", 
                  label = "Select Assessment Unit Results to Display",
                  choices = c("Freshwater Atlas Assessment Watersheds", 
                              "Interior Watershed Assessment Protocol"),
                  selected = "Freshwater Atlas Assessment Watersheds"),
      
      uiOutput("controls")
    ),
    
    mainPanel(
      
      leafletOutput("inputMap", height = 400),
      dataTableOutput("filteredResults")
    )
  )
)

server <- function(input, output, session) {
  
  riskpal <- colorFactor(c("coral2", "forestgreen", "gold"), fwa$Risk)
  
  dataset_in <- reactive({ 
    switch(input$AU, "Freshwater Atlas Assessment Watersheds" = fwa,
           "Interior Watershed Assessment Protocol" = iwap)

    
  })
  
  rv <- reactiveValues()
  
  output$inputMap <- renderLeaflet({
    
    dataset <- dataset_in()
    
   
    leaflet(dataset) %>%
      addTiles() %>%
      addPolygons(layerId = ~ID, label = ~ID, color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~riskpal(Risk),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>%
      addLegend("bottomright", pal = riskpal, values = ~Risk,
                title = "Risk Rating",
                opacity = 1)
  })
  
  
  output$controls <- renderUI({
    req(input$inputMap_shape_click)
    sidebarPanel(id = "controls", top = 100, left = 50, 
                  right = "auto", bottom = "auto", width = "auto", height = "auto",
                  actionButton(inputId = "reset", label = "Clear selection", class = "btn-primary")
    )
  })
  

  
  observeEvent(input$inputMap_shape_click, {

    dataset <- dataset_in()
    click <- input$inputMap_shape_click
    req(click)

     rv$dataset <- filter(dataset, ID == click$id)

    leafletProxy("inputMap", session, data = rv$dataset) %>%
      removeShape("selected") %>%
      addPolygons(layerId = "selected",
                  fillColor = "red",
                  fillOpacity = 1)

     #show('controls')


  })
  output$filteredResults <- renderDataTable({
    
    dataset <- dataset_in()
    if (is.null(rv$dataset)){
      return(st_set_geometry(dataset, NULL))
    } else {return(st_set_geometry(rv$dataset, NULL))}
    
  })
  
  

  
  
  # output$text <- renderText({
  #  paste("input$filteredResults_rows_selected is", paste(as.character(input$filteredResults_rows_selected)))
  # })
  

  
  observeEvent(input$reset, {
    
    #hide('controls')
    dataset <- dataset_in()
    rv$dataset <- dataset
    
    leafletProxy("inputMap", session, data = dataset) %>%
      removeShape("selected") 
    
  })
  
}

shinyApp(ui = ui, server = server)