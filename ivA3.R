library(dplyr)
library(ggplot2)
library(leaflet)
library(shiny)
library(sf)
library(jsonlite)

#data loading and processing
#-------------------------------------------------------------------------------
geo_data <- st_read("./data/LGA_2023_AUST_GDA2020/LGA_2023_AUST_GDA2020.shp")
geo_data <- st_transform(geo_data, crs = st_crs("+proj=longlat +datum=WGS84"))

selected_geo <- geo_data %>%
  filter(LGA_CODE23 == 24600)

#bars data
data_bar <- fromJSON("data/bars-and-pubs-with-patron-capacity.json")
data_bar_2023 <- data_bar %>%
  filter(census_year == 2021)

#User interfaces
#-------------------------------------------------------------------------------
ui <- fluidPage(

  titlePanel(" "),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "my-sidebar",
      br(),
      br(),
      h3("touris"),
      br(),
      checkboxGroupInput("checkboxes1",  "topic1：",choices = c("op1", "op2", "op3", "op1", "op2", "op3"),selected = "op1"), 
      checkboxGroupInput("checkboxes2", "topic1：",choices = c("op1", "op2", "op3"),selected = "op1"), 
      tags$div(actionButton("show_modal", "Show charts"), class = "my-checkbox-group" )
    ),
    mainPanel(
      class = "my-main",
      width = "100%",
      leafletOutput("mapPlot", height = "100vh"), 
      textOutput("checkbox_status"),
    )
  )
)

server <- function(input, output, session) {
  
  output$mapPlot <- renderLeaflet({
    leaflet(data_bar_2023) %>%
      addTiles() %>%
      setView(lat = -37.82, lng = 144.96, zoom = 15) %>%
      addProviderTiles("CartoDB.Positron")%>%
      addPolygons(data = selected_geo, color = "blue", fillOpacity = 0)%>% 
      addCircleMarkers(
        lng = ~longitude, 
        lat = ~latitude,  
        popup = ~trading_name
      )
  })
  
  # button event
  observeEvent(input$show_modal, {
    modal_content <- modalDialog(
      title = "this is a title",
      fluidRow(
        column(width = 6, "text, text,text,text。"),
        column(width = 6,
               plotOutput("plot_inside_modal", height = "300px"))
      ),
      footer = NULL,  
      easyClose = TRUE,  
      id = "my_modal"
    )
    
    # output model
    output$plot_inside_modal <- renderPlot({
      plot(1:10, type = "l", main = "chart example")
    })
    
    showModal(modal_content)
  })
}

shinyApp(ui, server)






