library(dplyr)
library(shiny)
library(shinyjs)
library(rsconnect)
library(readr)
library(htmltools)
library(shinythemes)

library(maptools)
library(rgeos)
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)
library(googleway)
library(ggrepel)
library(ggspatial)
library(leaflet)

teams <- readOGR(dsn="Shape")
data <- read_csv("WFTDA_data.csv")
data <- data[-1]
data$Websites <- as.character(data$Websites)
data$Names <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", data$Names)
data$Country <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", data$Country)
data$Location <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", data$Location)

ui <- fluidPage(
  theme = shinytheme("slate"),
  useShinyjs(),
  titlePanel("Women's Flat Track Derby Association League Logos"),
  sidebarPanel(
  selectInput(inputId = "inputCountry", 
              label = "Countries",
              choices = data$Country),
  tags$div(title = "Select Team",
           selectInput(inputId = "inputNames",
                       label = "League",
                       choices = sort(unique(data$Names)))),
  textOutput("Location")),

  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Logo",
                         uiOutput("image1", click = "myImage")),
                tabPanel("Map",
                         leafletOutput("map")),
                tabPanel("Data",
                         tableOutput("table")),
                tabPanel("About",
                         textOutput("text")))
    )
)


server <- function(input, output, session){
  
  observe({
    country <- input$inputCountry
    
    if(is.null(country))
      country <- "United States"
    
    
    updateSelectInput(session, "inputNames",
                      label = paste("League"),
                      choices = data$Names[data$Country == country])
    })
  
  output$image1 <- renderUI({
    imgurl2 = data$Logo[data$Names == input$inputNames]

    div(id = "myImage",
        tags$img(src=imgurl2, width = 600, height = 600),
        tags$a(href=as.character(data$Websites[data$Names == input$inputNames]), "Website"))
    })

  output$Location <- renderText(
    paste(data$Location[data$Names == input$inputNames],
    data$Country[data$Names == input$inputNames])
    )


    onclick(
    "myImage",
    {
      
    }
  )
    output$table <- renderTable(data[-4:-5])
    output$map <- renderLeaflet(
      {
      leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lat = teams$Y, lng = teams$X, popup = teams$Names) %>%
          addPopups(lat = teams$Y[teams$Names == input$inputNames], 
                    lng = teams$X[teams$Names == input$inputNames],
                    popup = teams$Names[teams$Names == input$inputNames])})
    output$text <- renderText({"This data was scraped from the Women's Flat Track Derby Association website
      by Denali Carpenter '19. The original idea for this project was from the request of Tamara Beauboeuf-Lafontant to
      scrape women's roller derby logos. I believed that there was a better way to visualize the data associated with those
      logos. This app is the result."})
}

shinyApp(ui, server)




