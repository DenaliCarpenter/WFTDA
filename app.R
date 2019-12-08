library(dplyr)
library(shiny)
library(shinyjs)
library(rsconnect)
library(readr)
library(htmltools)
library(shinythemes)

data <- read_csv("WFTDA_data.csv")
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
  textOutput("Location")
  tags$a(href=as.character(data$Website[data$Names == input$inputNames]),
         "Website")),

  mainPanel(
  uiOutput("image1", click = "myImage")
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
        tags$img(src=imgurl2, width = 600, height = 600)
        # tags$a(href=as.character(data$Website[data$Names == input$inputNames]),
        #        "Website")
        )
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
}

shinyApp(ui, server)




