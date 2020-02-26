library(ECharts2Shiny)
library(shiny)
library(ggplot2)
library(dplyr)

bcl <- read.csv("./www/mydata1.csv", stringsAsFactors = FALSE)



ui <- fluidPage(
  titlePanel("Customer Segmentation"),
  
  loadEChartsLibrary(),
  

  
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "recency", 0, 400, c(0, 400), pre = "D"),
      sliderInput("priceInput1", "frequenci", 0, 210, c(0, 210), pre = "T"),
      sliderInput("priceInput2", "monitery", 0, 80000, c(0, 80000), pre = "$"),

      uiOutput("countryOutput")
    ),
    mainPanel(
      #plotOutput("coolplot"),
      tags$div(id="test", style="width:50%;height:400px;"),
      deliverChart(div_id = "test"),
      br(), br(),
      tableOutput("results")
    )
  )
)

server <- function(input, output) {
  
 
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    bcl %>%
      filter(recency >= input$priceInput[1],
             recency <= input$priceInput[2],
             frequenci >= input$priceInput1[1],
             frequenci <= input$priceInput1[2],
             monitery >= input$priceInput2[1],
             monitery <= input$priceInput2[2],
             #Type == input$typeInput,
             country == input$countryInput
      )
  })
  

  output$results <- renderTable({
    filtered()
  })
  
  dat<-c(rep("Group-1:Champions", 122),rep("Group-2:Recent Customers", 1840),rep("Group-3:Hibernating", 1504), rep("Group-4:Promising", 972))
  
  
  renderPieChart(div_id = "test",
                 data = dat)
  
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$country)),
                selected = "CANADA",
    )
  })  
  
}

shinyApp(ui = ui, server = server)