library(shiny)
library(jsonlite)
library(data.table)
library(httr)
library(rtsdata)
library(DT)
library(TTR)
library(plotly)
library(dygraphs)
library(shinythemes)
library(shinydashboard)
source('functions.R')

ui <-dashboardPage(
  skin = "black",
  dashboardHeader(title = 'Political & Civil liberties scores around the globe'),
  
  dashboardSidebar(
    sidebarMenu(
      h5('Controls'),
      uiOutput("my_region"),
      uiOutput("my_year"),
      menuItem("Plotly chart on PR", tabName = "plotly", icon = icon("dashboard")),
      menuItem("Data table", tabName = "data", icon = icon("th")),
      menuItem("Ggplot chart on CL", tabName = "ggplot", icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "plotly",
              h1('Political rights data for selected region and year - lower = better'),
              plotlyOutput("data_plot")
      ),
      tabItem(tabName = "data",
              h2('This is a data table with all relevant datapoints'),
              tableOutput("table_out")
      ),
      tabItem(tabName = "ggplot",
              h2("Civil liberties data for selected region and year - lower = better"),
              plotOutput("simple_plot")
      )
    )
  )
)


server <- function(input, output, session) {
  data <- get_data()
  
  output$my_region <- renderUI({
    selectInput("region", "Select a region", choices = unique(data$Region_Name),
                selected = 'Asia', multiple = FALSE)
  })
  
  output$my_year <- renderUI({
    numericInput("my_year", label = "Year", 2000, min = min(data$year), max = max(data$year))
  }
    
  )
  
  observeEvent(input$region, {
    print(input$my_date)
    
  })
  
  my_reactive_df <- reactive({
    get_filtered_data(region_name = input$region, year = input$my_year)
  }) 
  
  output$table_out <- renderTable({
    my_reactive_df()
  })
  
  output$data_plot <- renderPlotly({
    plot_plotly(my_reactive_df())
  })
  
  output$simple_plot <- renderPlot({
    plot_ggplot(my_reactive_df())
  })
}

shinyApp(ui, server)
