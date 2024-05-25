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

mydata <- get_data()

ui <-dashboardPage(
  skin = "black",
  dashboardHeader(title = 'Political & Civil liberties scores around the globe'),
  
  dashboardSidebar(
    sidebarMenu(
      h5('Controls'),
      uiOutput("my_region"),
      uiOutput("my_country"),
      uiOutput("my_year"),
      sliderInput("range", "Filter by score:",
                  min = min(mydata$CL), max = max(mydata$CL),
                  value = c(min(mydata$CL),max(mydata$CL))),
      menuItem("Plotly chart on PR", tabName = "plotly", icon = icon("dashboard")),
      menuItem("Data table", tabName = "data", icon = icon("th")),
      menuItem("Ggplot chart on CL", tabName = "ggplot", icon = icon("dashboard")),
      menuItem("DT", tabName = "dt_data", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "plotly",
              h1('Civil liberties data for selected region, countries and year - lower = better'),
              plotlyOutput("data_plot")
      ),
      tabItem(tabName = "data",
              h2('This is a data table with all relevant datapoints'),
              tableOutput("table_out")
      ),
      tabItem(tabName = "ggplot",
              h2("Civil liberties data for selected region, countries and year - lower = better"),
              plotOutput("simple_plot")
              ),
      tabItem(tabName = "dt_data",
              DT::dataTableOutput('table_out_dt')
              )
      ),
    valueBoxOutput("progressBox_free"),
    valueBoxOutput("progressBox_partfree"),
    valueBoxOutput("progressBox_nonfree")
    )
  )



server <- function(input, output, session) {
  output$my_region <- renderUI({
    selectInput("region", "Select a region", choices = unique(mydata$Region_Name),
                selected = 'Asia', multiple = FALSE)
  })
  
  output$my_year <- renderUI({
    numericInput("my_year", label = "Year", 2000, min = min(mydata$year), max = max(mydata$year))
  })
  
  output$my_country <- renderUI({
    selectInput("my_country", label = "Country/-ies", choices = unique(mydata$country), multiple = TRUE)
  })
  
  output$progressBox_free <- renderValueBox({filtered_data <- mydata[mydata$Status == "F" & mydata$Region_Name == input$region & mydata$year == input$my_year & mydata$country %in% input$my_country & mydata$CL <= input$range[2] & mydata$CL >= input$range[1], ]
  
  free_countries_count <- nrow(filtered_data)
  
  valueBox(
    paste0("FREE: ", free_countries_count), "From selected countries", icon = icon("list"),
    color = "green"
  )
  })
  
  output$progressBox_partfree <- renderValueBox({filtered_data2 <- mydata[mydata$Status == "PF" & mydata$Region_Name == input$region & mydata$year == input$my_year & mydata$country %in% input$my_country & mydata$CL <= input$range[2] & mydata$CL >= input$range[1], ]
  
  part_free_countries_count <- nrow(filtered_data2)
  
  valueBox(
    paste0("PARTIALLY FREE: ", part_free_countries_count), "From selected countries", icon = icon("list"),
    color = "orange"
  )
  })
  
  output$progressBox_nonfree <- renderValueBox({filtered_data3 <- mydata[mydata$Status == "NF" & mydata$Region_Name == input$region & mydata$year == input$my_year & mydata$country %in% input$my_country & mydata$CL <= input$range[2] & mydata$CL >= input$range[1], ]
  
  non_free_countries_count <- nrow(filtered_data3)
  
  valueBox(
    paste0("NON-FREE: ", non_free_countries_count), "From selected countries", icon = icon("list"),
    color = "red"
  )
  })
  
  observeEvent(input$region, {
    selected_region <- input$region
    filtered_countries <- unique(mydata[mydata$Region_Name == selected_region, ]$country)
    
    updateSelectInput(session = session, inputId = "my_country", 
                      choices = filtered_countries, selected = filtered_countries[1])
  })
  
  my_reactive_df <- reactive({
    get_filtered_data(region_name = input$region, year = input$my_year, country = input$my_country, min_score = input$range[1], max_score = input$range[2])
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
  
  output$table_out_dt = DT::renderDataTable({
    datatable(
      my_reactive_df(), extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ))})
  
}

shinyApp(ui, server)
