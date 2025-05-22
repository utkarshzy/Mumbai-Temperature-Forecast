# Load required libraries
library(shiny)
library(httr)
library(jsonlite)
library(ggplot2)
library(plotly)

# Define UI for the dashboard
ui <- fluidPage(
  titlePanel("Mumbai Temperature Forecast (Open-Meteo API — Auto-Refresh)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Live temperature data refreshes every minute automatically."),
      verbatimTextOutput("last_updated")
    ),
    
    mainPanel(
      plotlyOutput("temp_plot"),
      verbatimTextOutput("raw_data")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store fetched data
  weather_data <- reactiveVal(NULL)
  last_update_time <- reactiveVal(NULL)
  
  # API URL
  api_url <- "https://api.open-meteo.com/v1/forecast?latitude=19.0728&longitude=72.8826&hourly=temperature_2m&timezone=Asia%2FSingapore"
  
  # Function to fetch data from API
  fetch_data <- function() {
    res <- GET(api_url)
    if (status_code(res) == 200) {
      json_data <- content(res, as = "text")
      parsed_data <- fromJSON(json_data)
      
      hourly <- parsed_data$hourly
      df <- data.frame(
        time = as.POSIXct(hourly$time, format="%Y-%m-%dT%H:%M", tz="Asia/Singapore"),
        temperature = hourly$temperature_2m
      )
      return(df)
    } else {
      return(NULL)
    }
  }
  
  # Timer to auto-fetch data every 60 sec (use 3600*1000 for hourly)
  autoInvalidate <- reactiveTimer(60000, session)  # 60000 ms = 60 sec
  
  observe({
    autoInvalidate()  # triggers this block at interval
    data <- fetch_data()
    if (!is.null(data)) {
      weather_data(data)
      last_update_time(Sys.time())
    }
  })
  
  # Plot output (BAR PLOT)
  output$temp_plot <- renderPlotly({
    data <- weather_data()
    if (is.null(data)) {
      return(NULL)
    }
    
    p <- ggplot(data, aes(x = time, y = temperature)) +
      geom_bar(stat = "identity", fill = "tomato") +
      labs(title = "Hourly Temperature Forecast in Mumbai (Auto-Updated Bar Plot)",
           x = "Time",
           y = "Temperature (°C)") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Show raw data (optional)
  output$raw_data <- renderPrint({
    data <- weather_data()
    if (!is.null(data)) head(data)
  })
  
  # Show last update time
  output$last_updated <- renderText({
    t <- last_update_time()
    if (is.null(t)) {
      "Waiting for first data fetch..."
    } else {
      paste("Last updated at:", format(t, "%Y-%m-%d %H:%M:%S"))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)