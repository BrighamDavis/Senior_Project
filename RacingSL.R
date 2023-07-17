library(plotly)
analyse_data <- function(file_path) {
  
  library(tidyverse)
  
  data <- read.table(file_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE, skip = 62)
  
  df <- separate(data, X.data., into = paste0("col", 1:22), sep = " ")
  
  df <- as.data.frame(lapply(df, function(x) as.numeric(as.character(x))))
  
  column_names <- c("sats", "time", "lat", "long", "velocity", "heading", "height", "vert-vel", "Tsample", "avifileindex", "avitime", "Brake_Pressure", "Engine_Speed", "External_Lap_Number", "External_Lap_Time", "Gear", "Indicated_Lateral_Acceleration", "Indicated_Longitudinal_Acceleration", "Indicated_Vertical_Acceleration", "Steering_Angle", "Throttle_Position", "Yaw_Rate")
  
  colnames(df) <- column_names
  
  plots <- list()
  
  plots$track <- ggplot(df, aes(long, lat)) +
    geom_polygon(fill = "white", colour = "black") +
    scale_x_reverse() +
    coord_quickmap()
  
  plots$track_efficiency <- ggplot(df, aes(x = time, y = velocity)) +
    geom_line() +
    labs(title = "Speed over Time", x = "Time", y = "Speed (km/h)")
  
  plots$brake_analysis <- ggplot(df) +
    geom_line(aes(x = time, y = Brake_Pressure, color = "Brake Pressure")) +
    geom_line(aes(x = time, y = velocity, color = "Speed")) +
    labs(title = "Braking Analysis", x = "Time", y = "Brake Pressure")
  
  plots$brake_map <- ggplot(df, aes(x = long, y = lat, color = Brake_Pressure > 0)) +
    geom_point(alpha=0.2) +
    labs(title = "Braking Points", x = "Longitude", y = "Latitude", color = "Braking")
  
  plots$gear_throttle <- ggplot(df, aes(x = Gear, y = Engine_Speed, color = Throttle_Position)) +
    geom_point(position="jitter") +
    labs(title = "Gear Usage vs. Engine Speed and Throttle Position", x = "Gear", y = "Engine Speed (RPM)")
  
  plots$cornering <- ggplot(df, aes(x = long, y = lat)) +
    geom_hex(bins = 200) +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(title = "Driving Line Heatmap", x = "Longitude", y = "Latitude")
  
  plots$cornering_speed <- ggplot(df, aes(x = Steering_Angle, y = Indicated_Lateral_Acceleration, color = velocity)) +
    geom_point() +
    scale_color_gradient(low = "blue", high = "red") +
    labs(title = "Cornering with Speed", x = "Steering Angle", y = "Lateral Acceleration", color = "Speed (km/h)")
  
  plots$engine_speed <- ggplot(df, aes(x = time, y = Engine_Speed)) +
    geom_line() +
    labs(title = "Engine Speed over Time", x = "Time", y = "Engine Speed (RPM)")
  
  return(plots)
}

ui <- fluidPage(
  titlePanel("Driving Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Please upload your driving data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Track", plotlyOutput("trackPlot")),
        tabPanel("Track Efficiency", plotlyOutput("trackEfficiencyPlot")),
        tabPanel("Braking Analysis", plotlyOutput("brakeAnalysisPlot")),
        tabPanel("Braking Map Analysis", plotlyOutput("gearBreakMap")),
        tabPanel("Gear vs Engine Speed and Throttle Position", plotlyOutput("gearThrottlePlot")),
        tabPanel("Driving Line", plotlyOutput("corneringPlot")),
        tabPanel("Cornering with Speed", plotlyOutput("corneringSpeedPlot")),
        tabPanel("Engine Speed over Time", plotlyOutput("engineSpeedPlot"))
      )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$file, {
    req(input$file) 
    data <- analyse_data(input$file$datapath)
    
    output$trackPlot <- renderPlotly({
      ggplotly(data$track)
    })
    
    output$trackEfficiencyPlot <- renderPlotly({
      ggplotly(data$track_efficiency)
    })
    
    output$brakeAnalysisPlot <- renderPlotly({
      ggplotly(data$brake_analysis)
    })
    
    output$BrakeMapt <- renderPlotly({
      ggplotly(data$gear_engine)
    })
    
    output$gearThrottlePlot <- renderPlotly({
      ggplotly(data$gear_throttle)
    })
    
    output$corneringPlot <- renderPlotly({
      ggplotly(data$cornering)
    })
    
    output$corneringSpeedPlot <- renderPlotly({
      ggplotly(data$cornering_speed)
    })
    
    output$engineSpeedPlot <- renderPlotly({
      ggplotly(data$engine_speed)
    })
  })
}

shinyApp(ui = ui, server = server)

