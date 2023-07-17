library(shiny)
library(plotly)

analyse_data <- function(file_path) {
  
  library(tidyverse)
  
  data <- read.table(file_path, header = FALSE, sep = "\t", stringsAsFactors = FALSE, skip = 43)
  
  df <- separate(data, V1, into = paste0("col", 1:12), sep = " ")
  
  df <- as.data.frame(lapply(df, function(x) as.numeric(as.character(x))))
  
  column_names <- c("sats", "time", "lat", "long", "velocity", "heading", "height", "vert-vel", "Tsample", "avifileindex", "avitime", "comboG")
  
  colnames(df) <- column_names
  
  plots <- list()
  
  plots$track <- ggplot(df, aes(long, lat)) +
    geom_path(color = "black") +
    scale_x_reverse() +
    coord_quickmap() +
    labs(title = "Track")
  
  plots$velocity_time <- ggplot(df, aes(x = time, y = velocity)) +
    geom_line() +
    labs(title = "Velocity Over Time", x = "Time", y = "Velocity")
  
  plots$vert_vel_time <- ggplot(df, aes(x = time, y = `vert-vel`)) +
    geom_line() +
    labs(title = "Vertical Velocity Over Time", x = "Time", y = "Vertical Velocity")
  
  plots$height_time <- ggplot(df, aes(x = time, y = height)) +
    geom_line() +
    labs(title = "Height Over Time", x = "Time", y = "Height")
  
  plots$comboG_time <- ggplot(df, aes(x = time, y = `comboG`)) +
    geom_line() +
    labs(title = "Combo G Over Time", x = "Time", y = "Combo G")
  
  return(plots)
}

ui <- fluidPage(
  titlePanel("Driving Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Please upload your first driving data file"),
      fileInput("file2", "Please upload your second driving data file")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Track", plotlyOutput("trackPlot1"), plotlyOutput("trackPlot2")),
        tabPanel("Velocity Over Time", plotlyOutput("velocityPlot1"), plotlyOutput("velocityPlot2")),
        tabPanel("Vertical Velocity Over Time", plotlyOutput("vertVelPlot1"), plotlyOutput("vertVelPlot2")),
        tabPanel("Height Over Time", plotlyOutput("heightPlot1"), plotlyOutput("heightPlot2")),
        tabPanel("Combo G Over Time", plotlyOutput("comboGPlot1"), plotlyOutput("comboGPlot2"))
      )
    )
  )
)


server <- function(input, output) {
  
  observeEvent(c(input$file1, input$file2), {
    req(input$file1, input$file2) 
    data1 <- analyse_data(input$file1$datapath)
    data2 <- analyse_data(input$file2$datapath)
    
    output$trackPlot1 <- renderPlotly({ ggplotly(data1$track) })
    output$trackPlot2 <- renderPlotly({ ggplotly(data2$track) })
    
    output$velocityPlot1 <- renderPlotly({ ggplotly(data1$velocity_time) })
    output$velocityPlot2 <- renderPlotly({ ggplotly(data2$velocity_time) })
    
    output$vertVelPlot1 <- renderPlotly({ ggplotly(data1$vert_vel_time) })
    output$vertVelPlot2 <- renderPlotly({ ggplotly(data2$vert_vel_time) })
    
    output$heightPlot1 <- renderPlotly({ ggplotly(data1$height_time) })
    output$heightPlot2 <- renderPlotly({ ggplotly(data2$height_time) })
    
    output$comboGPlot1 <- renderPlotly({ ggplotly(data1$comboG_time) })
    output$comboGPlot2 <- renderPlotly({ ggplotly(data2$comboG_time) })
  })
}


shinyApp(ui = ui, server = server)


