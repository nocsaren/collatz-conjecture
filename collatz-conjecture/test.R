library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Dynamic ggplot2 Example"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("slider", "Select a range:", min = 1, max = 10, value = 5)
    ),
    mainPanel(
      plotOutput("plot1")
    )
  )
)

server <- function(input, output) {
  # Create a reactive expression to generate dynamic data
  dynamic_data <- reactive({
    x <- seq(1, input$slider)
    y <- x^2
    data.frame(x, y)
  })
  
  # Render the ggplot graph based on the dynamic data
  output$plot1 <- renderPlot({
    ggplot(dynamic_data(), aes(x, y)) +
      geom_point() +
      ggtitle("Dynamic ggplot2 Graph")
  })
}

shinyApp(ui, server)


# asffasafs


server <- function(input, output) {
  load("../collatz_output.rda")
  
  dynamic_data <- reactive({
    x <- seq(input$number, 1)
    y <- collatz_table$n[collatz_table$step_number %in% x]
    number <- input$number
    
    data.frame(number = rep(number, length(x)), x = x, y = y)
  })
  
  output$plot1 <- renderPlot({
    ggplot(dynamic_data(), aes(x, y, group = number)) +
      geom_point(size = 2) +
      geom_line(color = "red")
  })
}