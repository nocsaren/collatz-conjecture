## app.R ##
library(shinydashboard)
library(tidyverse)
library(plotly)


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("th")),
      menuItem("Steps By Number", tabName = "by_number", icon = icon("th")),
      menuItem("Step Count By Number", tabName = "steps_number", icon = icon("th")),
      menuItem("Most Occurring Numbers as Steps", tabName = "occurrance", icon = icon("th"))
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      # Dashboard
      tabItem(tabName = "dashboard",
              fluidPage()),
      # By Number
      tabItem(tabName = "by_number",
              fluidPage(
                fluidRow("Steps By Number",
                  box("Steps By Number",
                      width = NULL)),
                fluidRow(
                  column(2,
                         box(
                           title = "Controls",
                           numericInput("number", 
                                        label = "Number to Plot", 
                                        value = 7), 
                           height = NULL,
                           width = NULL
                         )
                  ),
                  column(10,
                         box( 
                           title = "Graph",
                           plotOutput("plot1"), 
                           height = NULL,
                           width = NULL))
                ))),
      # Step Count By Number
      tabItem(tabName = "steps_number",
                        fluidPage(
                          fluidRow("Step Count By Number",
                            box("Step Count By Number",
                                width = NULL)),          
                          
                fluidRow(
                  column(2,
                         box(
                           title = "Controls",
                           sliderInput("range",
                                       label = "Interval",
                                       round = TRUE,
                                       min = 2,
                                       max = 5000,
                                       value = c(2000, 2025)),
                           width = NULL)),
                  column(10,
                         box(
                           plotlyOutput("plot2"), 
                           width = NULL))
                ))),
      # occurrace
      tabItem(tabName = "occurrance",
              fluidPage(
                fluidRow("Most Occurring Numbers as Steps",
                  box("Most Occurring Numbers as Steps",
                      width = NULL)),
                fluidRow(
                  column(2,
                         box(
                           "asdadsdsaads2")),
                  column(10,
                         box(
                           plotOutput("plot3"), 
                           width = NULL))
                )))
      ) #tabitems
    ) # body
  ) # page
server <- function(input, output) {
  load("collatz_output.rda")
  
  dynamic_data <- reactive({
    number <- input$number
    x <- seq(0, max(collatz_table$step_number[collatz_table$starting_number == number]))
    y <- collatz_table$n[collatz_table$starting_number == number]
    data.frame(x = x, y = y)
  })
  
  output$plot1 <- renderPlot({
    dynamic_data_df <- dynamic_data()

      ggplot(dynamic_data_df, aes(x = x, y = y, fill = "blue")) +
        geom_point(size = 2) +
        geom_line(color = "blue") +
        geom_text(aes(label = paste0("(",x,",",y,")")), vjust = -1, color = "red")+
        labs(x = "Steps", y = "n")+
        theme_light()
  })
      dynamic_data_2 <- reactive({
        x <- c(input$range[1]:input$range[2])
        y <- collatz_table %>% group_by(starting_number) %>% filter(starting_number %in% x) %>% select(starting_number, step_count) %>% distinct() %>% ungroup() %>% select(-starting_number)
      data.frame(x = x, y = y$step_count)
  })
      output$plot2 <- renderPlotly({
        data <- dynamic_data_2()
        

        p <- ggplot(data, aes(x = x, y = y)) +
          geom_col(fill = "blue", color= "blue")+
          labs(x = "Starting Number", y = "Step Count")

        

        ggplotly(p)
      })
}

shinyApp(ui, server)
