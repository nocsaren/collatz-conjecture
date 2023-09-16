## app.R ##
library(shinydashboard)
library(tidyverse)
library(plotly)
library(parallel)


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("th")),
      menuItem("Steps By Number", tabName = "by_number", icon = icon("th")),
      menuItem("Step Count By Number", tabName = "steps_number", icon = icon("th")),
      menuItem("Most Occurring Numbers in Steps", tabName = "occurrance", icon = icon("th")),
      menuItem("Starting Numbers Reaching Highest Numbers", tabName = "highest_reach", icon = icon("th")),
      menuItem("Starting Numbers with Highest Number of Steps", tabName = "highest_step_count", icon = icon("th"))
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
                                        value = 7,
                                        min = 2,
                                        max = 5000), 
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
                           numericInput("start", 
                                        "Start Value:", 
                                        value = 2000,
                                        min = 2,
                                        max = 5000),
                           numericInput("end", 
                                        "End Value:", 
                                        value = 2025,
                                        min = 2,
                                        max = 5000),
                           
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
                fluidRow("Most Occurring Numbers in Steps",
                  box("Most Occurring Numbers in Steps",
                      width = NULL)),
                fluidRow(
                  column(2,
                         box(title = "Controls",
                           sliderInput("occ_x",
                                       label = "Value",
                                       round = TRUE,
                                       min = 2,
                                       max = 500,
                                       value = 10),
                           width = NULL)),
                  column(10,
                         box(
                           plotOutput("plot3"), 
                           width = NULL))
                ))),
      # highest reach
      tabItem(tabName = "highest_reach",
              fluidPage(
                fluidRow("Starting Numbers Reaching Highest Numbers",
                         box("Starting Numbers Reaching Highest Numbers",
                             width = NULL)),
                fluidRow(
                  column(2,
                         box(title = "Controls",
                             sliderInput("high_n",
                                         label = "Interval",
                                         round = TRUE,
                                         min = 2,
                                         max = 5000,
                                         value = 2000),
                             width = NULL)),
                  column(10,
                         box(
                           plotOutput("plot4"), 
                           width = NULL))
                ))),
      # highest num of steps
      tabItem(tabName = "highest_step_count",
              fluidPage(
                fluidRow("Starting Numbers with Highest Number of Steps",
                         box("Starting Numbers with Highest Number of Steps",
                             width = NULL)),
                fluidRow(
                  column(2,
                         box(title = "Controls",
                             sliderInput("occ_ran",
                                         label = "Interval",
                                         round = TRUE,
                                         min = 2,
                                         max = 5000,
                                         value = 2000),
                             width = NULL)),
                  column(10,
                         box(
                           plotOutput("plot5"), 
                           width = NULL))
                )))
      ) #tabitems
    ) # body
  ) # page
server <- function(input, output, session) {
  load("collatz_output.rda")
  # Steps By Number
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
  # Step Count By Number
      dynamic_data_2 <- reactive({
        
        x_start <- if (!is.null(input$start)) input$start else input$range[1]
        x_end <- if (!is.null(input$end)) input$end else input$range[2]
        
        x <- x_start:x_end
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
  # Most Occurring Numbers in Steps
      dynamic_data_3 <- reactive({
        occ_x <- input$occ_x
        x <- data.frame(Numbers = unique(collatz_table$n))
        y <- x %>%
          group_by(Numbers) %>%
          summarize(Count = sum(Numbers == collatz_table$n)) %>%
          select(-Numbers) %>% data.frame()
        y$Count <- as.numeric(y$Count)
        dynamic_data_3 <- data.frame(x = x, y = y, occ_x = occ_x)
        
        dynamic_data_3_sorted <- dynamic_data_3 %>%
          arrange(desc(y)) %>%
          head(occ_x)
          
          colnames(dynamic_data_3_sorted) <- c("x", "y", "occ_x")
        
        dynamic_data_3_sorted
      })
      
      output$plot3 <- renderPlot({
        dynamic_data_df_3 <- dynamic_data_3()
        
        ggplot(dynamic_data_df_3, aes(x = reorder(x, -y), y = y )) +
          geom_col()
      })
  # Starting Numbers Reaching Highest Numbers
      dynamic_data_4 <- reactive({
        high_n <- input$high_n
        x <- data.frame(starting_number = collatz_table$starting_number)
        y <- collatz_table %>% filter(starting_number %in% x) %>% max(n) %>% data.frame()
          
          max(collatz_table$n[collatz_table$starting_number %in% x])
          
        dynamic_data_4 <- data.frame(x = x, y = y, high_n = high_n)
        
        dynamic_data_4_sorted <- dynamic_data_4 %>%
          arrange(desc(y)) %>%
          head(high_n)
      })
          
      output$plot4 <- renderPlot({
        dynamic_data_df_4 <- dynamic_data_4()
        
        ggplot(dynamic_data_df_4, aes(x = reorder(x, -y), y = y )) +
          geom_col()
      })
  # Starting Numbers with Highest Number of Steps
      observe({
        updateSliderInput(session, "range", value = c(input$start, input$end))
      })
      
      observe({
        updateNumericInput(session, "start", value = input$range[1])
        updateNumericInput(session, "end", value = input$range[2])
      })
}

shinyApp(ui, server)
