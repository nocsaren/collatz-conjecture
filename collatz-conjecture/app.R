## app.R ##
library(shinydashboard)
library(tidyverse)
library(plotly)
library(parallel)


ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Collatz Sequence 1:5000"),
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Dashboard", tabName = "dashboard", icon = icon("th")),
      menuItem("Steps By Number", tabName = "by_number", icon = icon("th")),
      menuItem("Step Count By Number", tabName = "steps_number", icon = icon("th")),
      menuItem(HTML("Most Occurring Numbers <br/>in Steps"), tabName = "occurrance", icon = icon("th")),
      menuItem(HTML("Starting Numbers <br/>Reaching Highest Numbers"), tabName = "highest_reach", icon = icon("th")),
      menuItem(HTML("Starting Numbers <br/>with Highest Number of Steps"), tabName = "highest_step_count", icon = icon("th"))
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      # Dashboard
      tabItem(tabName = "dashboard",
              fluidPage("some boxes will be here with some images"
              )),
      # By Number
      tabItem(tabName = "by_number",
              fluidPage(
                fluidRow("Steps By Number",
                  box("explain what the controls and graph are doing",
                      width = NULL,
                      background = "green")),
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
                           width = NULL,
                           background = "green"
                         )
                  ),
                  column(10,
                         box( 
                           title = "Graph",
                           plotlyOutput("plot1"), 
                           height = NULL,
                           width = NULL,
                           background = "green"))
                ))),
      # Step Count By Number
      tabItem(tabName = "steps_number",
                        fluidPage(
                          fluidRow("Step Count By Number",
                            box("explain what the controls and graph are doing",
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
                           title = "Graph",
                           plotlyOutput("plot2"), 
                           width = NULL))
                ))),
      # occurrace
      tabItem(tabName = "occurrance",
              fluidPage(
                fluidRow("Most Occurring Numbers in Steps",
                  box("explain what the controls and graph are doing",
                      width = NULL)),
                fluidRow(
                  column(2,
                         box(title = "Controls",
                           sliderInput("occ_x",
                                       label = "Value",
                                       round = TRUE,
                                       min = 1,
                                       max = 500,
                                       value = 20),
                           width = NULL)),
                  column(10,
                         box(
                           title = "Graph",
                           plotlyOutput("plot3"), 
                           width = NULL))
                ))),
      # highest reach
      tabItem(tabName = "highest_reach",
              fluidPage(
                fluidRow("Starting Numbers Reaching Highest Numbers",
                         box("explain what the controls and graph are doing",
                             width = NULL)),
                fluidRow(
                  column(2,
                         box(title = "Controls",
                             sliderInput("high_n",
                                         label = "Interval",
                                         round = TRUE,
                                         min = 1,
                                         max = 500,
                                         value = 20),
                             width = NULL)),
                  column(10,
                         box(
                           title = "Graph",
                           plotlyOutput("plot4"), 
                           width = NULL))
                ))),
      # highest num of steps
      tabItem(tabName = "highest_step_count",
              fluidPage(
                fluidRow("Starting Numbers with Highest Number of Steps",
                         box("explain what the controls and graph are doing",
                             width = NULL)),
                fluidRow(
                  column(2,
                         box(title = "Controls",
                             sliderInput("high_step",
                                         label = "Interval",
                                         round = TRUE,
                                         min = 1,
                                         max = 500,
                                         value = 20),
                             width = NULL)),
                  column(10,
                         box(
                           plotlyOutput("plot5"), 
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
  
  output$plot1 <- renderPlotly({
    dynamic_data_df <- dynamic_data()

      o <- ggplot(dynamic_data_df, aes(x = x, y = y)) +
        geom_point() +
        geom_line(color = "blue") +
        labs(x = "Steps", y = "n")
      plotly_plot <- ggplotly(o)
      
      custom_labels <- paste("Step: ", dynamic_data_df$x, "<br>Number: ", scales::comma(dynamic_data_df$y))
      plotly_plot <- plotly_plot %>%
        style(text = custom_labels, hoverinfo = "text")
      
      plotly_plot
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
        dynamic_data_df_2 <- dynamic_data_2()
        

        p <- ggplot(dynamic_data_df_2, aes(x = x, y = y)) +
          geom_col()+
          labs(x = "Starting Number", y = "Step Count")+
          scale_y_continuous(labels = scales::comma)
        

        plotly_plot <- ggplotly(p)
        
        custom_labels <- paste("Starting Number: ", dynamic_data_df_2$x, "<br>Step Count: ", scales::comma(dynamic_data_df_2$y))
        
        plotly_plot <- plotly_plot %>%
          style(text = custom_labels, hoverinfo = "text")
        
        plotly_plot
      })
  
  # Most Occurring Numbers in Steps
      dynamic_data_3 <- reactive({
        occ_x <- input$occ_x
        occ_df <- collatz_table %>%
          group_by(n) %>%
          summarize(Count = n()) %>%
          ungroup() %>%
          rename(Numbers = n) %>%
          as.data.frame()
        x <- occ_df$Numbers
        y <- occ_df$Count

        dynamic_data_3 <- data.frame(x = x, y = y, occ_x = occ_x)
        
        dynamic_data_3_sorted <- dynamic_data_3 %>%
          arrange(desc(y)) %>%
          head(occ_x)
          
          colnames(dynamic_data_3_sorted) <- c("x", "y", "occ_x")
        
        dynamic_data_3_sorted
      })
      
      output$plot3 <- renderPlotly({
        dynamic_data_df_3 <- dynamic_data_3()
        
        q <- ggplot(dynamic_data_df_3, aes(x = reorder(x, -y), y = y )) +
          geom_col()+
          labs(x = "Number", y = "Occurring Count")
        plotly_plot <- ggplotly(q)
        

        custom_labels <- paste("Number: ", dynamic_data_df_3$x, "<br>Count: ", dynamic_data_df_3$y)
        
        plotly_plot <- plotly_plot %>%
          style(text = custom_labels, hoverinfo = "text")
        
        plotly_plot
      })
      
  # Starting Numbers Reaching Highest Numbers
      dynamic_data_4 <- reactive({
        high_n <- input$high_n
        x <- data.frame(starting_number = as.numeric(unique(collatz_table$starting_number)))
        y <- collatz_table %>%
          group_by(starting_number) %>%
          summarise(max_n = max(n)) %>% select(-starting_number)
          
       
        dynamic_data_4 <- data.frame(x = x, y = y, high_n = high_n)
        
        dynamic_data_4_sorted <- dynamic_data_4 %>%
          arrange(desc(y)) %>%
          head(high_n)
        colnames(dynamic_data_4_sorted) <- c("x", "y", "high_n")
        
        dynamic_data_4_sorted
      })
          
      output$plot4 <- renderPlotly({
        dynamic_data_df_4 <- dynamic_data_4()
        
        r <- ggplot(dynamic_data_df_4, aes(x = reorder(x, -y), y = y )) +
          geom_col()+
          labs(x = "Starting Number", y = "Highest Number Reached in a Step")+
          theme(axis.text.x = element_blank())+
          scale_y_continuous(labels = scales::comma)
        
        plotly_plot <- ggplotly(r)
        
        custom_labels <- paste("Starting Number: ", dynamic_data_df_4$x, "<br>Highest Number Reached in a Step: ", scales::comma(dynamic_data_df_4$y))
        
        plotly_plot <- plotly_plot %>%
          style(text = custom_labels, hoverinfo = "text")
        
        plotly_plot
        
      })
  # Starting Numbers with Highest Number of Steps
      dynamic_data_5 <- reactive({
        high_step <- input$high_step
        x <- data.frame(starting_number = as.numeric(unique(collatz_table$starting_number)))
        y <- collatz_table %>%
          group_by(starting_number) %>%
          summarise(max_n = max(step_count)) %>% select(-starting_number)
        
        
        dynamic_data_5 <- data.frame(x = x, y = y, high_step = high_step)
        
        dynamic_data_5_sorted <- dynamic_data_5 %>%
          arrange(desc(y)) %>%
          head(high_step)
        colnames(dynamic_data_5_sorted) <- c("x", "y", "high_step")
        
        dynamic_data_5_sorted
      })
      
      output$plot5 <- renderPlotly({
        dynamic_data_df_5 <- dynamic_data_5()
        
        s <- ggplot(dynamic_data_df_5, aes(x = reorder(x, -y), y = y )) +
          geom_col()+
          labs(x = "Starting Number", y = "Number of Steps to Reach 1")+
          theme(axis.text.x = element_blank())
        
        plotly_plot <- ggplotly(s)
        
        custom_labels <- paste("Starting Number: ", dynamic_data_df_5$x, "<br>Number of Steps to Reach 1: ", dynamic_data_df_5$y)
        
        plotly_plot <- plotly_plot %>%
          style(text = custom_labels, hoverinfo = "text")
        
        plotly_plot
        
      })
      
  # Observers    
      observe({
        updateSliderInput(session, "range", value = c(input$start, input$end))
      })
      
      observe({
        updateNumericInput(session, "start", value = input$range[1])
        updateNumericInput(session, "end", value = input$range[2])
      })
}

shinyApp(ui, server)
