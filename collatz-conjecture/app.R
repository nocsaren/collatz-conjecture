## app.R ##
library(shinydashboard)
library(tidyverse)
library(plotly)


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Collatz Conjecture 1:5000"),
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Dashboard", tabName = "dashboard", icon = icon("house")),
      menuItem("Paths of Numbers", tabName = "by_number", icon = icon("chart-line")),
      menuItem("Many Ways to Reach One", tabName = "steps_number", icon = icon("chart-column")),
      menuItem("Most Popular Stops", tabName = "occurrance", icon = icon("chart-column")),
      menuItem("Punching Above One's Height", tabName = "highest_reach", icon = icon("chart-column")),
      menuItem("The Wanderers", tabName = "highest_step_count", icon = icon("chart-column"))
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      # Dashboard
      tabItem(tabName = "dashboard",
              fluidPage(
                fluidRow(
                         box(background = "light-blue",
                             width = NULL,
                             solidHeader = FALSE,
                             tags$h2("Collatz Conjecture"),
                             p(style = "text-align: justify", "The Collatz conjecture is a famous unsolved problem in mathematics that asks whether repeating two simple arithmetic operations will eventually transform every positive integer into 1."),
                             p(style = "text-align: justify", "The operations are: if the number is even, divide it by two; if the number is odd, triple it and add one."),
                             p(style = "text-align: justify", "The conjecture was proposed by Lothar Collatz in 1937 and has not been proven or disproven yet.")),
                         box(width = NULL,
                             p(style = "text-align: justify", "I created this app to play around with the pathways of the starting numbers from 2 to 5000."),
                             p(style = "text-align: justify", "I built a simple function in R to do the operations on each number < 5001 and get the change of value on each step for each number. The loop also counted the number of operations needed to reach 1 for each number. Following that, the function write these four values (starting number, step number, number change [n], and step count) to a dataframe, ready to be worked on."),
                             p(style = "text-align: justify", "The shiny app you are now in loads the created dataframe and visualizes them with respect to some relations you can see on the left sidebar."),
                             p(style = "text-align: justify", "The data and visualizations in the app, albeit are right, are not meant to make mathematical sense, they are only built for exploration and visualization.")),
                         box(background = "light-blue",
                           width = NULL,
                           p(
                             style = "text-align: justify;",
                             "Source file to create the dataframe can be found ",
                             a(href = "https://github.com/nocsaren/collatz-conjecture/blob/main/collatz-conjecture/collatz_function.R", style = "color: white;", "here"),
                             "."
                             
                           ),
                           p(
                             style = "text-align: justify;",
                             "Dataframe itself is ",
                             a(href = "https://github.com/nocsaren/collatz-conjecture/blob/main/collatz-conjecture/collatz_output.rda", style = "color: white;", "here"),
                             ". (.Rda File)"))
              
                ))),
      # Paths of Numbers
      tabItem(tabName = "by_number",
              fluidPage(
                fluidRow(
                  box(background = "light-blue",
                      width = NULL,
                      solidHeader = FALSE,
                      tags$h2("Paths of Numbers"),
                      p(style = "text-align: justify", "Every number which ever tried to run in the operations eventually found its way to 1, but it is not yet proven wheather this is true for all numbers or not. As one can guess, numbers like 128 or 256 reach 1 easily, but some numbers, even relatively small ones like 27, need many steps to finish its path. On the other hand, numbers 26,  28, even the prime 29 reach 1 quite easily."))),
                fluidRow(
                  column(2,
                         box(
                           title = "Number to Plot",
                           numericInput("number",
                                        label = "",
                                        value = 7,
                                        min = 2,
                                        max = 5000), 
                           width = NULL,
                           height = "100%"
                         )
                  ),
                  column(10,
                         box( 
                           title = "Pathway",
                           plotlyOutput("plot1"), 
                           width = NULL,
                           height = "auto"))
                ),
              fluidRow(
                box(background = "light-blue",
                    width = NULL,
                    solidHeader = FALSE,
                    p(style = "text-align: justify", "Above, you can plot the path of every number from 2 to 5000. Select or type a number in the left box to plot."),
                    p(style = "text-align: justify", "You can hover on points for more details.")
                    )))),

        # Many Ways to Reach One
      tabItem(tabName = "steps_number",
                        fluidPage(
                          fluidRow(
                            box(background = "light-blue",
                                width = NULL,
                                solidHeader = FALSE,
                                tags$h2("Many Ways to Reach One"),
                                p(style = "text-align: justify", "Although the number of steps to reach 1 vary from number to number, there is no known pattern of relation between the starting number and the number of steps needed to reach 1. One can intuitively say that as the starting number grows, number of steps must grow too, but as it can be seen in the default graph below, many of the numbers in the interval still have relatively small step counts."))),
                          
                          
                fluidRow(
                  column(2,
                         box(
                           title = "Controls",
                           sliderInput("range",
                                       label = "Interval",
                                       round = TRUE,
                                       min = 2,
                                       max = 5000,
                                       value = c(1550, 1900)),
                           numericInput("start", 
                                        "Start Value:", 
                                        value = 1550,
                                        min = 2,
                                        max = 5000),
                           numericInput("end", 
                                        "End Value:", 
                                        value = 1900,
                                        min = 2,
                                        max = 5000),
                           width = NULL)),
                  column(10,
                         box(
                           title = "Graph",
                           plotlyOutput("plot2"), 
                           width = NULL))
                )),
              fluidPage(
                fluidRow(
                  box(background = "light-blue",
                      width = NULL,
                      p(style = "text-align: justify", "You can select a custom interval with the left side slider. If you want to fine tune your input (or you are on a mobile device) you can type the start and end numbers (between 2 and 5000) in the boxes."),
                      p(style = "text-align: justify", "You can hover on a column for more details."))),
              )),
      # Most Popular Stops
      tabItem(tabName = "occurrance",
              fluidPage(fluidRow(
                box(background = "light-blue",
                    width = NULL,
                    solidHeader = FALSE,
                    tags$h2("Most Popular Stops"),
                    p(
                      style = "text-align: justify;",
                      "As can be expected, we see some numbers more often than others as being steps to one. For example, numbers in the form of 2",
                      tags$sup("n"),
                      " are more likely to occur. Also, from the graph below, we can guess that the numbers in the form of 20k have a tendency of occurring as steps."
                    ))),

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
                           width = NULL))),
                fluidRow(
                  box(background = "light-blue",
                      width = NULL,
                      p(style = "text-align: justify", "Above, you see the most popular 20 steps visited by numbers between 2 and 5000. You can select the number of steps shown on the graph with the slider on the left side. Maximum number of numbers shown is limited to 500 in this graph for performance reasons."),
                      p(style = "text-align: justify", "You can hover on a column for more details.")))
                )),
      # Punching Above One's Height
      tabItem(tabName = "highest_reach",
              fluidPage(
                fluidRow(
                  box(background = "light-blue",
                      width = NULL,
                      solidHeader = FALSE,
                      tags$h2("Punching Above One's Height"),
                      p(style = "text-align: justify", "Although the conjecture states that starting from any number, the sequence of operations will eventually take us to 1; there are some numbers which, before starting their way to one, keep going higher and higher with their 3n+1 operations."))),
                
               
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
                ),
                fluidRow(
                  box(background = "light-blue",
                      width = NULL,
                      p(style = "text-align: justify", "Above, you see 20 starting numbers which, on their way to one, go up to highest numbers between 2 and 5000. You can select the number of starting numbers shown on the graph with the slider on the left side. Again, maximum number of starting numbers shown is limited to 500 for performance reasons."),
                      p(style = "text-align: justify", "You can hover on a column for more details."))))),
      # The Wanderers
      tabItem(tabName = "highest_step_count",
              fluidPage(
                fluidRow(
                  box(background = "light-blue",
                      width = NULL,
                      solidHeader = FALSE,
                      tags$h2("The Wanderers"),
                      p(style = "text-align: justify", "Also, some numbers take much more steps to reach their final destination, 1."))),
                
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
                ),
                fluidRow(
                  box(background = "light-blue",
                      width = NULL,
                      p(style = "text-align: justify", "Above, you see 20 starting numbers (between 2 and 5000) which take the most number of steps on their way to reaching one. You can select the number of starting numbers shown on the graph with the slider on the left side with a maximum of 500."),
                      p(style = "text-align: justify", "You can hover on a column for more details.")))))
      ) #tabitems
    ) # body
  ) # dashboardpage
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
        geom_line(color = "#367fa9") +
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
          geom_col(fill = "#367fa9")+
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
          geom_col(fill = "#367fa9")+
          labs(x = "Number", y = "Occurring Count")+
          theme(axis.text.x = element_blank())
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
          geom_col(fill = "#367fa9")+
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
          geom_col(fill = "#367fa9")+
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
