collatz <- function(max_number = 1000) {
  library(tidyverse)
  
  collatz_table <- tibble(starting_number = integer(0),
                          step_number = integer(0),
                          n = numeric(0),
                          step_count = numeric(0))
  for (starting_number in 2:max_number) {
    n <- starting_number
    step_number <- 0
    df <- data.frame(step_number = integer(0), 
                     n = numeric(0))
    df <- rbind(df, data.frame(step_number = step_number, n = n))
    while (n > 1) {
      if (n %% 2 == 0) {
        n <- n / 2
      } else {
        n <- 3 * n + 1
      }
      step_number <- step_number + 1
      df <- rbind(df, data.frame(step_number = step_number, n = n))
    }
    df$starting_number <- starting_number
    collatz_table <- bind_rows(collatz_table, df)
  }
  
  collatz_table <- collatz_table %>%
    group_by(starting_number) %>% 
    mutate(step_count = n() - 1)
  
  save(collatz_table, 
       file = "collatz_output.rda")
}
