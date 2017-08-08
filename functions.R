library(dplyr)
library(plotly)


choose_delay <- function(flights, range){
  stopifnot(is.data.frame(flights), range %in% c("a", "n", "p"))
  
  delay_range <- switch (range , 
                         "p" = list(delay_min = 0, delay_max = Inf),
                         "n" = list(delay_min = -Inf, delay_max = 0),
                         "a" = list(delay_min = -Inf, delay_max = Inf))
  flights %>% 
    filter(delay >= delay_range$delay_min, 
           delay <= delay_range$delay_max)
}



simpleCap <- function(x) {
  stopifnot(is.character(x))
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


plotly_delay <- function(flights_in, groupby_var, delay_type, airports_num){
  
  stopifnot(is.data.frame(flights_in), is.character(groupby_var), is.character(delay_type), is.numeric(airports_num),
            length(groupby_var) == 1, length(delay_type) == 1, length(airports_num) == 1)
  
  flights_in %>%
    group_by_(quote(origin), .dots = groupby_var) %>%
    #summarise_(delay = lazyeval::interp(~mean(var, na.rm = TRUE), var = as.name(delay))) %>%
    summarise(delay = mean(delay, na.rm = TRUE)) %>%
    rename_(.dots = setNames(groupby_var, "time")) %>% 
    plot_ly(x = ~time, y = ~delay) %>%
    add_markers(color = ~origin, colors = rainbow(airports_num), showlegend = TRUE) %>%
    add_lines(y = ~ fitted(loess(as.formula(delay ~ as.numeric(time)))), name = "Loess smoother") %>%
    layout(xaxis = list(title = simpleCap(groupby_var)), yaxis = list(title = "Delay"),
           title = sprintf("Average %s by %s", delay_type, groupby_var))
  
}
