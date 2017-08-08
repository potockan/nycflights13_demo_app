library(testthat)
library(nycflights13)

flights <- flights %>% 
  filter(!is.na(arr_delay)) %>% 
  mutate(dateymd = as.Date(as.character(time_hour)))

test_that("Test choose_delay(p)",{
  flights <- flights %>% rename(delay = arr_delay)
  expect_equal(min(choose_delay(flights, "p")$delay), 0)
  expect_error(choose_delay(7, 2))
})



test_that("Test simpleCap()",{
  expect_equal(simpleCap("a cat is looking at a snack"), "A Cat Is Looking At A Snack")
  expect_error(simpleCap(2))
})

test_that("Test plotly_delay(hour, arr_delay",{
  
  expected_output <- flights %>%
    group_by(origin, hour) %>%
    summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
    rename(time = hour) %>% 
    plot_ly(x = ~time, y = ~delay) %>%
    add_markers(color = ~origin, colors = rainbow(3), showlegend = TRUE) %>%
    add_lines(y = ~ fitted(loess(delay ~ time)), name = "Loess smoother") %>%
    layout(xaxis = list(title = "Hour"), yaxis = list(title = "Delay"),
           title = "Average arr_delay by hour")
  
  flights <- flights %>% rename(delay = arr_delay)
  fun_output <- plotly_delay(flights, "hour", "arr_delay", 3)
  
  expect_identical(plotly_build(fun_output)$x$data, 
                   plotly_build(expected_output)$x$data)
  
  
})

test_that("Test plotly_delay(dateymd, dep_delay",{
  
  expected_output <- flights %>%
    group_by(origin, dateymd) %>%
    summarise(delay = mean(dep_delay, na.rm = TRUE)) %>%
    rename(time = dateymd) %>% 
    plot_ly(x = ~time, y = ~delay) %>%
    add_markers(color = ~origin, colors = rainbow(3), showlegend = TRUE) %>%
    add_lines(y = ~ fitted(loess(delay ~ as.numeric(time))), name = "Loess smoother") %>%
    layout(xaxis = list(title = "dateymd"), yaxis = list(title = "Delay"),
           title = "Average dep_delay by dateymd")
  
  flights <- flights %>% rename(delay = dep_delay)
  fun_output <- plotly_delay(flights, "dateymd", "dep_delay", 3)
  
  expect_identical(plotly_build(expected_output)$x$data, 
                   plotly_build(fun_output)$x$data)
  
  
})

