library(nycflights13)
library(dplyr)
library(corrplot)
library(openxlsx)

data(flights)
data(weather)

flights <- flights %>% 
  filter(!is.na(arr_delay)) %>% 
  mutate(dateymd = as.Date(as.character(time_hour)))

source("functions.R")



shinyServer(function(session, input, output) {
  
  
  
  flights1 <- reactive({
    
    flights %>%
      filter(dateymd >= input$daterange[1], 
             dateymd <= input$daterange[2]) %>% 
      rename_(.dots = setNames(input$delay_type, "delay")) %>% 
      choose_delay(input$delay_range)
  })  
  
  airports_num <- reactive({
    flights1() %>% .$origin %>% unique() %>% length()
  })
  
  flights_weather <- reactive({
    left_join(flights1(), weather) %>% 
      select(delay, temp, dewp, humid, wind_dir, wind_speed, wind_gust, precip, pressure, visib)
  })
  
  output$plotly_flights_delay_daily <- renderPlotly({
    plotly_delay(flights1(), "dateymd", input$delay_type, airports_num())
  })
  
  output$plotly_flights_delay_hourly <- renderPlotly({
    plotly_delay(flights1(), "hour", input$delay_type, airports_num())
  })
  
  output$corrplot <- renderPlot({
    
    corrplot(cor(na.omit(flights_weather())), method = "circle", type = "upper",
             tl.srt = 25, tl.col = "Black", tl.cex = 1, title = "Correlation
         between all 'weather' variables & 'delay'", mar = c(0, 0, 4, 0) + 0.2)
  })
  
  output$downloadData <- downloadHandler(
    
    filename = "Flights_weather_data.xlsx",
    
    content = function(file){
      wb <- createWorkbook()
      addWorksheet(wb = wb, sheetName = "Flights", gridLines = FALSE)
      writeDataTable(wb = wb, sheet = 1, x = flights1())
      addWorksheet(wb = wb, sheetName = "Weather", gridLines = FALSE)
      writeDataTable(wb = wb, sheet = 2, x = flights_weather())
      saveWorkbook(wb, file)
    },
    contentType = ".xlsx"
    
  )
  
  output$data_flights <- DT::renderDataTable(
    DT::datatable(flights1(),
                  extensions = 'Buttons',
                  escape = -0,
                  options = list(
                    lengthMenu = list(c(5, 10, 15), c('5', '10', '15')),
                    pageLength = 15,
                    dom = 'Bfrtip', buttons = I('colvis')
                  )
    )
  )
  
  output$data_weather <- DT::renderDataTable(
    DT::datatable(flights_weather(),
                  extensions = 'Buttons',
                  escape = -0,
                  options = list(
                    lengthMenu = list(c(5, 10, 15), c('5', '10', '15')),
                    pageLength = 15,
                    dom = 'Bfrtip', buttons = I('colvis')
                  )
    )
  )
  
  
})
