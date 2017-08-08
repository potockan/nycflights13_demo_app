library(shiny)
library(shinydashboard)
library(dygraphs)
library(plotly)
library(DT)

function(request) {
  shinyUI(dashboardPage(skin = "blue",
                        dashboardHeader(title = "NYC Flights Analysis"),
                        dashboardSidebar(
                          sidebarMenu(id = "sidebarmenu",
                                      menuItem("Flights", icon = icon("line-chart"),
                                               menuSubItem("Graphs", tabName = "graphs1", icon = icon("bar-chart")),
                                               menuSubItem("Data", tabName = "data1", icon = icon("database"))
                                      )
                          ),
                          
                          dateRangeInput("daterange", "Date range",
                                         start = '2013-01-01', end ='2013-12-31',
                                         min = '2013-01-01', max ='2013-12-31',
                                         language = "en")
                        ),
                        
                        dashboardBody(id="body_app",
                                      helpText(paste0("This application presents the analysis of NYC flights from 2013. The purpose of the app is to find the cause of the flights delay. Inspiration for this app was taken from "), tags$a(href="https://soutik.github.io/NYC-Flight-Analysis/", "https://soutik.github.io/NYC-Flight-Analysis/")),
                                      tabItems(
                                        
                                        tabItem(tabName = "graphs1",
                                                fluidRow(
                                                  tabBox(title = tagList(shiny::icon("line-chart"),"flights"), width=12,
                                                         tabPanel("Flights",
                                                                  fluidRow(
                                                                    column(
                                                                      selectInput("delay_type",label = "Delay type",
                                                                                  choices = list(arrival = "arr_delay", departure = "dep_delay"),
                                                                                  selected = "arrival"),
                                                                      width = 6),
                                                                    column(
                                                                      selectInput("delay_range",label = "Delay range",
                                                                                  choices = list(all = "a", positive = "p", negative = "n"),
                                                                                  selected = "all"),
                                                                      width = 6)
                                                                  ),
                                                                  fluidRow(plotlyOutput("plotly_flights_delay_daily")),
                                                                  fluidRow(plotlyOutput("plotly_flights_delay_hourly")),
                                                                  fluidRow(plotOutput("corrplot"))
                                                         )
                                                  )
                                                )
                                        ),
                                        # Data
                                        tabItem(tabName = "data1",
                                                fluidRow(column(downloadButton('downloadData', 'Download data'), width = 12),
                                                         column(h2("Flights data"),
                                                                tags$div(style = "overflow-x:scroll", DT::dataTableOutput("data_flights")),
                                                                width = 12),
                                                         column(h2("Weather data"),
                                                                tags$div(style = "overflow-x:scroll", DT::dataTableOutput("data_weather")),
                                                                width = 12)
                                                ))
                                        
                                      )
                        )
  )
  )
}
