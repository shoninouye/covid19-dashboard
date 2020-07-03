#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinydashboard)

# Tab 1: Global map
global_tab <- tabPanel("Global Map",
                       # Sidebar
                       fluidRow(
                         column(width = 3, style = "padding-bottom:50px",
                                # Static current global stats
                                box(width = NULL,
                                    verticalLayout(
                                      valueBoxOutput("global_cases_box", width = 12),
                                      valueBoxOutput("global_deaths_box", width = 12),
                                      valueBoxOutput("global_recovered_box", width = 12)
                                    )
                                ),
                                br(),
                                # Country/region table
                                box(width = NULL,
                                    tabsetPanel(
                                      tabPanel("Cases", DTOutput("global_cases_df")),
                                      tabPanel("Deaths", DTOutput("global_deaths_df")),
                                      tabPanel("Recoveries", DTOutput("global_recovered_df"))
                                    )
                                )
                         ),
                         
                         # Global map
                         column(width = 9,
                                tabsetPanel(
                                  tabPanel("Cases", leafletOutput("global_map_cases", height = 900)),
                                  tabPanel("Deaths", leafletOutput("global_map_deaths", height = 900)),
                                  tabPanel("Recoveries", leafletOutput("global_map_recovered", height = 900))
                                )
                         )
                       )
)

# Tab 2: US map
us_tab <- tabPanel("U.S. Map",
                       # Sidebar
                       fluidRow(
                         column(width = 3, style = "padding-bottom:50px",
                                # Static current US stats
                                box(width = NULL,
                                    verticalLayout(
                                      valueBoxOutput("us_cases_box", width = 12),
                                      valueBoxOutput("us_deaths_box", width = 12)
                                    )
                                ),
                                br(),
                                # US states table
                                box(width = NULL,
                                    tabsetPanel(
                                      tabPanel("Cases", DTOutput("us_cases_df")),
                                      tabPanel("Deaths", DTOutput("us_deaths_df"))
                                    )
                                )
                         ),
                         
                         # US map
                         column(width = 9,
                                tabsetPanel(
                                  tabPanel("Cases", leafletOutput("us_map_cases", height = 900)),
                                  tabPanel("Deaths", leafletOutput("us_map_deaths", height = 900))
                                )
                         )
                       )
)

# Tab 3: Trends
trends_tab <- tabPanel("Trends",
                       # Daily metrics over time
                       column(width = 6,
                              # Global daily
                              h2("Global Daily"),
                              br(),
                              tabsetPanel(
                                id = "global_daily",
                                tabPanel("Cases", value = "cases", plotlyOutput("global_daily_cases")),
                                tabPanel("Deaths", value = "deaths", plotlyOutput("global_daily_deaths")),
                                tabPanel("Recoveries", value = "recovered", plotlyOutput("global_daily_recoveries"))
                              ),
                              # Country/region (default: US) daily
                              h2("Country/region Daily"),
                              selectInput("region_selection_daily",
                                          "",
                                          choices = distinct(covid19_global_full, country_region),
                                          selected = "United States"),
                              tabsetPanel(
                                id = "region_daily",
                                tabPanel("Cases", value = "cases", plotlyOutput("region_daily_cases")),
                                tabPanel("Deaths", value = "deaths", plotlyOutput("region_daily_deaths")),
                                tabPanel("Recoveries", value = "recovered", plotlyOutput("region_daily_recoveries"))
                              )

                       ),
                       # Cumulative total metrics over time
                       column(width = 6,
                              # Global cumulative totals
                              h2("Global Cumulative Total"),
                              br(), #br(), br(),
                              tabsetPanel(
                                id = "global_total",
                                tabPanel("Cases", value = "cases", plotlyOutput("global_total_cases")),
                                tabPanel("Deaths", value = "deaths", plotlyOutput("global_total_deaths")),
                                tabPanel("Recoveries", value = "recovered", plotlyOutput("global_total_recoveries"))
                              ),
                              # Country/region (default: US) cumulative totals
                              h2("Country/region Cumulative Total"),
                              selectInput("region_selection_total",
                                          "",
                                          choices = distinct(covid19_global_full, country_region),
                                          selected = "United States"),
                              tabsetPanel(
                                id = "region_total",
                                tabPanel("Cases", value = "cases", plotlyOutput("region_total_cases")),
                                tabPanel("Deaths", value = "deaths", plotlyOutput("region_total_deaths")),
                                tabPanel("Recoveries", value = "recovered", plotlyOutput("region_total_recoveries"))
                              )
                       )
)

# Tab 4: about
# about <- 

# Define UI for each tab/page
shinyUI(
  navbarPage("COVID-19 Dashboard", theme = shinytheme("flatly"),
             global_tab,
             us_tab,
             trends_tab
  )
)
