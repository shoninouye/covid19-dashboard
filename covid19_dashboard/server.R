#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output) {
  ### Tab 1: Global --------------------------------------------------------
  # * Global static stats --------------------------------------------------------
  output$global_cases_box <- renderValueBox({
    valueBox(value = tags$p(scales::comma(covid19_global_total$total_cases), 
                            style = "font-size: 150%;"), 
             subtitle = "Current cumulative cases")
  })
  output$global_deaths_box <- renderValueBox({
    valueBox(value = tags$p(scales::comma(covid19_global_total$total_deaths), 
                            style = "font-size: 150%;"), 
             subtitle = "Current cumulative deaths")
  })
  output$global_recovered_box <- renderValueBox({
    valueBox(value = tags$p(scales::comma(covid19_global_total$total_recovered), 
                            style = "font-size: 150%;"), 
             subtitle = "Current cumulative recoveries")
  })
  
  # * Global country/region tables -----------------
  output$global_cases_df <- renderDT({
    covid19_global_current %>% 
      select(country_region, Cases = current_cases) %>% 
      arrange(desc(Cases)) %>% 
      data.frame() %>% 
      rename('Country/region' = country_region)
  })
  output$global_deaths_df <- renderDT({
    covid19_global_current %>% 
      select(country_region, Deaths = current_deaths) %>% 
      arrange(desc(Deaths)) %>% 
      data.frame() %>% 
      rename('Country/region' = country_region)
  })
  output$global_recovered_df <- renderDT({
    covid19_global_current %>% 
      select(country_region, Recoveries = current_recovered) %>% 
      arrange(desc(Recoveries)) %>% 
      data.frame() %>% 
      rename('Country/region' = country_region)
  })
  
  # * Global Leaflet maps ----------------------------------------------------------
  # Global map - cases
  output$global_map_cases <- renderLeaflet({
    leaflet(covid19_global_geo) %>% 
      addTiles(options = tileOptions(minZoom = 1))  %>% 
      setView(lat = 25, lng = -10, zoom = 3) %>%
      setMaxBounds(lng1 = -180, 
                   lat1 = 85.05115,
                   lng2 = 180,
                   lat2 = -85.05115) %>% 
      addPolygons( 
        fillColor = ~cases_palette(current_cases), 
        stroke=TRUE, 
        fillOpacity = 0.85, 
        color="white", 
        weight=0.3,
        label = cases_text,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLegend(pal = cases_palette, 
                values = ~current_cases, 
                opacity = 0.9, 
                title = "Current cases", 
                position = "bottomleft")
  })
  
  # Global map - deaths
  output$global_map_deaths <- renderLeaflet({
    leaflet(covid19_global_geo) %>% 
      addTiles(options = tileOptions(minZoom = 1))  %>% 
      setView(lat = 25, lng = -10, zoom = 3) %>%
      setMaxBounds(lng1 = -180, 
                   lat1 = 85.05115,
                   lng2 = 180,
                   lat2 = -85.05115) %>% 
      addPolygons( 
        fillColor = ~deaths_palette(current_deaths), 
        stroke=TRUE, 
        fillOpacity = 0.85, 
        color="white", 
        weight=0.3,
        label = deaths_text,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLegend(pal = deaths_palette, 
                values = ~current_deaths, 
                opacity = 0.9, 
                title = "Current deaths", 
                position = "bottomleft")
  })
  
  # Global map - recovered
  output$global_map_recovered <- renderLeaflet({
    leaflet(covid19_global_geo) %>% 
      addTiles(options = tileOptions(minZoom = 1))  %>% 
      setView(lat = 25, lng = -10, zoom = 3) %>%
      setMaxBounds(lng1 = -180, 
                   lat1 = 85.05115,
                   lng2 = 180,
                   lat2 = -85.05115) %>% 
      addPolygons( 
        fillColor = ~recovered_palette(current_recovered), 
        stroke=TRUE, 
        fillOpacity = 0.85, 
        color="white", 
        weight=0.3,
        label = recovered_text,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLegend(pal = recovered_palette, 
                values = ~current_recovered, 
                opacity = 0.9, 
                title = "Current recoveries", 
                position = "bottomleft")
  })
  
  # Tab 2 - United States --------------------------------------------------------
  # * US static stats --------------------------------------------------------
  output$us_cases_box <- renderValueBox({
    valueBox(value = tags$p(scales::comma(covid19_us_total$total_cases), 
                            style = "font-size: 150%;"), 
             subtitle = "Current cumulative cases")
  })
  output$us_deaths_box <- renderValueBox({
    valueBox(value = tags$p(scales::comma(covid19_us_total$total_deaths), 
                            style = "font-size: 150%;"), 
             subtitle = "Current cumulative deaths")
  })
  
  # * US state tables -----------------------------------------------------------
  output$us_cases_df <- renderDT({
    covid19_us_current %>% 
      select(province_state, Cases = current_cases) %>% 
      arrange(desc(Cases)) %>% 
      data.frame() %>% 
      rename("State/region" = province_state)
  })
  output$us_deaths_df <- renderDT({
    covid19_us_current %>% 
      select(province_state, Deaths = current_deaths) %>% 
      arrange(desc(Deaths)) %>% 
      data.frame() %>% 
      rename("State/region" = province_state)
  })
  
  # * US Leaflet maps -------------------------------------------------------
  # US map - cases
  output$us_map_cases <- renderLeaflet({
    leaflet(covid19_us_geo) %>% 
      addTiles(options = tileOptions(minZoom = 1))  %>% 
      setView(lat=44, lng=-114 , zoom=4) %>%
      addPolygons( 
        fillColor = ~us_cases_palette(current_cases), 
        stroke=TRUE, 
        fillOpacity = 0.8, 
        color="white", 
        weight=0.3,
        label = us_cases_text,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLegend(pal = us_cases_palette, 
                values = ~current_cases, 
                opacity = 0.9, 
                title = "Current cases", 
                position = "bottomleft")
  })
  
  # US map - deaths
  output$us_map_deaths <- renderLeaflet({
    leaflet(covid19_us_geo) %>% 
      addTiles(options = tileOptions(minZoom = 1))  %>% 
      setView(lat=44, lng=-114 , zoom=4) %>%
      addPolygons( 
        fillColor = ~us_deaths_palette(current_deaths), 
        stroke=TRUE, 
        fillOpacity = 0.8, 
        color="white", 
        weight=0.3,
        label = us_deaths_text,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLegend(pal = us_deaths_palette, 
                values = ~current_deaths, 
                opacity = 0.9, 
                title = "Current deaths", 
                position = "bottomleft")
  })
  
  # Tab 3: Trends ------------------------------------------------------------

  # * Plotly global daily cases/deaths/recoveries over time --------------------
  global_daily_dict <- c("cases" = "sum_diff_cases",
                         "deaths" = "sum_diff_deaths",
                         "recovered" = "sum_diff_recovered")
  rolling_dict <- c("cases" = "rolling_7d_avg_cases",
                    "deaths" = "rolling_7d_avg_deaths",
                    "recovered" = "rolling_7d_avg_recovered")
  title_dict <- c("cases" = "Cases",
                  "deaths" = "Deaths",
                  "recovered" = "Recoveries")

  plot_global_new_daily <- function(metric){
    var <- global_daily_dict[metric]
    var_rolling <- rolling_dict[metric]
    
    covid19_global_daily %>%
      ggplot(aes(x = date,
                 y = !!ensym(var),
                 group = 1,
                 text = sprintf("New Daily %s: %s<br>Date: %s",
                                title_dict[metric],
                                scales::comma(!!ensym(var), accuracy = 1),
                                date))) +
      geom_col(aes(fill = !!ensym(var) < 0),
               width = 0.75) +
      geom_area(aes(x = date, y = !!ensym(var_rolling)),
                fill = "#F8766D",
                alpha = 0.25) +
      geom_line(aes(x = date, y = !!ensym(var_rolling))) +
      labs(title = sprintf("New global daily %s over time",
                           tolower(title_dict[metric])),
           x = "",
           y = sprintf("Number of %s",
                       tolower(title_dict[metric]))) +
      theme_light() +
      theme(legend.position = "none")
  }
  
  plotly_global_new_daily <- reactive({
    plot_global_new_daily(input$global_daily)
  })
  
  output$global_daily_cases <- renderPlotly({
    ggplotly(plotly_global_new_daily(), tooltip = "text") %>%
      style(hoverlabel = list(bgcolor="white")) %>% 
      style(hoverinfo = "none", 
            traces = 2:3)
  })
  output$global_daily_deaths <- renderPlotly({
    ggplotly(plotly_global_new_daily(), tooltip = "text") %>%
      style(hoverlabel = list(bgcolor="white")) %>% 
      style(hoverinfo = "none", 
            traces = 2:3)
  })
  output$global_daily_recoveries <- renderPlotly({
    ggplotly(plotly_global_new_daily(), tooltip = "text") %>%
      style(hoverlabel = list(bgcolor="white")) %>% 
      style(hoverinfo = "none", 
            traces = 2:3)
  })

  # * Plotly region daily cases/deaths/recoveries over time --------------------
  region_daily_dict <- c("cases" = "diff_cases",
                         "deaths" = "diff_deaths",
                         "recovered" = "diff_recovered")

  plot_region_new_daily <- function(region, metric){
    var <- region_daily_dict[metric]
    var_rolling <- rolling_dict[metric]
    
    covid19_full_daily %>%
      filter(country_region == region) %>%
      mutate(rolling_7d_avg_cases = rollmean(diff_cases, 7, align = "right", fill = NA),
             rolling_7d_avg_deaths = rollmean(diff_deaths, 7, align = "right", fill = NA),
             rolling_7d_avg_recovered = rollmean(diff_recovered, 7, align = "right", fill = NA)) %>%
      ggplot(aes(x = date,
                 y = !!ensym(var),
                 group = 1,
                 text = sprintf("New Daily %s: %s<br>Date: %s",
                                title_dict[metric],
                                scales::comma(!!ensym(var), accuracy = 1), date))) +
      geom_col(aes(fill = !!ensym(var) < 0),
               width = 0.75) +
      geom_area(aes(x = date, y = !!ensym(var_rolling)),
                fill = "#F8766D",
                alpha = 0.25) +
      geom_line(aes(x = date, y = !!ensym(var_rolling))) +
      labs(title = sprintf("%s new daily %s over time",
                           region,
                           tolower(title_dict[metric])),
           x = "",
           y = sprintf("Number of %s",
                       tolower(title_dict[metric]))) +
      theme_light() +
      theme(legend.position = "none")
  }
  
  plotly_region_new_daily <- reactive({
    plot_region_new_daily(input$region_selection_daily, input$region_daily)
  })
  
  output$region_daily_cases <- renderPlotly({
    ggplotly(plotly_region_new_daily(), tooltip = "text") %>%
      style(hoverlabel = list(bgcolor="white")) %>% 
      style(hoverinfo = "none", 
            traces = 3)
  })
  output$region_daily_deaths <- renderPlotly({
    ggplotly(plotly_region_new_daily(), tooltip = "text") %>%
      style(hoverlabel = list(bgcolor="white")) %>% 
      style(hoverinfo = "none", 
            traces = 3)
  })
  output$region_daily_recoveries <- renderPlotly({
    ggplotly(plotly_region_new_daily(), tooltip = "text") %>%
      style(hoverlabel = list(bgcolor="white")) %>% 
      style(hoverinfo = "none", 
            traces = 3)
  })

  # * Plotly global total cases/deaths/recoveries over time ------------------------------------------
  total_dict <- c("cases" = "sum_cases",
                  "deaths" = "sum_deaths",
                  "recovered" = "sum_recovered")
  
  plot_global_total <- function(metric){
    var <- total_dict[metric]
    
    covid19_sum %>%
      ggplot(aes(x = date,
                 y = !!ensym(var),
                 group = 1,
                 text = sprintf("Total Cumulative %s: %s<br>Date: %s",
                                title_dict[metric],
                                scales::comma(!!ensym(var), accuracy = 1),
                                date))) +
      geom_line(color = "#F8766D", size = 1) +
      scale_y_continuous(labels = scales::comma) +
      theme_light() +
      labs(title = sprintf("Global cumulative total %s over time",
                           tolower(title_dict[metric])),
           x = "",
           y = sprintf("Number of %s",
                       tolower(title_dict[metric])))
  }
  
  plotly_global_total <- reactive({
    plot_global_total(input$global_total)
  })
  
  output$global_total_cases <- renderPlotly({
    ggplotly(plotly_global_total(), tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor="white"))
  })
  output$global_total_deaths <- renderPlotly({
    ggplotly(plotly_global_total(), tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor="white"))
  })
  output$global_total_recoveries <- renderPlotly({
    ggplotly(plotly_global_total(), tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor="white"))
  })


  # * Plotly region total data (reactive) ---------------------------------------
  region_total_filtered <- reactive({
    covid19_global_full %>%
      filter(country_region == input$region_selection_total) %>%
      group_by(date) %>%
      summarize(sum_cases = sum(confirmed_cases, na.rm = TRUE),
                sum_deaths = sum(confirmed_deaths, na.rm = TRUE),
                sum_recovered = sum(confirmed_recovered, na.rm = TRUE))
  })

  # * Plotly region total cases/deaths/recoveries over time (reactive) ---------------------------------------
  plot_region_total <- function(region, metric){
    var <- total_dict[metric]
    
    region_total_filtered() %>%
      ggplot(aes(x = date,
                 y = !!ensym(var),
                 group = 1,
                 text = sprintf("Total Cumulative %s: %s<br>Date: %s",
                                title_dict[metric],
                                scales::comma(!!ensym(var), accuracy = 1),
                                date))) +
      geom_line(color = "#F8766D", size = 1) +
      scale_y_continuous(labels = scales::comma) +
      theme_light() +
      labs(title = sprintf("%s total cumulative %s over time",
                           region,
                           tolower(title_dict[metric])),
           x = "",
           y = sprintf("Number of %s",
                       tolower(title_dict[metric])))
  }
  
  plotly_region_total <- reactive({
    plot_region_total(input$region_selection_total, input$region_total)
  })
  
  output$region_total_cases <- renderPlotly({
    ggplotly(plotly_region_total(), tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor="white"))
  })
  output$region_total_deaths <- renderPlotly({
    ggplotly(plotly_region_total(), tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor="white"))
  })
  output$region_total_recoveries <- renderPlotly({
    ggplotly(plotly_region_total(), tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor="white"))
  })
  
})