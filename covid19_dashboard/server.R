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
  # * Plotly global cases over time ------------------------------------------
  output$global_cases_over_time <- renderPlotly({
      plotly_covid19_sum <- covid19_sum %>% 
      ggplot(aes(x = date, 
                 y = sum_cases,
                 group = 1,
                 text = sprintf("Total Cases: %s<br>Date: %s", scales::comma(sum_cases, accuracy = 1), date))) +
      geom_line(color = "dodgerblue", size = 1) + 
      scale_y_continuous(labels = scales::comma) + 
      theme_light() +
      labs(title = "Global cases over time",
           x = "",
           y = "Number of cases")
    ggplotly(plotly_covid19_sum, tooltip = "text") %>% 
      layout(hoverlabel=list(bgcolor="white"))
  })
  
  # * Plotly global deaths over time ------------------------------------------
  output$global_deaths_over_time <- renderPlotly({
    plotly_covid19_sum <- covid19_sum %>% 
      ggplot(aes(x = date, 
                 y = sum_deaths,
                 group = 1,
                 text = sprintf("Total Deaths: %s<br>Date: %s", scales::comma(sum_deaths, accuracy = 1), date))) +
      geom_line(color = "dodgerblue", size = 1) + 
      scale_y_continuous(labels = scales::comma) + 
      theme_light() +
      labs(title = "Global deaths over time",
           x = "",
           y = "Number of deaths")
    ggplotly(plotly_covid19_sum, tooltip = "text") %>% 
      layout(hoverlabel=list(bgcolor="white"))
  })
  
  # * Plotly global recoveries over time -------------------------------------
  output$global_recovered_over_time <- renderPlotly({
    plotly_covid19_sum <- covid19_sum %>% 
      ggplot(aes(x = date, 
                 y = sum_recovered,
                 group = 1,
                 text = sprintf("Total Recoveries: %s<br>Date: %s", scales::comma(sum_recovered, accuracy = 1), date))) +
      geom_line(color = "dodgerblue", size = 1) + 
      scale_y_continuous(labels = scales::comma) + 
      theme_light() +
      labs(title = "Global recoveries over time",
           x = "",
           y = "Number of recoveries")
    ggplotly(plotly_covid19_sum, tooltip = "text") %>% 
      layout(hoverlabel=list(bgcolor="white"))
  })
  
  # * Plotly region data (reactive) ---------------------------------------
  region_filtered <- reactive({
    covid19_global_full %>% 
      filter(country_region == input$region_selection) %>% 
      group_by(date) %>% 
      summarize(sum_cases = sum(confirmed_cases, na.rm = TRUE),
                sum_deaths = sum(confirmed_deaths, na.rm = TRUE),
                sum_recovered = sum(confirmed_recovered, na.rm = TRUE))
  })
  
  # * Plotly region cases over time (reactive) ---------------------------------------
  output$region_cases_over_time <- renderPlotly({
    plotly_region <- region_filtered() %>%
      ggplot(aes(x = date, 
                 y = sum_cases,
                 group = 1,
                 text = sprintf("Total Cases: %s<br>Date: %s", scales::comma(sum_cases, accuracy = 1), date))) +
      geom_line(color = "dodgerblue", size = 1) + 
      scale_y_continuous(labels = scales::comma) + 
      theme_light() +
      labs(title = sprintf("%s cases over time", input$region_selection),
           x = "",
           y = "Number of cases")
    ggplotly(plotly_region, tooltip = "text") %>% 
      layout(hoverlabel=list(bgcolor="white"))
  })
  
  # * Plotly region deaths over time (reactive) ---------------------------------------
  output$region_deaths_over_time <- renderPlotly({
    plotly_region <- region_filtered() %>%
      ggplot(aes(x = date, 
                 y = sum_deaths,
                 group = 1,
                 text = sprintf("Total Deaths: %s<br>Date: %s", scales::comma(sum_deaths, accuracy = 1), date))) +
      geom_line(color = "dodgerblue", size = 1) + 
      scale_y_continuous(labels = scales::comma) + 
      theme_light() +
      labs(title = sprintf("%s deaths over time", input$region_selection),
           x = "",
           y = "Number of deaths")
    ggplotly(plotly_region, tooltip = "text") %>% 
      layout(hoverlabel=list(bgcolor="white"))
  })
  
  # * Plotly region recovered over time (reactive) ---------------------------------------
  output$region_recovered_over_time <- renderPlotly({
    plotly_region <- region_filtered() %>%
      ggplot(aes(x = date, 
                 y = sum_recovered,
                 group = 1,
                 text = sprintf("Total Recoveries: %s<br>Date: %s", scales::comma(sum_recovered, accuracy = 1), date))) +
      geom_line(color = "dodgerblue", size = 1) + 
      scale_y_continuous(labels = scales::comma) + 
      theme_light() +
      labs(title = sprintf("%s recovered over time", input$region_selection),
           x = "",
           y = "Number of recovered")
    ggplotly(plotly_region, tooltip = "text") %>% 
      layout(hoverlabel=list(bgcolor="white"))
  })
})
