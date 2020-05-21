# Load packages
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)

# Read data
covid19_global_cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
covid19_global_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
covid19_global_recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

covid19_global_cases %>% View()
covid19_global_deaths %>% View()
covid19_global_recovered %>% View()
# Check to ensure cases, deaths, and recovered data has same column names
all.equal(colnames(covid19_global_cases), colnames(covid19_global_deaths), colnames(covid19_global_recovered))

# Function to convert covid19 data to long format
convert_covid_long <- function(data, col_name) {
  data %>% 
    rename(province_state = `Province/State`,
           country_region = `Country/Region`) %>% 
    pivot_longer(-c(province_state,country_region,Lat,Long),
                 names_to = "date", 
                 values_to = col_name)
}

# Convert covid19 data to long format
global_cases_long <- convert_covid_long(covid19_global_cases, "confirmed_cases")
global_deaths_long <- convert_covid_long(covid19_global_deaths, "confirmed_deaths")
global_recovered_long <- convert_covid_long(covid19_global_recovered, "confirmed_recovered")

# Join cases, deaths, and recovered data
covid19_global_full <- global_cases_long %>% 
  full_join(global_deaths_long) %>% 
  full_join(global_recovered_long) %>% 
  mutate(date = mdy(date),
         ## Make edits to regions names for geo joining 
         country_region = case_when(country_region == "Brunei" ~ "Brunei Darussalam",
                                    country_region == "Cote d'Ivoire" ~ "Côte d'Ivoire",
                                    country_region == "Czechia" ~ "Czech Republic",
                                    country_region == "Korea, South" ~ "Republic of Korea",
                                    country_region == "Laos" ~ "Lao PDR",
                                    country_region == "North Macedonia" ~ "Macedonia",
                                    country_region == "Russia" ~ "Russian Federation",
                                    country_region == "Taiwan*" ~ "Taiwan",
                                    country_region == "US" ~ "United States",
                                    country_region == "West Bank and Gaza" ~ "Palestine",
                                    # New names
                                    country_region == "Burma" ~ "Myanmar", 
                                    country_region == "Congo (Brazzaville)" ~ "Republic of Congo",
                                    country_region == "Congo (Kinshasa)" ~ "Democratic Republic of the Congo",
                                    # Greenland overrules denmark for geographic location
                                    province_state == "Greenland" ~ "Greenland",
                                    TRUE ~ country_region)
         ) 

global_cases_long %>% View()
covid19_global_full %>% View()

# covid19_global_full %>% 
#   filter(country_region == "Canada") %>% View()
# 
# covid19_global_full %>% 
#   filter(date == "2020-01-22", is.na(confirmed_cases))

### Dashboard V1
# Total cases/deaths/recovered over time
covid19_sum <- covid19_global_full %>% 
  group_by(date) %>% 
  summarize(sum_cases = sum(confirmed_cases, na.rm = TRUE),
            sum_deaths = sum(confirmed_deaths, na.rm = TRUE),
            sum_recovered = sum(confirmed_recovered, na.rm = TRUE)) 

plotly_covid19_sum <- covid19_sum %>% 
  ggplot(aes(x = date, 
             y = sum_cases,
             group = 1,
             text = sprintf("Total Cases: %s", sum_cases))) +
  geom_line(color = "dodgerblue", size = 1) + 
  scale_y_continuous(labels = scales::comma) + 
  theme_light() +
  labs(title = "Total Number of COVID-19 ____ Worldwide",
       x = "",
       y = "Number of ____")

ggplotly(plotly_covid19_sum, tooltip = "text")

plotly_region <- covid19_global_full %>% 
  filter(country_region == "United Kingdom") %>% 
  group_by(date) %>% 
  summarize(sum_cases = sum(confirmed_cases, na.rm = TRUE),
            sum_deaths = sum(confirmed_deaths, na.rm = TRUE),
            sum_recovered = sum(confirmed_recovered, na.rm = TRUE)) %>% 
  ggplot(aes(x = date, 
             y = sum_cases,
             group = 1,
             text = sprintf("Total Cases: %s<br>Date: %s", scales::comma(sum_cases, accuracy = 1), date))) +
  geom_line() + 
  scale_y_continuous(labels = scales::comma) + 
  expand_limits(y = 0) +
  theme_light() +
  labs(title = sprintf("%s cases over time", "United Kingdom"),
       x = "",
       y = "Number of cases")
ggplotly(plotly_region, tooltip = "text")



# List of confirmed cases/deaths/recovered by country/region
covid19_global_current <- covid19_global_full %>% 
  filter(date == max(date)) %>% 
  group_by(country_region) %>% 
  summarize(current_cases = sum(confirmed_cases, na.rm = TRUE),
            current_deaths = sum(confirmed_deaths, na.rm = TRUE),
            current_recovered = sum(confirmed_recovered, na.rm = TRUE)) %>% 
  right_join(distinct(covid19_global_full, country_region))
covid19_global_current %>% View()

# Look at histogram dist for map legend ranges
options(scipen=999)
covid19_global_current$current_cases %>% cut(8) %>% levels()
covid19_global_current %>% 
  ggplot(aes(x = current_recovered)) +
  geom_histogram(bins = 30) + 
  scale_x_log10(labels = scales::comma,
                breaks = c(0,10,50,100,500,1000,5000,10000,100000))

# Number of total worldwide confirmed cases/deaths/recovered
covid19_global_current %>% 
  summarize(total_cases = sum(current_cases, na.rm = TRUE),
            total_deaths = sum(current_deaths, na.rm = TRUE),
            total_recovered = sum(current_recovered, na.rm = TRUE))
covid19_global_full %>% 
  distinct(country_region)

# New cases per day ------------------------------------------------------------------------------
covid19_global_daily <- covid19_global_full %>% 
  group_by(country_region, date) %>% 
  summarize(sum_cases = sum(confirmed_cases, na.rm = TRUE),
            sum_deaths = sum(confirmed_deaths, na.rm = TRUE),
            sum_recovered = sum(confirmed_recovered, na.rm = TRUE)) %>% 
  mutate(diff_cases = sum_cases - lag(sum_cases),
         diff_deaths = sum_deaths - lag(sum_deaths),
         diff_recovered = sum_recovered - lag(sum_recovered))
covid19_daily %>% View()

covid19_daily %>% 
  filter(country_region == "France") %>% 
  ggplot(aes(x = date, y = diff_cases)) + 
  geom_col() + 
  theme_light()

# Map: Cases/deaths/recovered by country/region

library(leaflet)
library(tigris)
library(rnaturalearth)
library(rnaturalearthdata)

# GEO JOINING -------------------------------------------
ne_t <- ne_countries(type = "tiny_countries")
ne_s <- ne_countries(type = "sovereignty")
ne_u <- ne_countries(type = "map_units")
ne_c <- ne_countries()
ne_t@data$name_long %>% View()
ne_s@data$name_long
ne_u@data$name_long %>% View()
ne_c@data$name_long %>% View()

library(maptools)
class(ne_c@polygons )
rbind.SpatialPointsDataFrame(ne_c, ne_t)
ne_ct$name_long %>% View()

data.frame(name = ne_u@data$name_long) %>% 
  anti_join(data.frame(name = ne_c@data$name_long ))

### join tiny & unit and unit & country to see what regions are mission from each other
### use complete region data to join on covid19_global_current

countries <- ne_countries()
covid19_global_geo <- geo_join(countries, covid19_global_current, "name_long", "country_region")
covid19_global_geo

x <- covid19_global_current %>% 
  mutate(country_region2 = country_region) %>% 
  select(country_region, country_region2)

covid19_global_geo@data %>% select(name_long, country_region) %>% View()
covid19_global_geo@data %>% 
  select(name_long) %>% 
  full_join(x, by = c("name_long" = "country_region"), keep = TRUE) %>% 
  View()

countries@data %>%
  select(name_long) %>%
  full_join(x, by = c("name_long" = "country_region"), keep = TRUE) %>%
  # filter(is.na(country_region2)) %>%
  View()

x %>% 
  select(country_region) %>% 
  full_join(mutate(data.frame(name_long = countries@data$name_long), name_long2 = name_long), 
            by = c("country_region" = "name_long")) %>% 
  # filter(is.na(name_long2)) %>% 
  View()

countries_not_in_x <- countries@data %>%
  select(name_long) %>%
  full_join(x, by = c("name_long" = "country_region"), keep = TRUE) %>%
  filter(is.na(country_region2))
countries_not_in_x %>% View()

countries_not_in_ne <- x %>% 
  select(country_region) %>% 
  full_join(mutate(data.frame(name_long = countries@data$name_long), name_long2 = name_long), by = c("country_region" = "name_long"), keep = TRUE) %>% 
  filter(is.na(name_long2))
countries_not_in_ne %>% View()

ne_country_list <- data.frame(name_long = countries@data$name_long) %>% 
  bind_cols(data.frame(name = countries@data$name))


# Leaflet mapping ---------------------
covid19_global_geo@data$current_cases
covid19_global_geo@data %>% 
  # filter(current_cases < 10000) %>%  
  ggplot(aes(x = current_cases)) + 
  geom_histogram(bins = 100)

library(RColorBrewer)
mybins <- c(0,10,100,500,1000,5000,10000,50000,100000,Inf)
mypalette <- colorBin(palette = "YlOrBr", 
                      domain = covid19_global_geo@data$current_cases, 
                      na.color = "transparent", 
                      bins = mybins)

# Prepare the text for tooltips:
mytext <- paste(
  "Country/region: ", covid19_global_geo@data$name_long,"<br/>", 
  "Current cases: ", covid19_global_geo@data$current_cases, "<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

# Final Map
leaflet(covid19_global_geo) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(current_cases), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend(pal = mypalette, 
            values = ~current_cases, 
            opacity = 0.9, 
            title = "Current cases", 
            position = "bottomleft")

# plotly
library(geojsonio)
spdf <- geojson_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes.geojson",  what = "sp")
spdf$code

spdf_fortified <- tidy(spdf, region = "code")
data <- read.table("https://www.r-graph-gallery.com/wp-content/uploads/2017/12/data_on_french_states.csv", header=T, sep=";")
head(data)


### Testing other choropleth mapping options --------

library(broom)
library(mapproj)
covid19_global_geo@data %>% View()
covid_geo_fortified <- tidy(covid19_global_geo)
ggplot() +
  geom_polygon(data = covid19_global_geo, aes(x = long, y = lat, group = group), color="grey") +
  theme_void() +
  coord_map()



library(maps)
world_map <- map("world", fill = TRUE, plot = FALSE)
world_map




map('world',col="grey", fill=TRUE, bg="white", lwd=0.05, mar=rep(0,4),border=0, ylim=c(-80,80) )

map('world', fill = TRUE, col = 1:10)
mapStates = map("state", fill = TRUE, plot = FALSE)
str(mapStates)
leaflet(data = mapStates) %>% 
  addProviderTiles(provider = "CartoDB") %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)




####### US
# Read data
covid19_us_cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
covid19_us_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
covid19_us_cases %>% View()
covid19_us_deaths %>% View()

all.equal(colnames(covid19_us_cases), colnames(covid19_us_deaths))

# Convert us data to long format
us_cases_long <- covid19_us_cases %>% 
  rename(province_state = Province_State,
         country_region = Country_Region,
         Long = Long_) %>% 
  select(-c(UID, iso2, iso3, code3, FIPS, Admin2, Combined_Key)) %>% 
  pivot_longer(-c(province_state,country_region,Lat,Long),
               names_to = "date", 
               values_to = "confirmed_cases")

us_deaths_long <- covid19_us_deaths %>% 
  rename(province_state = Province_State,
         country_region = Country_Region,
         Long = Long_) %>% 
  select(-c(UID, iso2, iso3, code3, FIPS, Admin2, Combined_Key)) %>% 
  pivot_longer(-c(province_state,country_region,Lat,Long,Population),
               names_to = "date", 
               values_to = "confirmed_deaths")

# Join cases, deaths, and recovered us data
covid19_us_full <- us_cases_long %>% 
  full_join(us_deaths_long) %>% 
  mutate(date = mdy(date))

# List of confirmed cases/deaths/recovered by state
covid19_us_current <- covid19_us_full %>% 
  filter(date == max(date),
         !province_state %in% c("Diamond Princess", "Grand Princess")) %>% 
  group_by(province_state) %>% 
  summarize(current_cases = sum(confirmed_cases, na.rm = TRUE),
            current_deaths = sum(confirmed_deaths, na.rm = TRUE))
covid19_us_full %>% distinct(province_state) %>% View()

# Total cases/deaths/recovered over time
covid19_sum <- covid19_us_full %>% 
  group_by(date) %>% 
  summarize(sum_cases = sum(confirmed_cases, na.rm = TRUE),
            sum_deaths = sum(confirmed_deaths, na.rm = TRUE))

# EDA HISTOGRAM
covid19_us_current %>% 
  ggplot(aes(x = current_deaths)) +
  geom_histogram(bins = 30) + 
  scale_x_log10(labels = scales::comma,
                breaks = c(0,10,50,100,500,1000,2500,5000))
# US MAP


# Get geo US data
# devtools::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)
us_states <- ne_states(country = "United States of America")
covid19_us_geo <- geo_join(us_states, covid19_us_current, "name", "province_state")

us_cases_bins <- c(0,1000,5000,10000,20000,40000,Inf)
us_cases_palette <- colorBin(palette = "viridis", 
                             reverse = TRUE,
                             domain = covid19_us_geo@data$current_cases, 
                             na.color = "transparent", 
                             bins = us_cases_bins)

# Prepare the text for tooltips:
us_cases_text <- paste(
  "State: ", covid19_us_geo@data$name,"<br/>", 
  "Current cases: ", scales::comma(covid19_us_geo@data$current_cases), "<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

# US Map
leaflet(covid19_us_geo) %>% 
  addTiles()  %>% 
  setView(lat=50, lng=-125 , zoom=3) %>%
  # setMaxBounds(lng1 = -180, 
  #              lat1 = 85.05115,
  #              lng2 = 180,
  #              lat2 = -85.05115) %>% 
  addPolygons( 
    fillColor = ~us_cases_palette(current_cases), 
    stroke=TRUE, 
    fillOpacity = 0.7, 
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



