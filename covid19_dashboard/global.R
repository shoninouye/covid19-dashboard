# Load packages
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)
library(RColorBrewer)
library(DT)
library(rnaturalearth)
library(rnaturalearthdata)
# devtools::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)
library(leaflet)
library(tigris)



# Read data
covid19_global_cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
covid19_global_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
covid19_global_recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
covid19_us_cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
covid19_us_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

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
  mutate(date = mdy(date))

### Dashboard V1
# Total cases/deaths/recovered over time
covid19_sum <- covid19_global_full %>% 
  group_by(date) %>% 
  summarize(sum_cases = sum(confirmed_cases, na.rm = TRUE),
            sum_deaths = sum(confirmed_deaths, na.rm = TRUE),
            sum_recovered = sum(confirmed_recovered, na.rm = TRUE)) 



# List of confirmed cases/deaths/recovered by country/region
covid19_global_current <- covid19_global_full %>% 
  filter(date == max(date)) %>% 
  # Greenland should be a region, even tho it is part of denmark 
  group_by(country_region) %>% 
  summarize(current_cases = sum(confirmed_cases, na.rm = TRUE),
            current_deaths = sum(confirmed_deaths, na.rm = TRUE),
            current_recovered = sum(confirmed_recovered, na.rm = TRUE)) %>% 
  right_join(distinct(covid19_global_full, country_region))

# Number of total worldwide confirmed cases/deaths/recovered
covid19_global_total <- covid19_global_current %>% 
  summarize(total_cases = sum(current_cases, na.rm = TRUE),
            total_deaths = sum(current_deaths, na.rm = TRUE),
            total_recovered = sum(current_recovered, na.rm = TRUE))

### Map - global
# Global Map
covid19_global_current <- covid19_global_current %>%
  mutate(country_region = case_when(country_region == "Brunei" ~ "Brunei Darussalam",
                                    country_region == "Cote d'Ivoire" ~ "Côte d'Ivoire",
                                    country_region == "Czechia" ~ "Czech Republic",
                                    country_region == "Korea, South" ~ "Republic of Korea",
                                    country_region == "Laos" ~ "Lao PDR",
                                    country_region == "North Macedonia" ~ "Macedonia",
                                    country_region == "Russia" ~ "Russian Federation",
                                    country_region == "Taiwan*" ~ "Taiwan",
                                    country_region == "US" ~ "United States",
                                    country_region == "West Bank and Gaza" ~ "Palestine",
                                    TRUE ~ as.character(country_region)))
countries <- ne_countries()
covid19_global_geo <- geo_join(countries, covid19_global_current, "name_long", "country_region")

# PLotly global map - cases
cases_bins <- c(0,100,500,1000,5000,10000,50000,200000,Inf)
cases_palette <- colorBin(palette = "viridis", 
                      reverse = TRUE,
                      domain = covid19_global_geo@data$current_cases, 
                      na.color = "transparent", 
                      bins = cases_bins)
cases_text <- paste(
  "Country/region: ", covid19_global_geo@data$name_long,"<br/>", 
  "Current cases: ", scales::comma(covid19_global_geo@data$current_cases), "<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

# PLotly global map - deaths
deaths_bins <- c(0,10,50,100,500,1000,5000,10000,100000,Inf)
deaths_palette <- colorBin(palette = "viridis", 
                          reverse = TRUE,
                          domain = covid19_global_geo@data$current_deaths, 
                          na.color = "transparent", 
                          bins = deaths_bins)
deaths_text <- paste(
  "Country/region: ", covid19_global_geo@data$name_long,"<br/>", 
  "Current deaths: ", scales::comma(covid19_global_geo@data$current_deaths), "<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

# PLotly global map - recovered
recovered_bins <- c(0,10,25,100,250,1000,5000,10000,100000,Inf)
recovered_palette <- colorBin(palette = "viridis", 
                          reverse = TRUE,
                          domain = covid19_global_geo@data$current_recovered, 
                          na.color = "transparent", 
                          bins = recovered_bins)
recovered_text <- paste(
  "Country/region: ", covid19_global_geo@data$name_long,"<br/>", 
  "Current recoveries: ", scales::comma(covid19_global_geo@data$current_recovered), "<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

### United States
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

# Number of total US confirmed cases/deaths/recovered
covid19_us_total <- covid19_us_current %>% 
  summarize(total_cases = sum(current_cases, na.rm = TRUE),
            total_deaths = sum(current_deaths, na.rm = TRUE))

### US Map
# Get geo US data
us_states <- ne_states(country = "United States of America")
covid19_us_geo <- geo_join(us_states, covid19_us_current, "name", "province_state")

# Tooltip for us cases
us_cases_bins <- c(0,1000,5000,10000,50000,100000,Inf)
us_cases_palette <- colorBin(palette = "viridis", 
                             reverse = TRUE,
                             domain = covid19_us_geo@data$current_cases, 
                             na.color = "transparent", 
                             bins = us_cases_bins)
us_cases_text <- paste(
  "State: ", covid19_us_geo@data$name,"<br/>", 
  "Current cases: ", scales::comma(covid19_us_geo@data$current_cases), "<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

# Tooltip for us deaths
us_deaths_bins <- c(0,50,100,500,1000,2500,5000,Inf)
us_deaths_palette <- colorBin(palette = "viridis", 
                             reverse = TRUE,
                             domain = covid19_us_geo@data$current_deaths, 
                             na.color = "transparent", 
                             bins = us_deaths_bins)
us_deaths_text <- paste(
  "State: ", covid19_us_geo@data$name,"<br/>", 
  "Current deaths: ", scales::comma(covid19_us_geo@data$current_deaths), "<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)
