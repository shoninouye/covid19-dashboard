# COVID-19 dashboard notes

## Ideas
* Maps
  - Confirmed cases/deaths/recovered by country
    * Also by state/province
* Plots/graphs
  - Confirmed cases/deaths/recovered over time
  - Deaths since 10th death
  - Demographic statistics
* Summary statistics
  - Confirmed cases/deaths/recovered by country
  - Number of tests by country

## Layout
* Panel 1: Maps and quick summary statistics worldwide
  - Map: Cases/deaths/recovered by country
    * Interactive w/ plotly
  - Statistics: Number of total worldwide confirmed cases/deaths/recovered
    * Static numbers
  - Statistics: List of confirmed cases/deaths/recovered by country
    * Default is cases, option to switch to deaths/recovered
    * Select country moves map to country-specific map
    * Make country map show cities/states/provinces if applicable
* Panel 2: Maps and quick summary statistics for US
  - Toggle between state and city
* Panel 2.1: Maps and quick summary statistics for countries with city/state/province data
  - Dropdown to select city/state/province based on selected country
* Panel 3: Plots/graphs by worldwide/country
  - Add option for city/state/province if applicable?
  - Confirmed cases/deaths/recovered by country
* Panel 4: Comparison?


## V1
* Panel 1: Maps and quick summary statistics worldwide
  - [ ] Map: Cases/deaths/recovered by country
    * Interactive w/ plotly
  - [x] Statistics: Number of total worldwide confirmed cases/deaths/recovered
    * Static numbers
  - [x] Statistics: List of confirmed cases/deaths/recovered by country
    * Default is cases, option to switch to deaths/recovered
* Panel 2: Plots/graphs worldwide
  - [x] Confirmed cases/deaths/recovered by country
