# Visualizing COVID-19 Mortality by Poverty Level in the US Across Counties 

# Analyze the data from https://github.com/nytimes/covid-19-data 
# and link it to US Census ACS (American Community Survey) data on 
# population and poverty data from 2020 
# 
# Render a visualization, accompanying table, and save them. 


# dependencies ------------------------------------------------------------

library(tidyverse)
library(tidycensus)
library(Hmisc)

# load covid19 data -------------------------------------------------------

covid19_data <- list(
  readr::read_csv("data/us-counties-2020.csv"),
  readr::read_csv("data/us-counties-2021.csv"),
  readr::read_csv("data/us-counties-2022.csv"),
  readr::read_csv("data/us-counties-2023.csv")
)

# combine into 1 data frame
covid19_data <- covid19_data |> dplyr::bind_rows()

# aggregating up to monthly level  ----------------------------------------

# make a year-month column
covid19_data <- covid19_data |> 
  mutate(
    date = lubridate::ymd(date), 
    year_month = paste0(
      lubridate::year(date), "-", lubridate::month(date)))

# aggregate by county and year-month
covid19_data <- covid19_data |> 
  group_by(geoid, year_month) |> 
  dplyr::summarize(
    deaths = sum(deaths, na.rm=TRUE))

# fetch population and poverty data from ACS ------- 

# fetch data from ACS
population_and_poverty <- tidycensus::get_acs(
  geography = 'county',
  year = 2020,
  variables = c(
    population = "B01001_001",
    poverty_numerator = "B05010_002",
    poverty_denominator = "B05010_001"
  ))

# pivot wider so we can calculate poverty rates
population_and_poverty <- population_and_poverty |> 
  tidyr::pivot_wider(
    id_cols = c(GEOID, NAME),
    values_from = 'estimate',
    names_from = 'variable')

# calculate poverty rates (proportion of population in poverty)
population_and_poverty <- population_and_poverty |> 
  mutate(
    proportion_in_poverty = poverty_numerator / poverty_denominator)

# merge covid19 and ACS data  ---------------------------------------------

# remove unnecessary "USA-" prefix
covid19_data$geoid <- stringr::str_remove(covid19_data$geoid, "USA-")

# join data together
covid19_data <- left_join(covid19_data, population_and_poverty,
                          by = c('geoid' = 'GEOID'))

# cutpoints for poverty ---------------------------------------------------

# categorize poverty (0-5%, 5-10%, 10-20%, 20-100%)
covid19_data <- covid19_data |> 
  dplyr::mutate(
    poverty_cat = cut(proportion_in_poverty, c(0, 0.05, 0.10, 0.20, 1)))


# summarize by poverty level ----------------------------------------------

# calculate mortality rates
covid19_data <- covid19_data |> 
  mutate(
    deaths_per_100k_person_months = deaths / population * 1e5)

# calculate (weighted) average mortality rates within 
# each poverty level
covid19_data_by_poverty_level <- covid19_data |> 
  group_by(year_month, poverty_cat) |> 
  dplyr::summarize(
    deaths_per_100k_person_months = Hmisc::wtd.mean(
      deaths_per_100k_person_months, normwt = population))

# visualize trends in COVID-19 by poverty ---------------------------------

# make the year-month an ordered factor variable
covid19_data_by_poverty_level$year_month <- 
  factor(
    covid19_data_by_poverty_level$year_month,
    levels = 
      paste0(
        rep(2020:2023, each = 12),
        "-",
        rep(1:12, 4)))

# visualize as a line + point plot for each of the poverty-levels
ggplot(covid19_data_by_poverty_level, 
       aes(
         x = year_month, 
         y = deaths_per_100k_person_months,
         group = poverty_cat,
         color = poverty_cat,
         shape = poverty_cat,
         linetype = poverty_cat
         )) + 
  geom_line() + 
  geom_point() + 
  ylim(c(0, 45)) + 
  ggtitle("US COVID-19 Mortality Rates by Poverty Level") + 
  ylab("COVID-19 Mortality per 100k Person-Months") + 
  xlab("Year and Month") + 
  scale_color_brewer( # use a red-blue color palette
    palette = 'RdBu', 
    direction = -1) + # make sure that highest poverty = red
  theme_bw() +  # remove gray background
  # rotate x-axis text
  theme(
    axis.text.x = element_text(angle = 80, hjust = 1)) 

# export figure -----------------------------------------------------------

ggsave(
  "covid19_mortality_by_poverty_levels.png",
  width = 6,
  height = 4,
  scale = 1.3)


# export table ------------------------------------------------------------

covid19_data_by_poverty_level |> 
  dplyr::arrange(year_month, poverty_cat) |> 
  write.csv("covid19_mortality_by_poverty_levels.csv",
            row.names = FALSE)
