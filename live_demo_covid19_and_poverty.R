# analyzing monthly county COVID-19 mortality rates and poverty levels

# steps: 
#   - dependencies
#   - load the covid data 
#   - cleaning the covid data 
#   - fetch the poverty data 
#   - clean 
#   - merge datasets 
#   - data visualization


# dependencies ------------------------------------------------------------

library(tidyverse)
library(tidycensus)
library(here)


# load covid data ---------------------------------------------------------

covid <- list(
  readr::read_csv("us-counties-2020.csv"),
  readr::read_csv("us-counties-2021.csv"),
  readr::read_csv("us-counties-2022.csv")
)

covid <- bind_rows(covid)


# cleaning ----------------------------------------------------------------

covid$geoid <- stringr::str_replace_all(covid$geoid, "USA-", "")

# aggregate up to monthly observations 

covid <- covid |> 
  mutate(
    year_month = paste0(lubridate::year(date), "-", lubridate::month(date))
  )

covid$year_month <- factor(covid$year_month, levels = 
                       paste0(rep(2020:2022, each = 12), "-", rep(1:12, 3)))

covid <- covid |> 
  group_by(year_month, geoid) |> 
  summarize(
    deaths_avg_per_100k = mean(deaths_avg_per_100k, na.rm = TRUE)
  )

# fetch county covariates data ------------------------------------------------------

popsize_and_poverty <- tidycensus::get_acs(
  geography = 'county',
  year = 2020,
  variables = c(
    popsize = 'B01001_001',
    total_for_poverty_table = 'B05010_001',
    in_poverty = 'B05010_002'
  )
)

popsize_and_poverty <- popsize_and_poverty |> 
  select(-moe) |> 
  tidyr::pivot_wider(
    id_cols = c(GEOID, NAME),
    values_from = estimate,
    names_from = variable
  )

popsize_and_poverty <- popsize_and_poverty |> 
  mutate(
    proportion_in_poverty = in_poverty / total_for_poverty_table
  )

popsize_and_poverty <- popsize_and_poverty |> 
  select(GEOID, popsize, proportion_in_poverty)


# merge data --------------------------------------------------------------

covid <- left_join(covid, popsize_and_poverty, by = c('geoid' = 'GEOID'))


# make poverty levels -----------------------------------------------------

covid$poverty_cut <- cut(covid$proportion_in_poverty, c(0, 0.05, 0.1, 0.2, 1))


# weighted average by poverty level ---------------------------------------

covid_by_poverty_level <- covid |> 
  group_by(poverty_cut, year_month) |> 
  summarize(
    deaths_avg_per_100k = Hmisc::wtd.mean(deaths_avg_per_100k, normwt = popsize)
  )


# visualize ---------------------------------------------------------------

ggplot(
  covid_by_poverty_level |> filter(! is.na(poverty_cut)),
  aes(x = year_month, 
      y = deaths_avg_per_100k, 
      color = poverty_cut,
      group = poverty_cut)) + 
  geom_line() + 
  scale_color_brewer(palette = 'RdBu', direction = -1) + 
  xlab("Date") + 
  ylab("COVID-19 Mortality per 100k (monthly observations)") + 
  ggtitle("Monthly County COVID-19 Mortality Estimates by Poverty Level in the US") + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1))


# save results ------------------------------------------------------------

ggsave("covid19_rates_and_poverty_levels.png", width=9, height=5)

covid_by_poverty_level <- covid_by_poverty_level %>% 
  arrange(year_month)

write.csv(covid_by_poverty_level, file = "covid19_rates_and_poverty_levels.csv",
            row.names = FALSE)
