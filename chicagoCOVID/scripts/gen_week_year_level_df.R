library(tidyverse)
library(janitor)
library(lubridate)

gen_year_week_level_df <- function(covid_day_level_df) {
  # Initial cleaning ----
  covid_day_level_df <- covid_day_level_df %>% 
    janitor::clean_names() %>% 
    filter(!is.na(date)) %>%  # one date is missing
    mutate(date = mdy(date),
           year = year(date),
           month = month(date),
           week = epiweek(date)) %>% 
    arrange(year, month) 
  
  # Drop all rows with hospitalizations > cases or deaths > cases 
  covid_day_level_df <- covid_day_level_df %>% 
    filter(cases_total > hospitalizations_total) %>% 
    filter(cases_total > deaths_total)
  
  # Generate year-week level df ----
  min_date <- min(covid_day_level_df$date) # grab the first date we see in the data 
  
  covid_year_week_level_df <- covid_day_level_df %>% 
    group_by(year, week) %>%
    summarise(cases_total = sum(cases_total),
              hospitalizations_total = sum(hospitalizations_total),
              deaths_total = sum(deaths_total)) %>% 
    ungroup() %>% 
    mutate(cumweek = row_number(),
           # first week on the x-axis should start at min_date 
           # increment by 7 days thereafter
           week_to_plot = min_date + 7 * (cumweek - 1))
}
