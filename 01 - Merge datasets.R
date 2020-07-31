
library(tidyverse)
library(lubridate)
library(magrittr)

setwd("~/Github/Conflict-and-infectious-disease")

dons <- read_csv('./Infectious Disease Data/DON-1.1.0001.csv')

# 0. Outbreak deaths 

dons %>% group_by(DONid) %>% summarize(maxdeaths = max(`Death count`, na.rm = TRUE))



# 1. Aggregate DONs by country, month, year, outbreak identity, retain outbreak type

dons %<>% select("Year(s) of outbreak","Country","ISO code", "Disease/pathogen name", "Date recorded",
                "Pathogen etiology", "DONid") 

parse_date_time("1/22/1996", "m/d/y") # This was testing on one date

dons %>% mutate(time.fixed = parse_date_time(`Date recorded`, "m/d/y")) %>% # fixes the time part
  mutate(month = month(time.fixed),
         year = year(time.fixed)) %>% 
  select(-`Year(s) of outbreak`) %>%
  select(-time.fixed) %>% 
  select(-DONid) %>%
  select(-`Date recorded`) %>%
  unique() -> dons

# 2. Aggregate conflict by country, month, year, retain and aggregate deaths

con <- read_csv('./Conflict Data/Georeferenced Events Data/ged201.csv')

con %>% select(country, date_start, date_end, best) %>% 
  mutate(date_range = interval(date_start, date_end)) -> con # Make a column that stores "interval" objects we can work with

monther <- function(x) {seq(int_start(x), int_end(x), by = 'months')} # Make a custom function to solve this problem

con$months <- lapply(con$date_range, monther) # This is because mutate has trouble with stuff like this sometimes

con %>% mutate(months = as.vector(months)) %>% separate_rows(months) -> con # This splits each month of conflict out into its own unique row

con$months[con$months %in% c("","c")] <- NA # This is a weird painful data cleaning step - ignore it

con %>% mutate(months = as.numeric(months)) %>% 
  mutate(months = crawl::intToPOSIX(months)) %>% 
  na.omit() %>% unique() %>% 
  mutate(month = month(months),
         year = year(months)) -> con

con %>%
  select(-months, -date_range, -date_start, -date_end) %>%
  rename(deaths = best) %>% 
  rename(Country = country) -> con

con %>% group_by(Country, year, month) %>%
  summarize(deaths = sum(deaths, na.rm = TRUE)) %>% # Because several conflicts a month (see AFG)
  filter(year > 1995) -> con # Because DONs start in 96

con

# 3. Merge things

full_join(con, dons) %>% 
  mutate(conflict = !is.na(deaths)) %>%
  rename(Disease = `Disease/pathogen name`) %>%
  rename(Etiology = `Pathogen etiology`) %>%
  select(-`ISO code`) %>% 
  select(Country, year, month, conflict, deaths, Disease, Etiology) -> donflict

donflict %>% unique() -> donflict

donflict %>% mutate(conflict2 = ((conflict==TRUE) & deaths > 25)) -> donflict.test





# 4. An example analysis

prop.table(table(donflict$Etiology, donflict$conflict),1) %>% round(2)

prop.table(table(donflict$Disease, donflict$conflict),1) %>% data.frame() %>%
  filter(Var2 == 'TRUE') %>% View()
  round(2) %>%
  as.matrix() %>% data.frame() %>% View()

# Definitely a lot of things more likely during conflict!

# 5. Some things for Margaret to check

# A. Are there any country names that didn't merge right because they're not standardized? 

unique(donflict$Country)