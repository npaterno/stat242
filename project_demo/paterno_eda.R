# Load Packages----------------------------------------------------------

library(dplyr) # For data wrangling
library(tidyr) # For data tidying
library(gapminder) # For data

# Load Data -------------------------------------------------------------

raw_data <- gapminder
View(raw_data)

# Calculate summary statistics for categorical variables ----------------

## What countries and continents are represented?
## How many years do we have data for each country?

countries <- raw_data %>% 
  group_by(country) %>% 
  summarize(count = n())

continents <- raw_data %>% 
  group_by(continent) %>% 
  summarize(count = n())

# Descriptive statistics for numeric variables --------------------------

## What years do we have data on?
project_data %>% 
  summarize(unique(year))

## Do we have data for each country for each year?
unique(countries$count)

## Calculate the five number summary for gdp for each continent across all years.
gdp_summary <- raw_data %>% 
  group_by(continent) %>% 
  summarize(five_num = fivenum(gdpPercap),
            num_countries = n()/12) %>% 
  mutate(name = c("min", "q1", "med", "q3", "max")) %>% 
  pivot_wider(names_from = name, 
              values_from = five_num)
## Calculate the five number summary for gdp globally across all years
global_gdp_summmary <- project_data %>% 
  summarize(five_num = fivenum(gdpPercap))

## Calculate the mean and standard deviation for life expectancy by country
life_stats <- raw_data %>% 
  group_by(country) %>% 
  summarize(life_mean = mean(lifeExp),
            life_sd = sd(lifeExp))

## Calculate the mean and standard deviation for population by country
pop_stats <- raw_data %>% 
  group_by(country) %>% 
  summarize(pop_mean = mean(pop),
            pop_sd = sd(pop))
