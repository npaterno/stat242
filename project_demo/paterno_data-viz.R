# Load Packages -----------------------------------------------------------

library(dplyr) # For data wrangling
library(tidyr) # For data tidying
library(ggthemes) # For presentation plots
library(ggplot2) # For plotting data
library(gapminder) # For data

# Load data from part I ---------------------------------------------------

source(here::here("project_demo/paterno_eda.R"))

# Histograms --------------------------------------------------------------

## Histogram for life expectancy with no grouping
ggplot(raw_data, aes(lifeExp)) +
  geom_histogram()

## Grouping by continent via color
ggplot(raw_data, aes(lifeExp, fill = continent)) +
  geom_histogram(position = "dodge")

## Grouping by continent via facets
ggplot(raw_data, aes(lifeExp, fill = as.factor(year))) +
  geom_histogram()+
  facet_wrap(~continent)

# Scatter plots -----------------------------------------------------------

## Population v Year
ggplot(raw_data, aes(year, pop))+
  geom_point()

## Life Expectancy v Year
ggplot(raw_data, aes(year, lifeExp))+
  geom_point()

## Life Expectancy v Pop
ggplot(raw_data, aes(pop, lifeExp))+
  geom_point()

## Life Expectancy v GDP
ggplot(raw_data, aes(gdpPercap, lifeExp, color = continent))+
  geom_point()+
  facet_wrap(~continent)

## Presentation plot (not required for individual files but required for group file)
ggplot(raw_data, aes(gdpPercap, lifeExp, size = pop/10^6))+
  geom_point(alpha = 0.2)+
  facet_wrap(~continent)+
  labs(
    title = "Life Expectancy v. GDP per Capita by Continent",
    subtitle = "1952 - 2007",
    size = "Population (millions)",
    x = "GDP per Capita",
    y = "Life Expectancy",
    caption = "Source: Gapminder"
  )+
  theme_economist()+ # from ggthemes
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Box plot ----------------------------------------------------------------

ggplot(raw_data, aes(lifeExp, reorder(continent, lifeExp)))+
  geom_boxplot(outlier.alpha = 0.5)
