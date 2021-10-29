# Load packages --------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(moderndive)

# Load data and plot from part 2 ---------------------------------------------
source(here::here("project_demo/paterno_data-viz.R"))

# Check correlation ----------------------------------------------------------

get_correlation(data = raw_data %>% group_by(continent), 
                formula = lifeExp ~ gdpPercap)

# Given the sample sizes (count from the continents data) all of these are 
# actually enough to say there is a correlation between gdp per capita and 
# life expectancy. For the continents where it is lower, there more significant
# contributing factors.

# Build model ----------------------------------------------------------------
# I'll focus on Europe (you can focus on one continent/country as well) since
# it had a high correlation value and relatively large sample size 

europe <- raw_data %>% 
  filter(continent == "Europe")

life_gdp_mod <- lm(formula = lifeExp ~ gdpPercap, 
                   data = europe)

# Get the regression table
reg_table <- tibble(get_regression_table(life_gdp_mod))

# Plot regression model
ggplot(europe, aes(gdpPercap, lifeExp, size = pop/10^6))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE)+
  labs(
    title = "Life Expectancy v. GDP per Capita in Europe",
    subtitle = "1952 - 2007",
    size = "Population (millions)",
    x = "GDP per Capita",
    y = "Life Expectancy",
    caption = "Source: Gapminder"
  )+
  theme_economist()+ # from ggthemes
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Since the size of the points is the population, its reasonable to 
# guess that the aparent outliers are the same country. Let's check
# with a facet wrap by year.
ggplot(europe, aes(gdpPercap, lifeExp, size = pop/10^6))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE)+
  facet_wrap(~year)+
  labs(
    title = "Life Expectancy v. GDP per Capita in Europe",
    subtitle = "1952 - 2007",
    size = "Population (millions)",
    x = "GDP per Capita",
    y = "Life Expectancy",
    caption = "Source: Gapminder"
  )+
  theme_economist()+ # from ggthemes
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# This suggests it is one country. Let's remove it and rerun the analysis 
# to see if it improves. 
  
europe %>% filter(lifeExp < 50)

# It's Turkey! 

europe_2 <- europe %>% 
  filter(country != "Turkey")

get_correlation(europe_2, lifeExp ~ gdpPercap)

europe_2_model <- lm(lifeExp ~ gdpPercap, data = europe_2)

eruope_2_model_summary <- tibble(get_regression_table(europe_2_model))

ggplot(europe_2, aes(gdpPercap, lifeExp, size = pop/10^6))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE)+
  labs(
    title = "Life Expectancy v. GDP per Capita in Europe",
    subtitle = "1952 - 2007",
    size = "Population (millions)",
    x = "GDP per Capita",
    y = "Life Expectancy",
    caption = "Source: Gapminder"
  )+
  theme_economist()+ # from ggthemes
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
