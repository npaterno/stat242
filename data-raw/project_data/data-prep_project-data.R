# Load Packages -------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)

# Load Data (from gapminder.org/data) ---------------------------------------------------
file_path <- here::here("data-raw//project_data//")

csv_file_names <- file_path %>%
  list.files() %>%
  .[str_detect(., ".csv")]

csv_file_names %>%
  purrr::map(function(file_name){ # iterate through each file name
    assign(x = str_remove(file_name, ".csv"), # Remove file extension ".csv"
           value = read_csv(paste0(file_path, file_name)),
           envir = .GlobalEnv)
  })

# Clean Data ----------------------------------------------------------------------------

### Cell Phone Data ###
clean_cell <- cell_phones_total %>% 
  select(c(country,`1979`:`2018`)) %>% 
  pivot_longer(
    cols = `1979`:`2018`,
    names_to = "year",
    values_to = "num_phones"
  ) %>% 
  mutate(
    year = as.numeric(year),
    num_phones = gsub("([[:alpha:]]$)",",\\1",num_phones) 
  )  %>% 
  separate(num_phones, into = c("phones", "units"), sep = ",", fill = "right") %>% 
  mutate(phones = as.numeric(phones), 
         num_phones = case_when( 
           units == "k" ~ phones*10^3,
           units == "M" ~ phones*10^6,
           units == "B" ~ phones*10^9,
           TRUE ~ phones
         )
  ) %>% 
  select(-c("phones", "units"))

### Life Expectancy Data  ###
clean_life_exp <- life_expectancy_years %>% 
  pivot_longer(cols = `1799`:`2099`,
               names_to = "year",
               values_to = "life_exp") %>% 
  mutate(year = as.numeric(year),
         life_exp = as.numeric(life_exp)) %>% 
  filter(year %in% 1979:2018)

### Population Data ###
clean_population <- population_total %>% 
  pivot_longer(cols = `1799`:`2099`,
               names_to = "year",
               values_to = "population") %>% 
  mutate(
    population = gsub("([[:alpha:]]$)",",\\1", population)
  ) %>% 
  separate(population, into = c("pop", "units"), sep = ",", fill = "right") %>% 
  mutate(pop = as.numeric(pop), 
         population = case_when( 
           units == "k" ~ pop*10^3,
           units == "M" ~ pop*10^6,
           TRUE ~ pop
         )
  ) %>% 
  select(-c("pop", "units")) %>% 
  mutate(year = as.numeric(year),
         population = as.numeric(population)) %>% 
  filter(year %in% 1979:2018)

### Internet Use Data ###
net_mid <- net_users_num %>% 
  pivot_longer(cols = `1989`:`2018`, names_to = "year", values_to = "net_users") %>% 
  mutate(
    net_users = gsub("([[:alpha:]]$)",",\\1", net_users)
  ) %>% 
  separate(net_users, into = c("users", "units"), sep = ",", fill = "right") %>% 
  mutate(users = as.numeric(users),
         net_users = case_when(
           units == "k" ~ users*10^3,
           units == "M" ~ users*10^6,
           TRUE ~ users
         )) %>% 
  select(-c("users", "units")) %>% 
  mutate(year = as.numeric(year))

non_net_mid <- non_net_users_num %>% 
  pivot_longer(cols = `1989`:`2018`, names_to = "year", values_to = "non_net_users") %>% 
  mutate(
    non_net_users = gsub("([[:alpha:]]$)",",\\1", non_net_users)
  ) %>% 
  separate(non_net_users, into = c("users", "units"), sep = ",", fill = "right") %>% 
  mutate(users = as.numeric(users),
         non_net_users = case_when(
           units == "k" ~ users*10^3,
           units == "M" ~ users*10^6,
           units == "B" ~ users*10^9,
           TRUE ~ users
         )) %>% 
  select(-c("users", "units")) %>% 
  mutate(year = as.numeric(year))

clean_net_use <- net_mid %>% 
  full_join(non_net_mid)

### Join Data ###
project_data <- clean_cell %>% 
  left_join(clean_life_exp, by = c("country", "year")) %>% 
  left_join(clean_net_use, by = c("country", "year")) %>% 
  left_join(clean_population, by = c("country", "year")) %>% 
  left_join(continents, by = "country")

# Save Data -----------------------------------------------------------------------------
usethis::use_data(project_data, overwrite = TRUE)
