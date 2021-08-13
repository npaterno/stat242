library(readr)

# load data --------------------------------------------------------------------------

piracy <- read_csv(here::here("data-raw/piracy/piracy.csv"))

# cleaning: clean names using janitor

# Save -------------------------------------------------------------------------------

usethis::use_data(piracy, overwrite = TRUE)
