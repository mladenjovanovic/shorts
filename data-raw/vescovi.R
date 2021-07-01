## code to prepare `vescovi` dataset
require(tidyverse)
require(shorts)

# Load data
vescovi <- read_csv("vescovi_data.csv")

# Sort the athletes from fastest 35m to slowest
vescovi <- vescovi %>%
  mutate(Athlete = fct_reorder(Athlete, `35m`)) %>%
  arrange(`35m`) %>%
  # Add splits
  mutate(
    `10m-5m split` = `10m` - `5m`,
    `20m-10m split` = `20m` - `10m`,
    `30m-20m split` = `30m` - `20m`,
    `35m-30m split` = `35m` - `30m`
  )

usethis::use_data(vescovi, overwrite = TRUE)
