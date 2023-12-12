## code to prepare `dynaspeed` dataset goes here

# Load data
dynaspeed <- read.csv("data-raw/dynaspeed.csv", header = TRUE)

usethis::use_data(dynaspeed, overwrite = TRUE)
