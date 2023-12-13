## code to prepare `dynaspeed` dataset goes here

# Load data
dynaspeed <- read.csv("data-raw/dynaspeed.csv", header = TRUE)
dynaspeed$time <- dynaspeed$time + 1.43
dynaspeed$distance <- dynaspeed$distance + 2.74

usethis::use_data(dynaspeed, overwrite = TRUE)
