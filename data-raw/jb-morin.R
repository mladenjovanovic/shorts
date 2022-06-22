## code to prepare `jb_morin` dataset

# Load data
jb_morin <- read.csv("data-raw/jb-morin-data.csv", header = TRUE)

usethis::use_data(jb_morin, overwrite = TRUE)
