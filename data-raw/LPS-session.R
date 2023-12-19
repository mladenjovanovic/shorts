## code to prepare `LPS_session` dataset goes here

# Load data
LPS_session <- read.csv("data-raw/LPS-session.csv", header = TRUE)

LPS_session$clock <- NULL

usethis::use_data(LPS_session, overwrite = TRUE)
