source("R/scraping/web_scraping_code.R")

scraping(bairro = "bessa", tab = FALSE)

cat("--> ", parallel::detectCores(), "\n")