source("R/scraping/web_scraping_code.R")

cat(try_scraping(bairro = "bessa", tab = FALSE))

cat("--> ", parallel::detectCores(), "\n")