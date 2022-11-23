source("R/scraping/web_scraping_code.R")

dados <- scraping(bairro = "bessa", tab = FALSE)

dados

cat("--> ", parallel::detectCores(), "\n")