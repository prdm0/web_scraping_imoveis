source("R/scraping/web_scraping_code.R")

dados <- scraping(bairro = "bessa")

cat("--> ", parallel::detectCores(), "\n")

print(dados)