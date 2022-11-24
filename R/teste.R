source("R/scraping/web_scraping_code.R")

dados <- scraping(bairro = "grotao", tab = FALSE, cores = 2)

print(dados)

cat("N. cores: ", parallel::detectCores(), "\n")