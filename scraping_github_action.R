source("api_with_proxies.R")

valentina <- scraping(bairro = "valentina-de-figueiredo", tab = FALSE)

save(valentina, file = "valentina_github_action.RData")