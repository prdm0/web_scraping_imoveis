source("api_with_proxies.R")

valentina <- scraping(bairro = "valentina-de-figueiredo", tab = FALSE)

save(valentina, file = glue::glue("valentina_github_action_{Sys.time()}.RData"))