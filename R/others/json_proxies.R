library(glue)
library(jsonlite)
library(httr)
library(zeallot)

# Função que faz uso da API
# https://freeproxyapi.com
encontrando_proxies_api <- function(string_url = "https://www.zapimoveis.com.br",
                                    tipo_proxy = "https"){
  
  tipo_proxy <- tolower(tipo_proxy)
  
  tipo_proxy <- dplyr::case_when(
    tipo_proxy == "http"  ~ "3",
    tipo_proxy == "https" ~ "4"
  )
  
  try_GET <- function(...)
    tryCatch(
      expr = httr::GET(...),
      error = function(e) NA
    )
  
  teste_url <- function(x){
    httr::GET(
      url = string_url,
      httr::use_proxy(
        url = x$ip,
        port = x$port
      )
    )
  }
  
  obter_proxy <- function()
    glue("https://public.freeproxyapi.com/api/Proxy/ProxyByType/0/{tipo_proxy}") |> 
    jsonlite::fromJSON()
  
  proxy <- obter_proxy()
  result <- teste_url(proxy)
  
  while(!(class(result) == "response" && result$status_code == 200)){
    proxy <- obter_proxy()
    result <- teste_url(proxy)
  }
  result 
}

stirng_url <- "https://www.zapimoveis.com.br"

proxy <- encontrando_proxies_api()

httr::GET(
  url = string_url,
  httr::use_proxy(
    url = proxy$host,
    port = proxy$port
  )
)


