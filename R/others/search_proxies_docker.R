library(glue)
library(httr)
library(tibble)
# Função que faz uso da API
# https://freeproxyapi.com
encontrando_proxies_api <- function(n = 2000,
                            string_url = "https://www.zapimoveis.com.br/venda/imoveis/pb+joao-pessoa/?pagina=1",
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
  
  df_proxies <- tibble::tibble(
    ip = proxy$host,
    port = proxy$port
  )
  
  search <- function(i){
    repeat{
      result <- obter_proxy()
      teste <- teste_url(result)
      if(class(teste) == "response" && teste$status_code == 200){
        df_proxies <<- rbind(df_proxies, c(result[[1L]], result[[2L]]))
        break
      } 
    }
  }
  purrr::walk(.x = 1L:n, .f = search)
  df_proxies[-1,]
}

df_proxies <- encontrando_proxies_api(n = 1)

# Testando o acesso ao site da linguagem
httr::GET(
  "https://www.zapimoveis.com.br/venda/imoveis/pb+joao-pessoa/?pagina=1",
  httr::use_proxy(
    url = df_proxies$ip[1],
    port = as.integer(df_proxies$port[1])
  )
)

