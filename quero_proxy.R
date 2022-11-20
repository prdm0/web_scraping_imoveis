library(rlang)
library(stringr)
library(rlang)
library(httr)
library(glue)
library(zeallot)

quero_conexao <- function(url, requisicao = GET, tipo = "https", ...){

  request <- function(...)
    rlang::call2(.fn = requisicao, ...) |> 
      eval()
  
  try_request <- function(...)
    tryCatch(
      expr = request(...),
      error = function(e) NA
    )
  
  tipo_proxy <- 
    dplyr::case_when(
      tipo == "http"  ~ "3",
      tipo == "https" ~ "4"
    )
  
  obter_proxy <- function()
    glue("https://public.freeproxyapi.com/api/Proxy/ProxyByType/0/{tipo_proxy}") |> 
      jsonlite::fromJSON()
  
  # Foçando a busca de proxies que estabeleça a conexão
  repeat{
    c(ip, porta, ...) %<-% obter_proxy()
    
    conexao <- 
      try_request(
        url = url,
        use_proxy(
           url = ip,
           port = porta
        ),
        ...
      )
    
    if(class(conexao) == "response" && conexao$status_code == 200)
      break
  }
  
  # Devolvendo a conexao
  conexao
}

quero_conexao(
  url = "https://www.imdb.com",
  tipo = "https",
  requisicao = GET
)
