library(stringr)
library(purrr)
library(glue)

extrair_ip_porta <- function(string) {
  padrao <- "^(?:[^ ]+ ){4}([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+):([0-9]+)"
  correspondencias <- str_match(string, padrao)
  if (is.na(correspondencias[1L])) {
    stop("Padrão não encontrado na string fornecida.")
  } else {
    ip <- correspondencias[2L]
    porta <- correspondencias[3L]
    c(ip, porta)
  }
}

obter_proxy <- function(n = 1L){
  l <- 
    glue("proxybroker find --types HTTPS --strict -l {n}") |> 
    system(ignore.stderr = TRUE, intern = TRUE) |> 
    extrair_ip_porta()
  
  list(ip = l[[1L]], port = as.integer(l[[2L]]))
}

#m = obter_proxy()

###############

encontrando_proxies_api2 <- function(string_url = "https://www.zapimoveis.com.br/venda/imoveis/pb+joao-pessoa/?pagina=1"){
  try_GET <- function(...)
    tryCatch(
      expr = httr::GET(...),
      error = function(e) NA
    )
  
  teste_url <- function(x){
    try_GET(
      url = string_url,
      httr::use_proxy(
        url = x$ip,
        port = x$port
      )
    )
  }
  
  proxy <- obter_proxy()
  result <- teste_url(proxy)
  
  repeat{
    
    if(result$status_code == 200)
      break
    
    # if(class(result) == "response" && is.numeric(result$status_code))
    #   break

    proxy <- obter_proxy()
    result <- teste_url(proxy)
  }
  
  pagina_vazia <- function(){
    string_url |> 
      xml2::read_html() |> 
      xml2::xml_find_first(xpath =  "//div[@class='results__list js-results']//strong")
  }
  
  if(result$status_code != 200 | is.na(pagina_vazia()))
    return(NA)
  else
    return(result)
}
