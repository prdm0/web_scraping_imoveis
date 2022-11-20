library(xml2)
library(rvest)
library(glue)
library(purrr)
library(dplyr)
library(stringr)
library(abjutils)
library(jsonlite)
library(gt)
library(tidygeocoder)
library(httr)
library(zeallot)

# Função que faz uso da API
# https://freeproxyapi.com
encontrando_proxies_api <- function(string_url = "https://www.zapimoveis.com.br/venda/imoveis/pb+joao-pessoa/?pagina=1"){
  
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
  
  obter_proxy <- function()
    glue("https://public.freeproxyapi.com/api/Proxy/ProxyByType/0/4") |> 
    jsonlite::fromJSON()
  
  proxy <- obter_proxy()
  result <- teste_url(proxy)
  
  repeat{
    
    if(result$status_code != 200)
      break
    
    if(class(result) == "response" && is.numeric(result$status_code))
      break
    
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

# Encontrando proxies usando o proxybroker2
# https://github.com/bluet/proxybroker2
encontrando_proxies_proxybroker2 <- function(
    n = 1L, 
    nivel = "alto",
    tipo = "https",
    paises = NULL){
  
  nivel <- tolower(nivel)
  
  nivel <- dplyr::case_when(
    nivel == "alto"         ~ "High",
    nivel == "anonimo"      ~ "Anonymous",
    nivel == "transparente" ~ "Transparent"
  )
  
  if(is.null(paises)) 
    lista_paises <- ""
  else 
    lista_paises <- glue("--countries {paste(paises, collapse = ' ')}")
  
  command <- 
    "docker run bluet/proxybroker2 find --types {tipo}\\
     --lvl {nivel} {lista_paises} --strict -l {n}
    " |> glue()
  
  proxy <-
    system(
      command = command,
      intern = TRUE,
      ignore.stderr = TRUE
    ) |> 
    as.character() |> 
    stringr::str_extract("\\d+.\\d+.\\d+.\\d+:\\d+")
  
  ip <- proxy |> stringr::str_extract("\\d+.\\d+.\\d+.\\d+")
  porta <- proxy |>
    stringr::str_remove(glue("{ip}:"))
  
  list(ip = ip, porta = as.integer(porta))
}

# Montando links para raspagem --------------------------------------------
montando_link <- 
  function(
    pagina = 1L,
    negocio = "venda",
    uf = "pb",
    cidade = "Joao Pessoa",
    bairro = "",
    imoveis = "imoveis",
    browser = TRUE
  ){
  
  # O argumento "negocio" poderá ser:
  # 1. venda
  # 2. aluguel
  # 3. lancamentos   
    
  # O argumento "imoveis" poderá ser:
  # 1. imoveis
  # 2. apartamentos
  # 3. casas
  # 4. quitinetes
  # 5. flat
  # 6. terrenos-lotes-condominios
  # 7. casas-de-condominio
  # 8. cobertura
  
  negocio <-   
    stringr::str_split(
      string = abjutils::rm_accent(tolower(negocio)),
      pattern = "\\s+"
    )[[1L]] |> 
    paste0(collapse = "-")
  
  imoveis <-   
    stringr::str_split(
      string = abjutils::rm_accent(tolower(imoveis)),
      pattern = "\\s+"
    )[[1L]] |> 
    paste0(collapse = "-")  
    
  negocio <- dplyr::case_when(
    stringr::str_detect(negocio, "^?vend") ~ "venda",
    stringr::str_detect(negocio, "^?alu")  ~ "aluguel",
    stringr::str_detect(negocio, "^?lanc") ~ "lancamentos",
    stringr::str_detect(negocio, "^?lanc") ~ "lancamentos"
  )  
  
  imoveis <- dplyr::case_when(
    stringr::str_detect(imoveis, "^?imo+?|^?tot+?|^?tod+?|^?tud+?")  ~ "imoveis",
    stringr::str_detect(imoveis, "^?apa+?|^?apt+?")                  ~ "apartamentos",
    stringr::str_detect(imoveis, "^?casas-+?")                       ~ "casas-de-condominio",
    stringr::str_detect(imoveis, "^?cas[s]*")                        ~ "casas",
    stringr::str_detect(imoveis, "^?qui+?|^?kit+?")                  ~ "quitinetes",
    stringr::str_detect(imoveis, "^?fla+?")                          ~ "flat",
    stringr::str_detect(imoveis, "^?ter+?|^?lot+?|^?cond+?")         ~ "terrenos-lotes-condominios",
    stringr::str_detect(imoveis, "cob+?|^?cob+?")                    ~ "cobertura"
  )
  
  cidade <-   
    stringr::str_split(
      string = abjutils::rm_accent(tolower(cidade)),
      pattern = "\\s+"
    )[[1L]] |> 
    paste0(collapse = "-") 
  
  bairro <-   
    stringr::str_split(
      string = abjutils::rm_accent(tolower(bairro)),
      pattern = "\\s+"
    )[[1L]] |> 
    paste0(collapse = "-") 

  url <- 
    "
    https://www.zapimoveis.com.br/\\
    {negocio}/\\
    {imoveis}/\\
    {tolower(uf)}+\\
    {cidade}
    " |> glue()
  
  if(bairro != ""){
    complemento_url <- 
      "
      ++\\
      {bairro}/?pagina=\\
      {pagina}
      " |> glue()
    url <- glue("{url}{complemento_url}")
  } else {
    url <- glue("{url}/?pagina={pagina}")  
  }
  
  try_encontrando_proxies_api <- function(...)
    tryCatch(
      expr = encontrando_proxies_api(...),
      error = function(e) NA
    )
  
  dados_url <- 
    try_encontrando_proxies_api(string_url = url)
  
  if(class(dados_url) != "response")
    return(NA)
  
  dados_url <- dados_url |> xml2::read_html()

  n_imoveis <- 
    dados_url |> 
    xml2::xml_find_first(xpath =  " //div[@class='results__list js-results']//strong") |> 
    xml2::xml_text() |> 
    stringr::str_remove("[:punct:]") |> 
    stringr::str_extract("\\d+") |> 
    as.integer()
    
  if(!browser) 
    return(
      list(
        numero_imoveis = as.integer(n_imoveis),
        url_string = as.character(url),
        url = dados_url
      )
    )
  
  if(browser)
    browseURL(url)
  
  return(
    list(
      numero_imoveis = as.integer(n_imoveis),
      url_string = as.character(url),
      url = dados_url
    )
  )
}

id_imovel <- function(url){
  
  list_ids <- 
    url |> 
    xml2::xml_find_all("//div[@class='listings__container']/div") |> 
    xml2::xml_attrs("data-id")
  
  purrr::map(
    .x = seq_along(list_ids), 
    .f = \(x) purrr::pluck(list_ids, x, 1L)
  ) |> unlist() |> as.numeric()
}

valor <- function(url){
  id <- id_imovel(url)
  
  steps <- function(i){
    result <-
      url |>
      xml2::xml_find_all(xpath = glue("//div[@data-id={i}]//p//strong")) |>
      xml2::xml_text() |>
      stringr::str_squish() |> 
      stringr::str_remove_all(pattern = "\\.+") |> 
      stringr::str_extract("[:digit:]+") |>
      lapply(FUN = \(i) paste0(i, collapse = "")) |> 
      unlist()
    
    if(is.null(result)) return(NA)
    
    mean(as.numeric(result))
  }
  
  try_steps <- function(...) tryCatch(exp = steps(...), error = function(e) NA)
  
  sapply(X = id, FUN = try_steps)
}

rua <- function(url) {
  id <- id_imovel(url)
  
  steps <- function(i){
    result <-
      url |>  
      xml2::xml_find_all(xpath = glue("//div[@data-id='{i}']//div/h2")) |>
      xml2::xml_text() |>
      stringr::str_squish() |> 
      unlist()
    
    if(is.null(result))
      return(NA)
    
    result[[1L]]
  }
  
  try_steps <- function(...) tryCatch(exp = steps(...), error = function(e) NA)
  
  sapply(X = id, FUN = try_steps)
}

area <- function(url){
  
  # Algumas áreas são das como intervalos. Nesse caso será considerada a média
  # Exemplo: 42 - 44 m²
  
  id <- id_imovel(url)
  
  steps <- function(i){
    result <- 
      url |> 
      xml2::xml_find_all(xpath = glue("//div[@data-id='{i}']//span[@itemprop='floorSize']")) |> 
      xml2::xml_text() |> 
      stringr::str_squish() |> 
      stringr::str_remove(pattern = " m²") |> 
      stringr::str_extract(pattern = "[[:digit:]]+") |> 
      stringr::str_extract_all("\\d+.")
    
    if(is.null(result)) return(NA)
    
    as.numeric(result[[1L]]) |>
      mean()
  }
  
  try_steps <- function(...) tryCatch(exp = steps(...), error = function(e) NA)
  
  sapply(X = id, FUN = try_steps)
}

quarto <- function(url){
  
  # Em alguns empreendimentos, a quantidade de quartos pode ser um intervalo.
  # Assim, o valor informado aqui é o número mínimo de quartos.
  
  id <- id_imovel(url)
  
  steps <- function(i){
    result <-
      url |> 
      xml2::xml_find_all(xpath = glue("//div[@data-id='{i}']//span[@itemprop='numberOfRooms']")) |> 
      xml2::xml_text() |> 
      stringr::str_squish() |> 
      stringr::str_extract_all("\\d") |>
      unlist()
    
    if(is.null(result)) return(NA)
    
    min(as.numeric(result))
  }
  
  try_steps <- function(...) tryCatch(exp = steps(...), error = function(e) NA)
  
  sapply(X = id, FUN = try_steps) |> 
    unlist()
}

vaga <- function(url){
  
  id <- id_imovel(url)
  
  steps <- function(i){
    result <- 
      url |> 
      xml2::xml_find_all(xpath = glue("//div[@data-id='{i}']//li[@class='feature__item text-small js-parking-spaces']//span")) |> 
      xml2::xml_text() |>
      stringr::str_squish() |> 
      stringr::str_extract_all(pattern = "\\d+") |>
      unlist() 
    
    if(length(result) == 0) return(NA)
    
    min(as.numeric(result))
  }
  
  try_steps <- function(...) tryCatch(exp = steps(...), error = function(e) NA)
  
  sapply(X = id, FUN = try_steps) |> 
    unlist()
}

banheiro <- function(url){
  id <- id_imovel(url)
  
  steps <- function(i){
    result <-
      url |> 
      xml2::xml_find_all(xpath = glue("//div[@data-id={i}]//span[@itemprop='numberOfBathroomsTotal']")) |>
      xml2::xml_text() |>
      stringr::str_squish() |> 
      stringr::str_extract_all(pattern = "\\d+") |>
      unlist() 
    
    if(is.null(result)) return(NA)
    
    min(as.numeric(result))
  }

  try_steps <- function(...) tryCatch(exp = steps(...), error = function(e) NA)
  
  sapply(X = id, FUN = try_steps) |> 
    unlist()
}

condominio <- function(url){
  
  id <- id_imovel(url)
  
  steps <- function(i) {
    result <-
      url |> 
      xml2::xml_find_all(
        xpath = glue(
          "//div[@data-id={i}]//ul[@class='prices prices--regular']//\\
          li[@class='card-price__item condominium text-regular']//\\
          span[@class='card-price__value']"
        )
      ) |>
      xml2::xml_text() |>
      stringr::str_squish() |>
      stringr::str_remove(pattern = "\\.+") |> 
      stringr::str_extract(pattern = "[[:digit:]]+") |> 
      as.numeric()
    
    if(length(result) == 0) return(NA)
    
    mean(result)
  }
  
  try_steps <- function(...) tryCatch(exp = steps(...), error = function(e) NA)
  
  sapply(X = id, FUN = try_steps) |> 
    unlist()
}

iptu <- function(url){
  id <- id_imovel(url)
  
  steps <- function(i) {
    result <-
      url |> 
      xml2::xml_find_all(
        xpath = glue(
          "//div[@data-id={i}]//ul[@class='prices prices--regular']//\\
          li[@class='card-price__item iptu text-regular']//\\
          span[@class='card-price__value']"
        )
      ) |>
      xml2::xml_text() |>
      stringr::str_squish() |>
      stringr::str_remove(pattern = "\\.+") |> 
      stringr::str_extract(pattern = "[[:digit:]]+") |> 
      as.numeric()
    
    if(length(result) == 0) return(NA)
    
    mean(result)
  }
  
  try_steps <- function(...) tryCatch(exp = steps(...), error = function(e) NA)
  
  sapply(X = id, FUN = try_steps) |> 
    unlist()
}

try_scraping <- function(
    uf = "pb",
    negocio = "venda",
    cidade = "joao pessoa",
    bairro = "altiplano cabo branco",
    imoveis = "tudo",
    cores = parallel::detectCores(),
    tab = TRUE){
  
  # O argumento "negocio" poderá ser:
  # 1. venda
  # 2. aluguel
  # 3. lancamentos   
  
  # O argumento "imoveis" poderá ser:
  # 1. imoveis
  # 2. apartamentos
  # 3. casas
  # 4. quitinetes
  # 5. flat
  # 6. terrenos-lotes-condominios
  # 7. casas-de-condominio
  # 8. cobertura
  
  conexao <- montando_link(
    pagina = 1L,
    negocio = negocio, 
    uf = uf,
    cidade = cidade,
    bairro = bairro,
    imoveis = imoveis,
    browser = FALSE
  )
    
  if(is.list(conexao) && !is.na(conexao$numero_imoveis))
    numero_imoveis <- conexao[["numero_imoveis"]]

  achar_numero_paginas <- function(){
    numero_paginas_aproximado <- ceiling(numero_imoveis/34)

    repeat{
      conexao <-
        montando_link(
          pagina = numero_paginas_aproximado,
          negocio = negocio,
          uf = uf,
          cidade = cidade,
          bairro = bairro,
          imoveis = imoveis,
          browser = FALSE
        )

      if(any(is.na(conexao))){
        numero_paginas_aproximado <- numero_paginas_aproximado - 1L
        next
      }

      if(is.list(conexao))
        break
    }
    numero_paginas_aproximado
  }
  
  numero_de_paginas <- achar_numero_paginas() |>
    suppressWarnings()
  
  #numero_de_paginas <- (numero_imoveis/34) |> ceiling()
  
  step <- function(i){
    
    Sys.sleep(sample(5L:12L, size = 1L))
    
    repeat{
      conexao <-
        montando_link(
          pagina = i,
          negocio,
          uf,
          cidade,
          bairro,
          imoveis,
          browser = FALSE
        )
      
      if(is.list(conexao))
        break
    }

    list(
      id = id_imovel(conexao$url),
      endereco = rua(conexao$url),
      valor = valor(conexao$url),
      area = area(conexao$url), 
      iptu = iptu(conexao$url),
      condominio = condominio(conexao$url),
      quarto = quarto(conexao$url),
      vaga = vaga(conexao$url),
      banheiro = banheiro(conexao$url) 
    ) |> data.frame()
  }
 
  raspagem <- 
    pbmcapply::pbmclapply(
      X = 1L:numero_de_paginas, FUN = \(i) step(i),
      mc.cores = cores
    )
  
  raspagem <- 
    purrr::list_rbind(raspagem) |> 
    tibble::as_tibble() |> 
    dplyr::mutate(bairro = bairro, .before = valor)

  if(tab){
    raspagem |>
      gt::gt() |>
      tab_header(
        title = md("**Web Scraping (Imoveis) - ZAP Imoveis**"),
        subtitle = glue("({toupper(cidade)} - {toupper(bairro)})")
      ) |>
      gt::tab_caption(glue("Tabela: Imóveis Negociados em {toupper(cidade)}, {toupper(bairro)}.")) |>
      tab_style(
        locations = cells_column_labels(columns = everything()),
        style  = list(
        cell_borders(sides = "bottom", weight = px(3)),
        cell_text(weight = "bold")
      ))
  }else{
    raspagem
  }
}

scraping <- function(...)
  tryCatch(
    expr = try_scraping(...),
    error = function(e) NULL
  )

bairros <- function(uf = "pb", cidade = "João Pessoa"){
  
  uf <- tolower(uf)
  estado <- case_when(
    uf == "ac" ~ "acre",
    uf == "al" ~ "alagoas",
    uf == "ap" ~ "amapa",
    uf == "am" ~ "amazonas",
    uf == "ba" ~ "bahia",
    uf == "ce" ~ "ceara",
    uf == "df" ~ "distrito-federal",
    uf == "es" ~ "espirito-santo",
    uf == "go" ~ "goias",
    uf == "ma" ~ "maranhao",
    uf == "mt" ~ "mato-grosso",
    uf == "ms" ~ "mato-grosso-do-sul",
    uf == "mg" ~ "minas-gerais",
    uf == "pa" ~ "para",
    uf == "pb" ~ "paraiba",
    uf == "pr" ~ "parana",
    uf == "pe" ~ "pernambuco",
    uf == "pi" ~ "piaui",
    uf == "rj" ~ "rio-de-janeiro",
    uf == "rn" ~ "rio-grande-do-norte",
    uf == "rs" ~ "rio-grande-do-sul",
    uf == "ro" ~ "rondonia",
    uf == "rr" ~ "ronraima",
    uf == "sp" ~ "sao-paulo",
    uf == "se" ~ "sergipe",
    uf == "to" ~ "tocantins",
  )

  estado <- estado |>
    stringr::str_squish() |> 
    abjutils::rm_accent() |> 
    stringr::str_replace_all(" ", "-") |> 
    tolower()
  
  cidade <- cidade |> 
    stringr::str_squish() |> 
    abjutils::rm_accent() |> 
    stringr::str_replace_all(" ", "-") |> 
    tolower()
  
  bairros_api <-
    glue::glue("https://cepbrasil.org/{estado}/{cidade}/") |>
    xml2::read_html() |> 
    xml2::xml_find_all("(//div[@class='col-md-6 coluna'])//a[@href]") |> 
    xml2::xml_text() |> 
    stringr::str_squish() |> 
    tolower() |> 
    abjutils::rm_accent() |> 
    stringr::str_replace_all(" ", "-") |> 
    suppressWarnings()
  
  if(cidade == "joao-pessoa"){
    complemento <- c(
      "ipes",
      "quadramares",
      "rangel",
      "trincheiras",
      "varadouro",
      "cid-dos-colibris",
      "jd-cid-universitaria",
      "jd-oceania",
      "jd-luna",
      "jd-s-paulo",
      "jd-veneza",
      "s-jose",
      "ponta-dos-seixas"
    )
    
    # Bairros em João Pessoa que não tem:
    # costa do sol
    # bairro dos novais
    # mata do buraquinho
    # mumbaba
    # mussure
    # padre zé
    
    return(c(bairros_api, complemento))
  }
}

varrer_cidade <- function(
    uf = "pb",
    negocio = "venda",
    cidade = "joao pessoa",
    imoveis = "tudo",
    intervalo_tempo = c(0,10),
    cores = parallel::detectCores()
  ){

  vetor_bairros <- 
    bairros(
      uf = uf,
      cidade = cidade
    )

  step <- function(b){
    Sys.sleep(sample(intervalo_tempo, size = 1L))
    cat("--> Bairro: ", b, '\n')
    
    scraping(
      uf = uf,
      negocio = negocio,
      cidade = cidade,
      bairro = b,
      imoveis = imoveis,
      cores = cores,
      tab = FALSE
    ) |> dplyr::mutate(bairro = b)
  }
  
  step_try <- function(...) 
    tryCatch(expr = step(...), error = function(e) NULL)
  
  list_data_frames <- 
    purrr::map(
      .x = vetor_bairros,
      .f = \(b) step_try(b)
    )
  
  purrr::list_rbind(list_data_frames) |>
    tibble::as_tibble()
}

# dados <- varrer_cidade()

add_coordenadas <- function(df, complemento = "João Pessoa, Brasil"){
  df |> 
    dplyr::mutate(
      end_completo = glue::glue("{df$endereco}, {complemento}"),
      .before = bairro
    ) |> 
    tidygeocoder::geocode(
      end_completo,
      method = 'arcgis',
      lat = latitude ,
      long = longitude
    ) |> 
    dplyr::select(-end_completo)
}

# dados_joao_pessoa_com_coord <- add_coordenadas(df = dados_joao_pessoa)
# save(
#   dados_joao_pessoa_com_coord,
#   file = "~/Dropbox/GitHub/API/dados_joao_pessoa_com_coord.RData"
# )