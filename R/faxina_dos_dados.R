library(dplyr)
library(stringr)
library(visdat)
library(tidyr)
library(abjutils)
library(pbmcapply)

zonas_jp <- function(x){
  dplyr::case_when(
    x == "agua_fria" ~ "sul",
    x == "altiplano_cabo_branco" ~ "leste",
    x == "alto_do_ceu" ~ "norte",
    x == "alto_do_mateus" ~ "oeste",
    x == "anatolia" ~ "sul",
    x == "aeroclube" ~ "leste",
    x == "bancarios" ~ "sul",
    x == "barra_de_gramame" ~ "sul",
    x == "bessa" ~ "leste",
    x == "brisamar" ~ "leste",
    x == "centro" ~ "norte",
    x == "castelo_branco" ~ "leste",
    x == "costa_e_silva" ~ "sul",
    x == "cristo_redentor" ~ "sul",
    x == "cruz_das_armas" ~ "oeste",
    x == "cuia" ~ "sul",
    x == "cidade_dos_colibris" ~ "sul",
    x == "cabo_branco" ~ "leste",
    x == "costa_do_sol" ~ "sul",
    x == "distrito_industrial" ~ "sul",
    x == "ernani_satiro" ~ "sul",
    x == "ernesto_geisel" ~ "sul",
    x == "estados" ~ "norte",
    x == "expedicionarios" ~ "norte",
    x == "funcionarios" ~ "sul",
    x == "gramame" ~ "sul",
    x == "grotao" ~ "sul",
    x == "industrias" ~ "sul",
    x == "ipes" ~ "norte",
    x == "joao_agripino" ~ "leste",
    x == "joao_paulo_ii" ~ "sul",
    x == "jose_americo_de_almeida" ~ "sul",
    x == "jardim_veneza" ~ "oeste",
    x == "jardim_luna" ~ "leste",
    x == "jaguaribe" ~ "oeste",
    x == "jardim_cidade_universitaria" ~ "sul",
    x == "jardim_oceania" ~ "leste",
    x == "jardim_sao_paulo" ~ "sul",
    x == "manaira" ~ "leste",
    x == "mandacaru" ~ "norte",
    x == "mangabeira" ~ "sul",
    x == "mucumagro" ~ "sul",
    x == "miramar" ~ "leste",
    x == "oitizeiro" ~ "oeste",
    x == "paratibe" ~ "sul",
    x == "planalto_boa_esperanca" ~ "sul",
    x == "portal_do_sol" ~ "leste",
    x == "ponta_dos_seixas" ~ "leste",
    x == "pedro_gondim" ~ "norte",
    x == "penha" ~ "leste",
    x == "quadramares" ~ "leste",
    x == "roger" ~ "norte",
    x == "rangel" ~ "oeste",
    x == "sao_jose" ~ "leste",
    x == "s_jose" ~ "leste",
    x == "tambia" ~ "norte",
    x == "treze_de_maio" ~ "norte",
    x == "trincheiras" ~ "oeste",
    x == "tambau" ~ "leste",
    x == "tambauzinho" ~ "leste",
    x == "torre" ~ "norte",
    x == "varadouro" ~ "norte",
    x == "varjao" ~ "oeste",
    x == "valentina_de_figueiredo" ~ "sul",
    x == "tambuzinho" ~ "leste",
    x == "jardim_esther" ~ "norte",
    x == "ilha_do_bispo" ~ "oeste"
  )
}

distrito_cabedelo <- function(x){
  if(x %in% c("morada_nova", "parque_esperanca", "renascer", "salinas")){
    return("renascer")
  } else {
    return("cabedelo")
  }
}

vec_distrito_cabedelo <- Vectorize(FUN = distrito_cabedelo, vectorize.args = "x")
  
faxina <- function(path = "joao_pessoa"){
  
  load(glue::glue("dados/{path}/dados_atualizados/dados.RData"))
  
  # if(ncol(dados) > 14L){
  #   return(dados)
  # }
  
  load(glue::glue("dados/{path}/dados_atualizados/covariaveis/dados_academia.RData"))
  load(glue::glue("dados/{path}/dados_atualizados/covariaveis/dados_acesso_24_horas.RData"))
  load(glue::glue("dados/{path}/dados_atualizados/covariaveis/dados_elevador.RData"))
  load(glue::glue("dados/{path}/dados_atualizados/covariaveis/dados_piscina.RData"))
  load(glue::glue("dados/{path}/dados_atualizados/covariaveis/dados_playground.RData"))
  load(glue::glue("dados/{path}/dados_atualizados/covariaveis/dados_quadra.RData"))
  load(glue::glue("dados/{path}/dados_atualizados/covariaveis/dados_salao_festa.RData"))
  load(glue::glue("dados/{path}/dados_atualizados/covariaveis/dados_sauna.RData"))
  load(glue::glue("dados/{path}/dados_atualizados/covariaveis/dados_spa.RData"))
  load(glue::glue("dados/{path}/dados_atualizados/covariaveis/dados_varanda_gourmet.RData"))
  
  dados <- dados |> 
    filter(
      !is.na(id),
      !is.na(valor), 
      !is.na(tipo),
      !is.na(endereco),
      !is.na(area)
    ) 
  
  # Irá introduzir com zero os valores de covariaveis
  # dos terrenos:
  covariaveis_terreno <- function(dados){
    id_terreno <- startsWith(dados$tipo, "terrenos")
    
    dados["banheiro"][id_terreno,] <- 0
    dados["vaga"][id_terreno,] <- 0
    dados["quarto"][id_terreno,] <- 0
    dados["vaga"][id_terreno,] <- 0
    
    id_terreno <- startsWith(dados$tipo, "terrenos_lotes_comerciais")
    dados["condominio"][id_terreno,] <- 0
    dados
  }
  
  check_endereco <- function(endereco){
    
    endereco <- 
      abjutils::rm_accent(endereco) |> 
      tolower() |> 
      stringr::str_replace_all(pattern = " ", "-")
    
    if(path == "joao_pessoa"){
      complemento <- c(
        "ipes",
        "quadramares",
        "rangel",
        "trincheiras",
        "varadouro",
        "cidade-dos-colibris",
        "jardim-cidade-universitaria",
        "jardim-oceania",
        "jardim-luna",
        "jardim-sao-paulo",
        "jardim-veneza",
        "sao-jose",
        "ponta-dos-seixas"
      )
    } else if(path == "cabedelo"){
      complemento <- c(
        "camboinha",
        "jardim-brasilia",
        "jardim-america",
        "loteamento-recanto-do-poco",
        "santa-catarina",
        "loteamento-parque-esperanca",
        "parque-verde"
      )
    }
      
    vetor_bairros <- c(unique(dados$bairro), complemento)
    
    r <- sapply(X = vetor_bairros, FUN = \(b) stringr::str_extract(endereco, paste0(b,"\\b")))
    
    r[!is.na(r)][1L]
  }
  
  dados <- covariaveis_terreno(dados)
  dados <- dplyr::distinct(dados, id, .keep_all = TRUE)
  
  bairros <- 
    pbmcapply::pbmclapply(
      X = unlist(dados$endereco), FUN = \(x) check_endereco(x),
      mc.cores = parallel::detectCores()
    )
  
  dados$bairro <- bairros
  
  dados <- dados |> filter(!is.na(bairro))
  
  dados <- dados |> 
    dplyr::mutate(b = substr(bairro, 1L, 1L)) |>
    dplyr::arrange(b) |> 
    dplyr::select(-b)
  
  piscina <- dados$id %in% dados_piscina$id
  elevador <- dados$id %in% dados_elevador$id
  salao_de_festa <- dados$id %in% dados_salao_festa$id
  academia <- dados$id %in% dados_academia$id
  playground <- dados$id %in% dados_academia$id
  quadra_de_esporte <- dados$id %in% dados_quadra$id
  portaria_24_horas <- dados$id %in% dados_acesso_24_horas$id
  varanda_gourmet <- dados$id %in% dados_varanda_gourmet$id
  sauna <- dados$id %in% dados_sauna$id
  spa <- dados$id %in% dados_spa$id
  
  # Agregando covariaveis:
  
  dados$piscina <- piscina
  dados$elevador <- elevador
  dados$salao_de_festa <- salao_de_festa
  dados$academia <- academia
  dados$playground <- playground
  dados$quadra_de_esporte <- quadra_de_esporte
  dados$portaria_24_horas <- portaria_24_horas
  dados$varanda_gourmet <- varanda_gourmet
  dados$sauna <- sauna
  dados$spa <- spa
  dados$bairro <- stringr::str_replace_all(dados$bairro, "-", "_")
  
  dados$comercio <- dados$tipo |> endsWith("comerciais")
  
  if(path == "joao_pessoa"){
    dados <- dados |> 
      dplyr::mutate(zona = zonas_jp(bairro), .after = bairro) |> 
      dplyr::relocate(latitude, .after = zona) |> 
      dplyr::relocate(latitude, longitude, .after = zona) |> 
      dplyr::relocate(iptu, condominio, .after = spa) |> 
      dplyr::relocate(tipo, .after = longitude) |> 
      dplyr::relocate(comercio, .after = tipo)
  } else if(path == "cabedelo") {
    dados <- dados |> 
      dplyr::mutate(distrito = vec_distrito_cabedelo(bairro), .after = bairro) |> 
      dplyr::relocate(latitude, .after = distrito) |> 
      dplyr::relocate(latitude, longitude, .after = distrito) |> 
      dplyr::relocate(iptu, condominio, .after = spa) |> 
      dplyr::relocate(tipo, .after = longitude) |> 
      dplyr::relocate(comercio, .after = tipo)
  }
  
  return(dados)
}