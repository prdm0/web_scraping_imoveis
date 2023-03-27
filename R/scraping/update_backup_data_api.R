library(emo)
library(tictoc)

# Lendo script com funcoes para fazer o web scraping ---------------------
source(file = "R/scraping/web_scraping_code.R")

# Lendo script com função para faxinar os dados ---------------------------
source(file = "R/faxina_dos_dados.R")

fazendo_backup <- function(path = "joao_pessoa"){
  
  if(!fs::dir_exists(fs::path("dados", path))){
    fs::dir_create(fs::path("dados", path))
    fs::dir_create(fs::path("dados", path, "dados_atualizados"))
    fs::dir_create(fs::path("dados", path, "historico_dos_dados"))
    fs::dir_create(fs::path("dados", path, "dados_atualizados", "covariaveis"))
    return(NULL)
  }
  
  if(!fs::file_exists(fs::path("dados", path, "dados_atualizados" ,"dados.RData")))
    return(NULL)
  
  load(glue::glue("dados/{path}/dados_atualizados/dados.RData"))
  
  if(!is.data.frame(dados) || any(dim(dados) == 0)){
    warning(" - O backup não pode ser realizado: check o seu webscraping.")
    return(NULL)
  }
  
  # Retorna um vetor com dados e hora
  data_hora <- function()
      Sys.time() |> stringr::str_split(pattern = " ") |> (\(.) `[[`(., 1L))() |> 
      paste(collapse = "_") |> 
      stringr::str_replace_all(pattern = ":", replacement = "_")
  
  # Sera colocado a dados de 
  fs::file_copy(
    path = glue::glue("dados/{path}/dados_atualizados/dados.RData"),
    new_path = glue::glue("dados/{path}/historico_dos_dados/dados_{data_hora()}.RData")
  )
}

# Raspando dados de uma cidade --------------------------------------------
raspando_e_salvando <- function(path = "joao_pessoa", complemento = "Joao Pessoa, Paraiba, Brasil", geo = TRUE, ...){
  # try_varrer_cidade <- function(...)
  #   tryCatch(
  #     expr = varrer_cidade(...),
  #     error = function(e) NULL
  #   )

  # Casas
  cat("-->", emo::ji("check"), "CASAS\n")
  
  dados_casas <- varrer_cidade(imoveis = "casas", ...)
  
  cat("-->", emo::ji("check"), "CASAS DE CONDOMÍNIO\n")
  
  dados_casas_de_condominio <- varrer_cidade(imoveis = "casas-de-condominio", ...)
  
  cat("-->", emo::ji("check"), "CASAS DE VILA\n")
  
  dados_casas_de_vila <- varrer_cidade(imoveis = "casas-de-vila", ...)
  
  cat("-->", emo::ji("check"), "CASAS COMERCIAIS\n")
  
  dados_casas_comercial <- varrer_cidade(imoveis = "casa-comercial", ...)
  
  casas <- dplyr::bind_rows(
    dados_casas,
    dados_casas_de_condominio, 
    dados_casas_de_vila,
    dados_casas_comercial
  ) |> dplyr::mutate(
    tipo = 
      c(
        rep(
          "casas", 
          nrow(dados_casas)  
        ),
        rep(
          "casas_de_condominio",
          nrow(dados_casas_de_condominio)
        ),
        rep(
          "casas_de_vila",
          nrow(dados_casas_de_vila)
        ),
        rep(
          "casas_comerciais",
          nrow(dados_casas_comercial)
        )
      )
  )

  cat("-->", emo::ji("check"), "APARTAMENTOS\n")

  # Apartamentos
  apartamentos <- varrer_cidade(imoveis = "apartamentos", ...) |>
    dplyr::mutate(tipo = "apartamentos")


  cat("-->", emo::ji("check"), "FLATS\n")

  # Flats
  flats <- varrer_cidade(imoveis = "flat", ...) |>
    dplyr::mutate(tipo = "flat")

  cat("-->", emo::ji("check"), "TERRENOS\n")

  # Terrenos
  dados_terrenos_lotes_comerciais <- varrer_cidade(imoveis = "terrenos-lotes-comerciais", ...)
  dados_terrenos_lotes_condominios <- varrer_cidade(imoveis = "terrenos-lotes-condominios", ...)

  terrenos <- dplyr::bind_rows(
    dados_terrenos_lotes_condominios,
    dados_terrenos_lotes_comerciais
  ) |> dplyr::mutate(
          tipo =
            c(
              rep(
                "terrenos_lotes_condominio",
                nrow(dados_terrenos_lotes_condominios)
              ),
              rep(
                "terrenos_lotes_comerciais",
                nrow(dados_terrenos_lotes_comerciais)
              )
            )
  )

  cat("-->", emo::ji("check"), "PONTOS COMERCIAIS\n")

  # Ponto Comercial
  dados_conjunto_comercial_sala <- varrer_cidade(imoveis = "conjunto-comercial-sala", ...)
  dados_loja_salao <- varrer_cidade(imoveis = "loja-salao", ...)

  ponto_comercial <- dplyr::bind_rows(
    dados_conjunto_comercial_sala,
    dados_loja_salao
  ) |> dplyr::mutate(
    tipo =
      c(
        rep(
          "conjunto_comercial_sala",
          nrow(dados_conjunto_comercial_sala)
        ),
        rep(
          "loja_salao",
          nrow(dados_loja_salao)
        )
      )
  )

  dados <- dplyr::bind_rows(
    casas,
    apartamentos,
    terrenos,
    flats,
    ponto_comercial
  )
  
  if(is.null(dados))
    return(NULL)
  
  # Adicionando geolocalizacao ----------------------------------------------
  if(geo)
    dados <- add_coordenadas(df = dados, complemento = complemento)
  
  # Atualizando a base de dados (dados completos) ---------------------------
  if(exists("dados") && is.data.frame(dados) && all(dim(dados) != 0))
    save(dados, file = glue::glue("dados/{path}/dados_atualizados/dados.RData"))
}

raspando_covariaveis <- function(path = "joao_pessoa", ...){
  
  cat("-->", emo::ji("check"), "ELEVADOR\n")
  dados_elevador <- varrer_cidade(covariavel = "elevador", ...)
  
  cat("-->", emo::ji("check"), "PISCINA\n")
  dados_piscina <- varrer_cidade(covariavel = "piscina", ...)

  cat("-->", emo::ji("check"), "SALAO DE FESTAS\n")
  dados_salao_festa <- varrer_cidade(covariavel = "salao-de-festas", ...)

  cat("-->", emo::ji("check"), "ACADEMIA\n")
  dados_academia <- varrer_cidade(covariavel = "academia", ...)

  cat("-->", emo::ji("check"), "VARANDA GOURMET\n")
  dados_varanda_gourmet <- varrer_cidade(covariavel = "varanda-gourmet", ...)

  cat("-->", emo::ji("check"), "ACESSO 24 HORAS\n")
  dados_acesso_24_horas <- varrer_cidade(covariavel = "acesso-24-horas", ...)

  cat("-->", emo::ji("check"), "PLAYGROUND\n")
  dados_playground <- varrer_cidade(covariavel = "playground", ...)

  cat("-->", emo::ji("check"), "SPA\n")
  dados_spa <- varrer_cidade(covariavel = "spa-com-hidromassagem", ...)

  cat("-->", emo::ji("check"), "SAUNA\n")
  dados_sauna <- varrer_cidade(covariavel = "sauna", ...)

  cat("-->", emo::ji("check"), "QUADRA POLIESPORTIVA\n")
  dados_quadra <- varrer_cidade(covariavel = "quadra-poliesportiva", ...)

  save(dados_elevador, file = glue::glue("dados/{path}/dados_atualizados/covariaveis/dados_elevador.RData"))
  save(dados_piscina, file = glue::glue("dados/{path}/dados_atualizados/covariaveis/dados_piscina.RData"))
  save(dados_salao_festa, file = glue::glue("dados/{path}/dados_atualizados/covariaveis/dados_salao_festa.RData"))
  save(dados_academia, file = glue::glue("dados/{path}/dados_atualizados/covariaveis/dados_academia.RData"))
  save(dados_varanda_gourmet, file = glue::glue("dados/{path}/dados_atualizados/covariaveis/dados_varanda_gourmet.RData"))
  save(dados_acesso_24_horas, file = glue::glue("dados/{path}/dados_atualizados/covariaveis/dados_acesso_24_horas.RData"))
  save(dados_playground, file = glue::glue("dados/{path}/dados_atualizados/covariaveis/dados_playground.RData"))
  save(dados_spa, file = glue::glue("dados/{path}/dados_atualizados/covariaveis/dados_spa.RData"))  
  save(dados_sauna, file = glue::glue("dados/{path}/dados_atualizados/covariaveis/dados_sauna.RData"))
  save(dados_quadra, file = glue::glue("dados/{path}/dados_atualizados/covariaveis/dados_quadra.RData"))
}

pacman <- function(path = "joao_pessoa", complemento, ...){
  # Realizando o backup -----------------------------------------------------
  fazendo_backup(path = path)
  
  # Raspando e atulizando os dados ------------------------------------------
  raspando_e_salvando(path = path, complemento, ...)
  
  # Raspando as covariáveis:
  raspando_covariaveis(path = path, ...)
  
  # Faxina dos dados:
  dados <- faxina(path = path)
  save(dados, file = glue::glue("dados/{path}/dados_atualizados/dados.RData"))
}

tic()
pacman(
  path = "joao_pessoa",
  uf = "PB",
  cidade = "joao pessoa",
  complemento = "Joao Pessoa, Paraiba, Brasil"
)
toc()