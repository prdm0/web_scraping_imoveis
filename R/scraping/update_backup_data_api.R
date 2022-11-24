# Lendo script com funcoes para fazer o web scraping ---------------------
source(file = "R/scraping/web_scraping_code.R")

fazendo_backup <- function(){
  
  load("data/updated_data/data.RData")
  
  if(!is.data.frame(data) || any(dim(data) == 0)){
    warning(" - O backup não pode ser realizado: check o seu webscraping.")
    return(NULL)
  }
  
  # Retorna um vetor com data e hora
  data_hora <- function()
      Sys.time() |> stringr::str_split(pattern = " ") |> (\(.) `[[`(., 1L))() |> 
      paste(collapse = "_") |> 
      stringr::str_replace_all(pattern = ":", replacement = "_")
  
  # Sera colocado a data de 
  fs::file_copy(
    path = glue::glue("data/updated_data/data.RData"),
    new_path = glue::glue("data/backup_data/data_{data_hora()}.RData")
  )
}

# Raspando dados de uma cidade --------------------------------------------
raspando_e_salvando <- function(..., complemento = "João Pessoa, Brasil", geo = TRUE){
  try_varrer_cidade <- function(...)
    tryCatch(
      expr = varrer_cidade(...),
      error = function(e) NULL
    )

  data <- varrer_cidade(...)

  if(is.null(data))
    return(NULL)

  # Adicionando geolocalizacao ----------------------------------------------
  
  if(geo)
    data <- add_coordenadas(df = data, complemento = complemento)

  # Atualizando a base de dados (dados completos) ---------------------------
  if(exists("data") && is.data.frame(data) && all(dim(data) != 0))
    save(data, file = "data/updated_data/data.RData")
}

# Realizando o backup -----------------------------------------------------
fazendo_backup()

# Raspando e atulizando os dados ------------------------------------------
raspando_e_salvando(intervalo_tempo = c(0, 0))