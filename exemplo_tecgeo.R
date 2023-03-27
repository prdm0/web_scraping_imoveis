library(tidymodels)

source("R/scraping/web_scraping_code.R")
source("R/faxina_dos_dados.R")
load("modelo_final_random_forest_cv20_grid_50.rda")


# Exemplo de faxina dos dados ---------------------------------------------
df <- faxina(path = "joao_pessoa")


# Exemplo de raspagem dos dados -------------------------------------------
dados <-
  scraping(
    uf = "PB",
    negocio = "venda",
    cidade = "joao pessoa",
    bairro = "bancarios",
    imoveis = "imoveis",
    cores = parallel::detectCores(),
    covariavel = "",
    tab = FALSE
)

# Exemplo de previsão -----------------------------------------------------
previsao <- function(endereco = "",
                     zona = "leste",
                     tipo = "apartamento",
                     comercio = FALSE,
                     area,
                     quarto,
                     vaga,
                     banheiro,
                     piscina = FALSE,
                     elevador = FALSE,
                     salao_de_festa = FALSE,
                     academia = FALSE,
                     playground = FALSE,
                     quadra_de_esporte = FALSE,
                     portaria_24_horas = FALSE,
                     varanda_gourmet = FALSE,
                     sauna = FALSE,
                     spa = FALSE) {
  coordenadas <- tidygeocoder::geo(endereco)
  
  latitude <- coordenadas$lat
  longitude <- coordenadas$long
  
  new_df <-
    tibble::tribble(
      ~ zona,
      ~ latitude,
      ~ longitude,
      ~ tipo,
      ~ comercio,
      ~ area,
      ~ quarto,
      ~ vaga,
      ~ banheiro,
      ~ piscina,
      ~ elevador,
      ~ salao_de_festa,
      ~ academia,
      ~ playground,
      ~ quadra_de_esporte,
      ~ portaria_24_horas,
      ~ varanda_gourmet,
      ~ sauna,
      ~ spa,
      zona,
      latitude,
      longitude,
      tipo,
      comercio,
      area,
      quarto,
      vaga,
      banheiro,
      piscina,
      elevador,
      salao_de_festa,
      academia,
      playground,
      quadra_de_esporte,
      portaria_24_horas,
      varanda_gourmet,
      sauna,
      spa
    )
  (10 ^ predict(modelo_final,  new_data = new_df)) |> round(0L)
}

# Exemplo (previsoes) -----------------------------------------------------

previsao(
  endereco = "Rua Randal Cavalcante Pimentel, Bessa, João Pessoa, Paraíba, Brasil",
  zona = "leste",
  tipo = "apartamentos",
  area = 120,
  quarto = 2,
  vaga = 1,
  banheiro = 2,
  piscina = FALSE,
  elevador = FALSE,
  varanda_gourmet = FALSE,
  salao_de_festa = TRUE,
  academia = FALSE,
  quadra_de_esporte = FALSE,
  portaria_24_horas = FALSE,
  playground = FALSE,
  spa = FALSE
)
