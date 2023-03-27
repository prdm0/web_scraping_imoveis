library(leaflet)
library(htmlwidgets)
library(leaflet.extras)
library(classInt)
library(scales)
library(tidymodels)

tidymodels::tidymodels_prefer()

# Lendo modelo
load("modelo_final_random_forest_cv20_grid_50.rda")

# Importando os dados faxinados:
source("R/faxina_dos_dados.R")
dados <- faxina()

casas <- dados |> dplyr::filter(startsWith(tipo, "casas"))
apartamentos <- dados |> dplyr::filter(tipo == "apartamento")
terrenos <- dados |> dplyr::filter(startsWith(tipo, "terrenos"))

# Data frames separados com as previsões:
previsoes_casas <- predict(modelo_final, new_data = casas) 
previsoes_apartamentos <- predict(modelo_final, new_data = apartamentos) 
previsoes_terrenos <- predict(modelo_final, new_data = terrenos) 
previsoes_tudo <- predict(modelo_final, new_data = dados)

# df_casas_previsoes <- 
#   casas |> 
#   mutate(previsao = base::round(10^previsoes_casas, 0L), .after = valor)
# 
# df_apartamentos_previsoes <- 
#   apartamentos |> 
#   mutate(previsao = base::round(10^previsoes_apartamentos, 0L), .after = valor)
# 
# df_terrenos_previsoes <- 
#   terrenos |> 
#   mutate(previsao = base::round(10^previsoes_terrenos, 0L), .after = valor)

dados <- bind_cols(dados, previsoes_tudo)
dados <- dados |> 
  mutate(.pred = 10^(.pred))

apartamentos <-  dados |> dplyr::filter(tipo == "apartamento")
terrenos <- dados |> dplyr::filter(startsWith(tipo, "terreno"))
casas <- dados |> dplyr::filter(startsWith(tipo, "casa"))

reais_formato <- scales::dollar_format(prefix = "R$ ")

mapa <- leaflet(dados) |> 
  setView(lng = -34.857807, lat = -7.152930, zoom = 13) 

mapa <- 
  mapa |> 
  addTiles() |> 
  addMarkers(
    data = dados,
    popup =
      paste0(
        "<b>ID: </b>", dados$id,"<br>",
        "<b>Bairro: </b>", toupper(stringr::str_replace_all(dados$bairro, "_", " ")), "<br>",
        #"<b>Endereço: </b>", toupper(dados$endereco), "<br>",
        "<b>Zona: </b>", toupper(dados$zona), "<br>",
        "<b>Tipo: </b>", toupper(dados$tipo),"<br>",
        "<b>Valor Anunciado: </b>", reais_formato(dados$valor),"<br>",
        "<b>Valor Previsto: </b>", reais_formato(dados$.pred),"<br>",
        "<b>Área: </b>", dados$area, " m²<br>",
        "<b>Nº. de quartos: </b>", dados$quarto, "<br>",
        "<b>Nº. de banheiros: </b>", dados$banheiro, "<br>",
        "<b>Nº. de vagas: </b>", dados$vaga, "<br>",
        "<b>Piscina: </b>", ifelse(dados$piscina, "Sim", "Não"), "<br>",
        "<b>Elevador: </b>", ifelse(dados$elevador, "Sim", "Não"), "<br>",
        "<b>Salão de Festa: </b>", ifelse(dados$salao_de_festa, "Sim", "Não"), "<br>",
        "<b>Academia: </b>", ifelse(dados$academia, "Sim", "Não"), "<br>",
        "<b>Playground: </b>", ifelse(dados$playground, "Sim", "Não"), "<br>",
        "<b>Quadra de Esporte: </b>", ifelse(dados$quadra_de_esporte, "Sim", "Não"), "<br>",
        "<b>Portaria 24h: </b>", ifelse(dados$portaria_24_horas, "Sim", "Não"), "<br>",
        "<b>Varanda Gourmet: </b>", ifelse(dados$varanda_gourmet, "Sim", "Não"), "<br>",
        "<b>Sauna: </b>", ifelse(dados$sauna, "Sim", "Não"), "<br>",
        "<b>Spa: </b>", ifelse(dados$spa, "Sim", "Não")
      ),
      label = ~tipo,
      group = "Agrupamento",
      clusterOptions = markerClusterOptions(freezeAtZoom = FALSE)
  ) |> 
  addCircleMarkers(
    data = dados,
    radius = 4,
    fillOpacity = 0.5, stroke = FALSE, 
    popup = paste0(
      "<b>ID: </b>", dados$id,"<br>",
      "<b>Bairro: </b>", toupper(stringr::str_replace_all(dados$bairro, "_", " ")), "<br>",
      #"<b>Endereço: </b>", toupper(dados$endereco), "<br>",
      "<b>Zona: </b>", toupper(dados$zona), "<br>",
      "<b>Tipo: </b>", toupper(dados$tipo),"<br>",
      "<b>Valor Anunciado: </b>", reais_formato(dados$valor),"<br>",
      "<b>Valor Previsto: </b>", reais_formato(dados$.pred),"<br>",
      "<b>Área: </b>", dados$area, " m²<br>",
      "<b>Nº. de quartos: </b>", dados$quarto, "<br>",
      "<b>Nº. de banheiros: </b>", dados$banheiro, "<br>",
      "<b>Nº. de vagas: </b>", dados$vaga, "<br>",
      "<b>Piscina: </b>", ifelse(dados$piscina, "Sim", "Não"), "<br>",
      "<b>Elevador: </b>", ifelse(dados$elevador, "Sim", "Não"), "<br>",
      "<b>Salão de Festa: </b>", ifelse(dados$salao_de_festa, "Sim", "Não"), "<br>",
      "<b>Academia: </b>", ifelse(dados$academia, "Sim", "Não"), "<br>",
      "<b>Playground: </b>", ifelse(dados$playground, "Sim", "Não"), "<br>",
      "<b>Quadra de Esporte: </b>", ifelse(dados$quadra_de_esporte, "Sim", "Não"), "<br>",
      "<b>Portaria 24h: </b>", ifelse(dados$portaria_24_horas, "Sim", "Não"), "<br>",
      "<b>Varanda Gourmet: </b>", ifelse(dados$varanda_gourmet, "Sim", "Não"), "<br>",
      "<b>Sauna: </b>", ifelse(dados$sauna, "Sim", "Não"), "<br>",
      "<b>Spa: </b>", ifelse(dados$spa, "Sim", "Não")
    ),
    label = ~tipo,
    group = "Todos"
  ) |> 
  addCircleMarkers(
    data = casas,
    radius = 4,
    fillOpacity = 0.5, stroke = FALSE, 
    popup = paste0(
      "<b>ID: </b>", dados$id,"<br>",
      "<b>Bairro: </b>", toupper(stringr::str_replace_all(casas$bairro, "_", " ")), "<br>",
      #"<b>Endereço: </b>", toupper(dados$endereco), "<br>",
      "<b>Zona: </b>", toupper(casas$zona), "<br>",
      "<b>Tipo: </b>", toupper(casas$tipo),"<br>",
      "<b>Valor Anunciado: </b>", reais_formato(casas$valor),"<br>",
      "<b>Valor Previsto: </b>", reais_formato(casas$.pred),"<br>",
      "<b>Área: </b>", casas$area, " m²<br>",
      "<b>Nº. de quartos: </b>", casas$quarto, "<br>",
      "<b>Nº. de banheiros: </b>", casas$banheiro, "<br>",
      "<b>Nº. de vagas: </b>", casas$vaga, "<br>",
      "<b>Piscina: </b>", ifelse(casas$piscina, "Sim", "Não"), "<br>",
      "<b>Elevador: </b>", ifelse(casas$elevador, "Sim", "Não"), "<br>",
      "<b>Salão de Festa: </b>", ifelse(casas$salao_de_festa, "Sim", "Não"), "<br>",
      "<b>Academia: </b>", ifelse(casas$academia, "Sim", "Não"), "<br>",
      "<b>Playground: </b>", ifelse(casas$playground, "Sim", "Não"), "<br>",
      "<b>Quadra de Esporte: </b>", ifelse(casas$quadra_de_esporte, "Sim", "Não"), "<br>",
      "<b>Portaria 24h: </b>", ifelse(casas$portaria_24_horas, "Sim", "Não"), "<br>",
      "<b>Varanda Gourmet: </b>", ifelse(casas$varanda_gourmet, "Sim", "Não"), "<br>",
      "<b>Sauna: </b>", ifelse(casas$sauna, "Sim", "Não"), "<br>",
      "<b>Spa: </b>", ifelse(casas$spa, "Sim", "Não")
    ),
    label = ~tipo,
    group = "Casas"
  ) |> 
  addCircleMarkers(
    data = apartamentos,
    radius = 4,
    fillOpacity = 0.5, stroke = FALSE, 
    popup = paste0(
      "<b>ID: </b>", dados$id,"<br>",
      "<b>Bairro: </b>", toupper(stringr::str_replace_all(apartamentos$bairro, "_", " ")), "<br>",
      #"<b>Endereço: </b>", toupper(dados$endereco), "<br>",
      "<b>Zona: </b>", toupper(apartamentos$zona), "<br>",
      "<b>Tipo: </b>", toupper(apartamentos$tipo),"<br>",
      "<b>Valor Anunciado: </b>", reais_formato(apartamentos$valor),"<br>",
      "<b>Valor Previsto: </b>", reais_formato(apartamentos$.pred),"<br>",
      "<b>Área: </b>", apartamentos$area, " m²<br>",
      "<b>Nº. de quartos: </b>", apartamentos$quarto, "<br>",
      "<b>Nº. de banheiros: </b>", apartamentos$banheiro, "<br>",
      "<b>Nº. de vagas: </b>", apartamentos$vaga, "<br>",
      "<b>Piscina: </b>", ifelse(apartamentos$piscina, "Sim", "Não"), "<br>",
      "<b>Elevador: </b>", ifelse(apartamentos$elevador, "Sim", "Não"), "<br>",
      "<b>Salão de Festa: </b>", ifelse(apartamentos$salao_de_festa, "Sim", "Não"), "<br>",
      "<b>Academia: </b>", ifelse(apartamentos$academia, "Sim", "Não"), "<br>",
      "<b>Playground: </b>", ifelse(apartamentos$playground, "Sim", "Não"), "<br>",
      "<b>Quadra de Esporte: </b>", ifelse(apartamentos$quadra_de_esporte, "Sim", "Não"), "<br>",
      "<b>Portaria 24h: </b>", ifelse(apartamentos$portaria_24_horas, "Sim", "Não"), "<br>",
      "<b>Varanda Gourmet: </b>", ifelse(apartamentos$varanda_gourmet, "Sim", "Não"), "<br>",
      "<b>Sauna: </b>", ifelse(apartamentos$sauna, "Sim", "Não"), "<br>",
      "<b>Spa: </b>", ifelse(apartamentos$spa, "Sim", "Não")
    ),
    label = ~tipo,
    group = "Apartamentos"
  ) |> 
  addCircleMarkers(
    data = terrenos,
    radius = 4,
    fillOpacity = 0.5, stroke = FALSE, 
    popup = paste0(
      "<b>ID: </b>", dados$id,"<br>",
      "<b>Bairro: </b>", toupper(stringr::str_replace_all(terrenos$bairro, "_", " ")), "<br>",
      #"<b>Endereço: </b>", toupper(dados$endereco), "<br>",
      "<b>Zona: </b>", toupper(terrenos$zona), "<br>",
      "<b>Tipo: </b>", toupper(terrenos$tipo),"<br>",
      "<b>Valor Anunciado: </b>", reais_formato(terrenos$valor),"<br>",
      "<b>Valor Previsto: </b>", reais_formato(terrenos$.pred),"<br>",
      "<b>Área: </b>", terrenos$area, " m²<br>",
      "<b>Nº. de quartos: </b>", terrenos$quarto, "<br>",
      "<b>Nº. de banheiros: </b>", terrenos$banheiro, "<br>",
      "<b>Nº. de vagas: </b>", terrenos$vaga, "<br>",
      "<b>Piscina: </b>", ifelse(terrenos$piscina, "Sim", "Não"), "<br>",
      "<b>Elevador: </b>", ifelse(terrenos$elevador, "Sim", "Não"), "<br>",
      "<b>Salão de Festa: </b>", ifelse(terrenos$salao_de_festa, "Sim", "Não"), "<br>",
      "<b>Academia: </b>", ifelse(terrenos$academia, "Sim", "Não"), "<br>",
      "<b>Playground: </b>", ifelse(terrenos$playground, "Sim", "Não"), "<br>",
      "<b>Quadra de Esporte: </b>", ifelse(terrenos$quadra_de_esporte, "Sim", "Não"), "<br>",
      "<b>Portaria 24h: </b>", ifelse(terrenos$portaria_24_horas, "Sim", "Não"), "<br>",
      "<b>Varanda Gourmet: </b>", ifelse(terrenos$varanda_gourmet, "Sim", "Não"), "<br>",
      "<b>Sauna: </b>", ifelse(terrenos$sauna, "Sim", "Não"), "<br>",
      "<b>Spa: </b>", ifelse(terrenos$spa, "Sim", "Não")
    ),
    label = ~tipo,
    group = "Terrenos"
  ) |> 
  addMeasure(
    position = "topleft",
    primaryLengthUnit = "meters",
    secondaryLengthUnit = "kilometers",
    primaryAreaUnit = "sqmeters",
    completedColor = "red",
    activeColor = "red"
    
  ) |> 
  addScaleBar(
    position = "bottomright",
    options = scaleBarOptions(imperial = FALSE)
  ) |> 
  addMiniMap(
    position = "topright",
    tiles = providers$Esri.WorldStreetMap,
    toggleDisplay = TRUE,
    minimized = FALSE
  ) |> 
  addResetMapButton() |> 
  addProviderTiles(
    "OpenStreetMap",
    # give the layer a name
    group = "OpenStreetMap"
  ) |> 
  addProviderTiles(
    "Stamen.Toner",
    group = "Stamen.Toner"
  ) %>%
  addProviderTiles(
    "Stamen.Terrain",
    group = "Stamen.Terrain"
  ) %>%
  addProviderTiles(
    "Esri.WorldStreetMap",
    group = "Esri.WorldStreetMap"
  ) %>%
  addProviderTiles(
    "Wikimedia",
    group = "Wikimedia"
  ) %>%
  addProviderTiles(
    "CartoDB.Positron",
    group = "CartoDB.Positron"
  ) %>%
  addProviderTiles(
    "Esri.WorldImagery",
    group = "Esri.WorldImagery"
  ) |> 
  # addLayersControl(baseGroups = c("Agrupamento","Todos","Casas", "Apartamentos", "Terrenos"), 
  #                  options = layersControlOptions(collapsed = FALSE)) |> 
  # add a layers control
  addLayersControl(
    baseGroups = c(
      "OpenStreetMap", "Stamen.Toner",
      "Stamen.Terrain", "Esri.WorldStreetMap",
      "Wikimedia", "CartoDB.Positron", "Esri.WorldImagery"
    ),
    overlayGroups = c("Agrupamento","Todos","Casas", "Apartamentos", "Terrenos"),
    # position it on the topleft
    position = "topleft"
  ) 

saveWidget(mapa, file = "/home/prdm0/Downloads/index.html")

