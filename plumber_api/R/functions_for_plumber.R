# Leitura do binario data.RData
load(fs::path(fs::path_wd(), "data/updated_data/data.RData"))

# Base de dados sem tratamento --------------------------------------------
# Nomes dos bairros de JP: conforme cadastro do zap imoveis ---------------
bairros_jp <- function(df)
  df$bairro |> unique()

# Dados brutos conforme cadastros no zap imoveis -----------
filtrando_por_bairro <- function(df, bairro)
  df |> dplyr::filter(bairro == {{bairro}})

# Todos os imoveis com area, endereco e iptus cadastrados --------
filtrando_iptu <- function(df)
  df |> dplyr::filter(!is.na(area), !is.na(iptu), !is.na(endereco))