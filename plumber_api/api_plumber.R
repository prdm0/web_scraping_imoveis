library(plumber)
library(dplyr)
library(fs)

source(file = "R/functions_for_plumber.R")

#* @apiTitle API - Imoveis João Pessoa - PB, Brasil
#* @apiVersion 0.1.0
#* @apiDescription Essa API fornece dados atualizados dos imóveis da cidade de João Pessoa - PB, Brasil. Os dados são provenientes do site <https://www.zapimoveis.com.br>. O ZAP é o maior portal de imóveis, pertencente ao [Grupo OLX](https://pt.wikipedia.org/wiki/OLX). No ano de 2017, o Zap imóveis anuncia a fusão com o seu maior concorrente (Viva Real) e assim nasce o Grupo Zap. Os dados do ZAP são bastante convenientes para a modelagem imobiliária, uma vez que possui uma base de dados considerável e é por meio desses dados que o indicador [Fipe ZAP](https://fipezap.zapimoveis.com.br) é calculado. **Com essa API, você poderá consumir**: a lista de dos bairros, conforme cadastro no ZAP; os dados brutos conforme cadastro dos imoveis no ZAP; filtrar os dados brutos por bairros (conforme a lista dos bairros); dados brutos dos imoveis cujo os IPTUs foram informados. Os dados estão georeferenciado, com base no [**tidygeocoder**](https://jessecambon.github.io/tidygeocoder/), que suporta diversos [Geocoding Services](https://jessecambon.github.io/tidygeocoder/articles/geocoder_services.html), obtendo assim informações mais consistentes, pelo confornto da consulta nesses diversos serviços. Ao todo, são mais de **32 mil imóveis cadastrados** em João Pessoa - PB.

#* Lista dos Bairros - João Pessoa, PB
#* @apiTag bairros Obtenha a lista dos bairros, conforme o cadastro no ZAP Imóveis

#* Base de dados bruta filtrando por bairro
#* @tag bairros Obtenha a lista dos bairros, conforme cadastrado do ZAP Imóveis
#* @get /bairros
\() bairros_jp(df = data)

#* Base de dados (dados brutos) por bairros - João Pessoa, PB
#* @apiTag dados_por_bairros Obtenha os **dados brutos** filtrando por bairro:

#* @tag dados_por_bairros
#* @param bairro
#* @post /dados_por_bairros
\(bairro) filtrando_por_bairro(df = data, bairro = {{bairro}})

# Base de dados (dados brutos) com IPTU informado
#* @apiTag dados_iptu Obtenha os **dados brutos** com iptus:

#* @tag dados_iptu
#* @get /dados_iptu
\() filtrando_iptu(df = data)