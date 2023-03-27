library(tidymodels)
library(recipes)
library(dplyr)
library(visdat)
library(DataExplorer)
library(doParallel)
library(h2o)
library(agua)
library(embed)
library(vip)
library(bonsai)

h2o_start()

rm(list = ls(all = TRUE))

# Resolvendo conflitos de pacotes:
tidymodels::tidymodels_prefer()

# speed up computation with parallel processing
registerDoParallel(cores = parallel::detectCores(logical = FALSE)) 

# Lendo dados -------------------------------------------------------------
load("dados/joao_pessoa/dados_atualizados/dados.RData")

# Usando distância interquartil:
remove_outlier <- function(dados, var = NULL){
  
  if(is.null(var)){
    x <- dados$valor/dados$area
  } else{
    x <- dados[var]
  }
  
  q1 <- quantile(unlist(x), 0.01)
  q3 <- quantile(unlist(x), 0.99)
  
  iqr <- q3 - q1
  
  limite_sup <- q3 + 1.5 * iqr
  limite_inf <- q3 - 1.5 * iqr
  
  if(is.null(var)){
    linhas <- which(x >= limite_inf & x <= limite_sup)
  } else {
    linhas <- which(dados[var] >= limite_inf & dados[var] <= limite_sup)
  }
  # if(var == "valor")
  #   dados[linhas, ] |> dplyr::filter(valor >= 10000)
  # else
  #   dados[linhas, ]
  
  dados[linhas, ]
}

# Importando os dados faxinados:
# source("R/faxina_dos_dados.R")
# dados <- faxina()

dados <- remove_outlier(dados, var = NULL)
dados <- remove_outlier(dados, var = "latitude")
dados <- remove_outlier(dados, var = "longitude")

dados <- dados |>
    select(-id, -iptu, -condominio, -endereco, -bairro, -comercio, -url) |> 
    mutate(valor = base::log10(valor))

dados <- dados %>% 
  mutate_all(~replace(., . == TRUE, 1)) %>% 
  mutate_all(~replace(., . == FALSE, 0))

# Explorando os dados:
# dados |> 
#   create_report()
# 
# dados |> visdat::vis_dat()
# 
# dados |> visdat::vis_miss()

set.seed(0)
# Divisão do conjunto de dados --------------------------------------------
dados_split <- initial_split(dados, prop = 0.8, strata = valor)

# Dados de treinamento:
dados_treinamento <- training(dados_split)

# Dados de teste:
dados_teste <- testing(dados_split)

# Receita:
receita <-
  recipe(valor ~ ., data = dados_treinamento) |> 
  recipes::step_log(all_numeric_predictors(), -latitude, -longitude, offset = 1L, base = 10) |> 
  recipes::step_impute_knn(all_numeric_predictors(), impute_with = imp_vars(area, banheiro)) |>
  recipes::step_zv(all_predictors()) |> 
  recipes::step_interact(~ area:quarto:vaga:banheiro) |> 
  recipes::step_normalize(all_numeric_predictors()) |> 
  recipes::step_corr(all_numeric_predictors()) |> 
  #recipes::step_dummy(all_nominal_predictors()) |> 
  recipes::step_ns(latitude, deg_free = 21) |> 
  recipes::step_ns(longitude, deg_free = 21)

# Estimando uma receita ---------------------------------------------------
# treinamento_rec <- prep(transformacoes, training = dados_treinamento)

# Aplicando as transformações e imputações --------------------------------
# treinamento <-  bake(receita, new_data = NULL)
# teste <- bake(receita, new_data = dados_teste)

# Modelo ------------------------------------------------------------------
modelo <- auto_spec <-
  auto_ml() |>
  set_engine("h2o", max_runtime_secs = 5000, seed = 0) |>
  set_mode("regression")

# modelo <- rand_forest(min_n = tune::tune(), mtry = tune::tune()) |>
#     set_engine('ranger', importance = "impurity", num.threads = parallel::detectCores()) |>
#     set_mode('regression')

# modelo <- 
#   boost_tree(
#     mtry = tune(), trees = 1000, tree_depth = tune(), 
#     learn_rate = tune(), min_n = tune(), loss_reduction = tune()
#   ) |> 
#   set_engine("lightgbm") |> 
#   set_mode("regression")

# Workflow
wf <- workflow() |> 
  add_model(modelo) |> 
  add_recipe(receita)

# Validação Cruzada
cv <- vfold_cv(dados_treinamento, v = 20L, strata = valor)

# cv <-
#   recipes::bake(
#     receita,
#     new_data = training(dados_split)
#   ) %>%
#   rsample::vfold_cv(v = 5)

# Treinamento
tunagem <-
  tune_grid(
    wf,
    resamples = cv,
    grid = 50L,
    metrics = metric_set(rsq, rmse),
    control = control_grid(save_pred = TRUE, verbose = TRUE, allow_par = FALSE)
  )

# tunagem <- tune::tune_grid(
#   object = wf,
#   resamples = cv,
#   grid = lgbm_grid,
#   metrics = yardstick::metric_set(rmse, rsq, mae),
#   control = tune::control_grid(verbose = TRUE, save_pred = TRUE) # set this to TRUE to see
#   # in what step of the process you are. But that doesn't look that well in
#   # a blog.
# )

# Observando os resultados

# ggplot2::autoplot(tunagem)

tunagem |> 
  show_best(n = 10L) 
  
# Atualizar o workflow com parametros tunados:
wf <- wf |> finalize_workflow(select_best(tunagem, "rmse"))

# Último ajuste:
ajuste_final <- last_fit(wf, dados_split, metrics = metric_set(rsq, rmse))

# Métricas de desempenho:
collect_metrics(ajuste_final)

# Previsões:
previsoes <- collect_predictions(ajuste_final)

# Modelo final:
modelo_final <- fit(wf, dados)

saveRDS.lgb.Booster(modelo_final, file = "modelo_final_lightgbm.rds")
#save(modelo_final, file = "modelo_final_lightgbm.rda")

# Lendo o modelo

load(file = "modelo_final_lightgbm.rds")

# Prevendo com novos dados:

previsoes_todos_dados <- predict(modelo_final, new_data = dados)

dados_com_previsoes <- bind_cols(dados, previsoes_todos_dados)


# Gráficos:
ajuste = bind_cols(previsoes = previsoes$.pred, valor = previsoes$valor)
r2 = rsq(ajuste, truth = valor, estimate = previsoes)
erro_quadratico_medio = rmse(ajuste, truth = valor, estimate = previsoes)

# Scatterplots:
plot_tudo <- previsoes |>  select(valor, .pred) |> 
   ggplot() + 
   aes(x = valor, y = .pred) +
   geom_point()

# # Checando o modelo com a base de teste:
# previsoes <- workflow() |> 
#   add_recipe(receita_final) |> 
#   add_model(modelo_final) |> 
#   last_fit(dados_split) |> 
#   collect_predictions()
#   
# # Adequação:
# ajuste = bind_cols(previsoes = previsoes$.pred, valor = previsoes$valor)
# r2 = rsq(ajuste, truth = valor, estimate = previsoes)
# erro_quadratico_medio = rmse(ajuste, truth = valor, estimate = previsoes)
# 
# previsoes |>  select(valor, .pred) |> 
#   ggplot() + 
#   aes(x = valor, y = .pred) |> 
#   geom_point()
# 
# 
# # Estimando em uma base toda. Por exemplo,
# # se eu fosse testar na base toda?
# 
# workflow() |> 
#   add_recipe(receita_final) |> 
#   add_model(modelo_final) 


# Previsões sobre todos os dados