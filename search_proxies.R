library(jsonlite)
library(vroom)
library(pbmcapply)
library(fs)

search_proxies <- function(
    url_test = "https://www.zapimoveis.com.br/venda/imoveis/pb+joao-pessoa",
    save_path = "../API/",
    time_out = 3.5){

  read_list_proxies_txt <- function(string_url){
    df <- vroom::vroom(
      string_url,
      progress = FALSE,
      show_col_types = FALSE,
      col_names = FALSE
    )  
    colnames(df) <- c("ip", "port")
    df
  }
  
  df_proxies_1 <- 
    read_list_proxies_txt(
      "https://raw.githubusercontent.com/monosans/proxy-list/main/proxies/http.txt"
    )
  
  df_proxies_2 <- 
    read_list_proxies_txt(
      "https://raw.githubusercontent.com/mertguvencli/http-proxy-list/main/proxy-list/data.txt"
    )
  
  df_proxies_3 <- 
    read_list_proxies_txt(
      "https://raw.githubusercontent.com/roosterkid/openproxylist/main/HTTPS_RAW.txt"
    )
  
  df_proxies_4 <-
    read_list_proxies_txt(
      "https://raw.githubusercontent.com/sunny9577/proxy-scraper/master/proxies.txt"
    )
  
  df_proxies_5 <- 
    read_list_proxies_txt( 
        "https://raw.githubusercontent.com/mmpx12/proxy-list/master/http.txt"
      )
  
  df_proxies_6 <- 
    read_list_proxies_txt( 
      "https://raw.githubusercontent.com/roosterkid/openproxylist/main/HTTPS_RAW.txt"
    )
  
  df_proxies_7 <- 
    read_list_proxies_txt( 
      "https://raw.githubusercontent.com/TheSpeedX/PROXY-List/master/http.txt"
    )
  
  df_proxies_8 <-
    read_list_proxies_txt(
      "https://raw.githubusercontent.com/ShiftyTR/Proxy-List/master/https.txt"
    )
  
  test <- function(ip, port){
    
    conexao <- 
      httr::GET(
        url_test,
        httr::timeout(time_out),
        httr::use_proxy(
          url = ip,
          port = port
        )
      )
    
    if(conexao$status_code == 200)
      return(TRUE)
    else
      return(FALSE)
  }
  
  try_test <- function(...) 
    tryCatch(
      expr = test(...),
      error = function(e) FALSE
    )
  
  search <- function(df){
    pbmcapply::pbmcmapply(
      df$ip,
      df$port,
      FUN = try_test,
      mc.cores = parallel::detectCores()
    )
  }
  
  list_proxies_1 <- search(df_proxies_1)
  list_proxies_2 <- search(df_proxies_2)
  list_proxies_3 <- search(df_proxies_3)
  list_proxies_4 <- search(df_proxies_4)
  list_proxies_5 <- search(df_proxies_5)
  list_proxies_6 <- search(df_proxies_6)
  list_proxies_7 <- search(df_proxies_7)
  list_proxies_8 <- search(df_proxies_8)
  
  proxies <- dplyr::bind_rows(
    df_proxies_1[unlist(list_proxies_1) == TRUE, ],
    df_proxies_2[unlist(list_proxies_2) == TRUE, ],
    df_proxies_3[unlist(list_proxies_3) == TRUE, ],
    df_proxies_4[unlist(list_proxies_4) == TRUE, ],
    df_proxies_5[unlist(list_proxies_5) == TRUE, ],
    df_proxies_6[unlist(list_proxies_6) == TRUE, ],
    df_proxies_7[unlist(list_proxies_7) == TRUE, ],
    df_proxies_8[unlist(list_proxies_8) == TRUE, ]
  )
  
  save(
    proxies,
    file = fs::path(save_path, "proxies", ext = "RData")
  )
  proxies
}    

# Função útil para testar se a tibble de proxies precisa ser
# ataulizada.
last_test <- function(df_proxies,
          url_string = "https://www.zapimoveis.com.br/venda/imoveis/pb+joao-pessoa"){
  step <- function(ip, port){
    conexao <- httr::GET(
      url_string,
      httr::use_proxy(
        url = ip,
        port = port
      )
    )
    
    if(conexao$status_code == 200)
      return(TRUE)
    else
      return(FALSE)
  }
  
  try_step <- function(...) 
    tryCatch(
      expr = step(...),
      error = function(e) FALSE
    )
  
  pbmcapply::pbmcmapply(
    df_proxies$ip,
    df_proxies$port,
    FUN = try_step,
    mc.cores = parallel::detectCores()
  )
}

df_proxies <- search_proxies()

result_test <- last_test(df_proxies = df_proxies)
  