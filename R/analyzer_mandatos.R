#' @title Extrai informações sobre o mandato de um parlamentar
#' @description Recebe o id e a casa de um parlamentar e retorna um dataframe contendo informações do mandato, como legislatura,
#' data de inicio, data de fim, situacao, código e descrição da causa do fim do exercício.
#' @param id_parlamentar id do parlamentar
#' @param casa casa do parlamentar
#' @return Dataframe contendo informações de legislatura,
#' data de inicio, data de fim, situacao, código e descrição da causa do fim do exercício.
#' @examples
#' processa_mandatos_parlamentar(141428, "camara")
#' @export
processa_mandatos_parlamentar <- function(id_parlamentar, casa) {
  print(paste0("Baixando informações de mandatos do parlamentar de id ", id_parlamentar, " na casa ", casa, "..."))
  if (tolower(casa) == 'camara') {
    source(here::here("R/fetcher_mandatos_camara.R"))
    return(fetch_mandatos_camara(id_parlamentar))
  } else if(tolower(casa) == "senado") {
    source(here::here("R/fetcher_mandatos_senado.R"))
    return(fetch_mandatos_senado(id_parlamentar))
  } else {
    stop("Argumento 'casa' inválido.")
  }
}

#' @title Extrai informações sobre os mandato de todos os parlamentares
#' @description Recebeum dataframe contendo id e casa dos parlamentares e retorna um dataframe contendo informações do mandato, como legislatura,
#' data de inicio, data de fim, situacao, código e descrição da causa do fim do exercício.
#' @param df_parlamentares dataframe com informações de id e casa dos parlamentares
#' @return Dataframe contendo informações de legislatura,
#' data de inicio, data de fim, situacao, código e descrição da causa do fim do exercício.
#' @examples
#' processa_mandatos_parlamentares(perfilparlamentar::fetch_deputados_legislatura(56))
#' @export
processa_mandatos_parlamentares <- function(df_parlamentares = NULL) {
  library(tidyverse)
  
  if (is.null(df_parlamentares)) {
    stop("É necessário passar um dataframe com as colunas 'id' e 'casa'.")
  }
  
  mandatos <-
    purrr::map2_df(df_parlamentares$id, df_parlamentares$casa, 
                   ~ extract_mandatos(.x, .y)) %>% 
    distinct()
  
  return(mandatos)
}