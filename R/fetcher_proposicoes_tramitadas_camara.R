#' @title Proposições de um tema que tramitaram na Câmara
#' @description Recupera lista de proposições de um tema que tramitaram a partir de uma determinada data 
#' na Câmara dos deputados.
#' @param data_inicio Data de início para ocorrência de eventos na tramitação das proposições (formato yyyy-mm-dd)
#' @param data_final Data de fim para ocorrência de eventos na tramitação das proposições (formato yyyy-mm-dd)
#' @param tema Código do tema das proposições. Lista disponível em: https://dadosabertos.camara.leg.br/api/v2/referencias/proposicoes/codTema
#' @param apenas_apresentada TRUE se o retorno deve considerar as proposições apresentadas no período passado como parâmetro.
#' @return Dataframe com informações das proposições
#' @examples
#' fetch_proposicoes_tramitadas_camara(data_inicio = "2019-02-01", data_final = "2019-12-31", tema = "48")
#' @export
fetch_proposicoes_tramitadas_camara <- function(data_inicio = "2019-02-01", data_final = "2019-12-31",
                                                tema = "48", 
                                                apenas_apresentada = FALSE) {
  library(tidyverse)
  library(RCurl)
  library(jsonlite)
  
  param_data_inicio <- "dataInicio"
  param_data_fim <- "dataFim"
  
  if (apenas_apresentada) {
    param_data_inicio <- "dataApresentacaoInicio"
    param_data_fim <- "dataApresentacaoFim"
  }
  
  url_api <- "https://dadosabertos.camara.leg.br/api/v2/proposicoes?%s=%s&%s=%s&codTema=%s&ordem=ASC&ordenarPor=id&pagina=1&itens=100"
  
  url <- url_api %>% 
    sprintf(param_data_inicio, data_inicio, param_data_fim, data_final, tema)
  
  links <- (getURL(url) %>% jsonlite::fromJSON())$links
  
  ultima_pagina <- links %>% 
    filter(rel == "last") %>% 
    pull(href) %>% 
    stringr::str_match("pagina=(.*?)&") %>% 
    tibble::as_tibble(.name_repair = c("universal")) %>% 
    pull(`...2`)
  
  proposicoes <- tibble(pagina = 1:as.numeric(ultima_pagina)) %>%
    mutate(data = map(
      pagina,
      fetch_proposicoes_tramitadas_por_pagina,
      data_inicio,
      data_final,
      tema,
      as.numeric(ultima_pagina),
      apenas_apresentada
    )) %>% 
    unnest(data) %>% 
    mutate(tema = tema)
  
  return(proposicoes)
}

#' @title Proposições de um tema que tramitaram na Câmara
#' @description Recupera lista de proposições de um tema que tramitaram a partir de uma determinada data 
#' na Câmara dos deputados para uma página da API da Câmara
#' @param data_inicio Data de início para ocorrência de eventos na tramitação das proposições (formato yyyy-mm-dd)
#' @param data_final Data de fim para ocorrência de eventos na tramitação das proposições (formato yyyy-mm-dd)
#' @param tema Código do tema das proposições. Lista disponível em: https://dadosabertos.camara.leg.br/api/v2/referencias/proposicoes/codTema
#' @param ultima_pagina Número da última página 
#' @param apenas_apresentada TRUE se o retorno deve considerar as proposições apresentadas no período passado como parâmetro.
#' @return Dataframe com informações das proposições
#' @examples
#' fetch_proposicoes_tramitadas(data_inicio = "2019-02-01", data_final = "2019-12-31", tema = "48")
fetch_proposicoes_tramitadas_por_pagina <- function(pagina = 1, 
                                                     data_inicio = "2019-02-01", 
                                                     data_final = "2019-12-31",
                                                     tema = "48",
                                                     ultima_pagina = 287,
                                                     apenas_apresentada = FALSE) {
  library(tidyverse)
  library(RCurl)
  library(jsonlite)
  
  param_data_inicio <- "dataInicio"
  param_data_fim <- "dataFim"
  
  if (apenas_apresentada) {
    param_data_inicio <- "dataApresentacaoInicio"
    param_data_fim <- "dataApresentacaoFim"
  }
  
  print(paste0("Fazendo download das proposições da página ", pagina, "/", ultima_pagina))
  url_api <- "https://dadosabertos.camara.leg.br/api/v2/proposicoes?%s=%s&%s=%s&codTema=%s&ordem=ASC&ordenarPor=id&pagina=%s&itens=100"
  
  url <- url_api %>% 
    sprintf(param_data_inicio, data_inicio, param_data_fim, data_final, tema, pagina)
  
  data <- (getURL(url) %>% jsonlite::fromJSON())$dados %>% 
    select(-uri)
  
  return(data)
}