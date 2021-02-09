#' @title Importa e processa dados de votações na Câmara dos Deputados
#' @description Recebe informações da votação específica para obtenção dos votos
#' @param id_votacao Id da votação no contexto interno do Voz Ativa
#' @return Dataframe contendo id da votação, id e voto e partido dos deputados que participaram de cada votação
#' @examples id_votacao = "2265603-43"
#' votacoes <- fetch_votos_por_votacao_camara(2165578, 8334)
fetch_votos_por_votacao_camara <- function(id_votacao) {
  library(tidyverse)
  library(RCurl)
  library(stringr)
  library(jsonlite)
  source("workspace/perfil-parlamentarR/R/fetcher_votacoes_camara.R")
  
  url <-  stringr::str_interp("https://dadosabertos.camara.leg.br/api/v2/votacoes/${id_votacao}/votos")
  
  print(paste0("Baixando votos da votação ", id_votacao))
      
  votos <- (RCurl::getURL(url) %>% 
                  jsonlite::fromJSON(flatten = TRUE))$dados
      
  votos <- votos %>% mutate(id_votacao = id_votacao)
      
  votacoes <- fetch_votacao(id_votacao)
      
  votos_alt <- inner_join(votacoes, votos, by = c("id_votacao")) %>% 
      select(id_votacao,
              id_proposicao, 
              data, 
              resumo, 
              tipoVoto,
              deputado_.id, 
              deputado_.siglaPartido) %>% 
      rename(voto = tipoVoto, 
             id_deputado = deputado_.id,
             partido = deputado_.siglaPartido)
      
      return(votos_alt)
}


#' @title Recupera informações de votos de todas as votações de uma determinada proposição para um determinado ano
#' @description A partir do id da proposição e do ano recupera votos que aconteceram na Câmara dos Deputados
#' @param id_proposicao ID da proposição
#' @param ano Ano para o período de votações
#' @return Votos dos parlametares para a proposição (inclui várias votações)
#' @examples
#' votos <- fetch_votos_por_ano_camara(2190355, 2019)
fetch_votos_por_ano_camara <- function(id_votacao, ano = 2019) {
  library(tidyverse)
  library(dplyr)
  library(lubridate)
  
  votacoes_filtradas <- fetch_votos_por_votacao_camara(id_votacao)
  
  votacoes_filtradas$data <-  ymd_hms(votacoes_filtradas$data)
  votacoes_filtradas$data_ano <- year(votacoes_filtradas$data) 
  
  votacoes_filtradas <- filter(votacoes_filtradas, data_ano == ano) %>% select(-data_ano)
  
  votacoes_filtradas <- votacoes_filtradas %>%
    distinct(id_votacao, .keep=T)
  
  votos_raw <- tibble(id_votacao = votacoes_filtradas$id_votacao) %>%
    rowwise(.) %>% 
    mutate(dados = map(
      id_votacao,
      fetch_votos_por_votacao_camara)) %>%
    select (-id_votacao) %>% 
    unnest(dados)  %>%
    ungroup()
  
  votos <- votos_raw %>%
    mutate(partido = padroniza_sigla(deputado_.siglaPartido)) %>%
    enumera_voto() %>%
    select(id_votacao, deputado_.id, tipoVoto, deputado_.siglaPartido) %>%
    distinct()  
  
  return(votos)
}
}