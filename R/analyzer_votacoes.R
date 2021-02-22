library(tidyverse)

#' @title Processa todas as votações da Câmara para um conjunto de anos
#' @description A partir de uma lista de anos recupera as votações da câmara
#' @param anos Lista de anos para recuperação das votações
#' @return Dataframe votações
#' @examples
#' processa_votacoes_camara_anos(anos = c(2020))
#' @export
processa_votacoes_camara_anos <- function(anos = c(2019, 2020)) {
  library(lubridate)
  proposicoes_votadas <-
    pmap_df(
      list(anos = anos),
      ~ perfilparlamentar::fetch_proposicoes_votadas_camara(..1)
    ) %>% 
    ungroup() %>% 
    mutate(ano = lubridate::year(data_votacao)) %>% 
    select(id_proposicao = id, ano) %>% 
    distinct(id_proposicao, ano)
  
  votacoes <- 
    pmap_df(
      list(proposicoes_votadas$id_proposicao,
           proposicoes_votadas$ano),
      function(x, y) {
        votacoes <- fetch_votacoes_por_proposicao_camara(x)
        
        votacoes <- tryCatch({
          data <- votacoes %>% 
            mutate(ano = year(ymd(strsplit(data, "T")[[1]][1]))) %>% 
            filter(ano == y) %>% 
            select(-ano)
          return(data)
        }, error = function(e) {
          print(paste0("Não foi possível filtrar votações que ocorreram em ", y, ". Retornando todas da proposição."))
          return(votacoes)
        })
      }
    )
  
  return(votacoes)
}

#' @title Processa todas as votações do Senado para um conjunto de anos
#' @description A partir de uma lista de anos recupera as votações do Senado
#' @param anos Lista de anos para recuperação das votações
#' @return Dataframe votações
#' @examples
#' processa_votacoes_senado_anos(anos = c(2020))
#' @export
processa_votacoes_senado_anos <- function(anos = c(2019, 2020)) {
  library(lubridate)
  
  data_inicial <- paste0("01/01/", anos[1])
  data_final <- paste0("31/12/", anos[-1])
  
  proposicoes_votadas <-
    perfilparlamentar::fetch_proposicoes_votadas_senado(data_inicial, data_final) %>% 
    filter(votacao_secreta == 0) 
  
  if (nrow(proposicoes_votadas) == 0) {
    return(tibble(id_proposicao = character(),
                  objeto_votacao = character(),
                  datetime = character(),
                  codigo_sessao = character()))
  }
  
  proposicoes_votadas <- proposicoes_votadas %>% 
    mutate(ano = year(ymd(datetime))) %>% 
    distinct(id_proposicao, ano)
  
  votacoes <- 
    pmap_df(
      list(proposicoes_votadas$id_proposicao,
           proposicoes_votadas$ano),
      ~ fetcher_votacoes_por_proposicao_senado(..1, ..2)
    )
  
  return(votacoes)
}
