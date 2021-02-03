#' @title Lista de proposições votadas em um determinado ano
#' @description Lista as proposições votadas em plenário para um determinado ano
#' @param ano Ano de ocorrência das votações
#' @return Dataframe contendo id da proposição e data da última votação
#' @examples
#' proposicoes_votadas_em_2019 <- fetch_proposicoes_votadas_camara(2019)
#' @export
fetch_proposicoes_votadas_camara <- function(ano = 2019) {
  library(tidyverse)
  library(RCurl)
  library(xml2)
  library(jsonlite)
  
  url_votacoes <- "https://dadosabertos.camara.leg.br/arquivos/votacoes/csv/votacoes-%s.csv"
  
  url <- url_votacoes %>% 
    sprintf(ano)
  
  proposicoes <- tryCatch({
    
   data <- read_delim(url, delim = ";") %>% 
     filter(siglaOrgao == 'PLEN') %>% 
     rowwise(.) %>% 
     mutate(proposicao_id = stringr::str_split(id, "-")[[1]][1]) %>% 
     ungroup()
   
   proposicoes <- data %>% 
     group_by(proposicao_id) %>% 
     mutate(lastest_data = max(data)) %>% 
     select(id = proposicao_id, data_votacao = lastest_data) %>% 
     distinct()
   
   return(proposicoes)
   
   # proposicoes_sigla <-
   #   purrr::map_df(proposicoes$proposicao_id, ~rcongresso::fetch_proposicao_camara(.x)) %>% 
   #   mutate(sigla = stringr::str_glue("{siglaTipo} {numero}/{ano}"),
   #          id = as.character(id)) %>% 
   #   select(id, sigla)
   # 
   # proposicoes_votadas <- data %>% 
   #   select(proposicao_id, data) %>% 
   #   inner_join(proposicoes_sigla, by = c("proposicao_id"="id"))
  
  }, error = function(e) {
    message(e)
    data <- tribble(
      ~ id, ~ data_votacao)
    return(data)
  })
  
  return(proposicoes)
}

#' @title Recupera informações de votações em plenário a partir de um id de proposição
#' @description A partir do id de uma proposição,
#' recupera dados de todas as votações disponíveis
#' @param id_proposicao ID da proposição
#' @return Info sobre as votações
#' @examples
#' votacoes_2190237 <- fetch_votacoes_por_proposicao_camara(2190237)
#' @export
fetch_votacoes_por_proposicao_camara <- function(id_proposicao) {
  library(tidyverse)
  
  url_votacoes <- "https://dadosabertos.camara.leg.br/api/v2/proposicoes/%s/votacoes"
  
  url <- url_votacoes %>% 
    sprintf(id_proposicao)
  
  votacoes <- tryCatch({
    data <- (RCurl::getURI(url) %>% jsonlite::fromJSON())$dados
    
    data <- data %>% 
      filter(siglaOrgao == 'PLEN') %>% 
      distinct(id) %>% 
      mutate(dados = map(id,
                         fetch_votacao)) %>% 
      unnest(dados) %>% 
      select(-id) %>% 
      mutate(id_proposicao = id_proposicao)
    
  }, error = function(e) {
    data <- tribble(~ id_votacao, ~ data, ~sigla_orgao, ~ obj_votacao, ~resumo, ~id_proposicaao)
    return(data)
  })

  return(votacoes)
}
