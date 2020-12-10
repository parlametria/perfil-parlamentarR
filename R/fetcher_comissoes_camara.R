#' @title Recupera informações de composição das comissões usando o LeggoR a partir de uma data de inicio
#' @description Utiliza o LeggoR para recuperar informações sobre a composição dos membros das composições do Congresso a partir de uma data
#' @param data_inicio Data inicial (formato AAAA-MM-DD)
#' @return Dataframe com parlamentares membros da comissão e seus respectivos cargos
#' @examples
#' fetch_comissoes_composicao_camara()
#' @export
fetch_comissoes_composicao_camara <- function(data_inicio = '2019-12-22') {
  library(tidyverse)
  library(agoradigital)
  # devtools::install_github('analytics-ufcg/leggoR', force = T)
  
  orgaos <- agoradigital::fetch_orgaos_camara() %>% 
    filter(tipoOrgao == 'Comissão Permanente') %>% 
    select(orgao_id, sigla)
  
  comissoes <- purrr::map2_df(orgaos$orgao_id, 
                              orgaos$sigla,
                              ~ .fetch_membros_comissao_camara_with_backoff(.x, .y, data_inicio)) %>% 
    dplyr::select(id, nome, cargo, situacao, sigla, casa) %>% 
    dplyr::distinct()
  
  return(comissoes)
}

#' @title Recupera informações da Comissão na Câmara dos Deputados
#' @description Utiliza o rcongresso para recuperar informações sobre uma Comissão específica na câmara dos deputados
#' @param sigla_comissao Sigla da Comissão
#' @return Dataframe com informações da Comissão
#' @examples
#' fetch_comissao_info_camara("CCJC")
#' @export
fetch_comissao_info_camara <- function(sigla_comissao) {
  library(tidyverse)
  library(agoradigital)
  
  comissao_info <- agoradigital::fetch_orgaos_camara() %>% 
    dplyr::filter(sigla == sigla_comissao) %>% 
    dplyr::select(comissao_id = orgao_id, nome_comissao = descricao)
  
  return(comissao_info)
}

#' @title Recupera informações dos membros de uma Comissão em uma data de inicio específica
#' @description A partir de um id, retorna os membros daquela comissão a partir da data de início especificada
#' @param orgao_id Id da Comissão
#' @param sigla_comissao Sigla da Comissão
#' @param data_inicio Data inicial de interesse (formato AAAA-MM-DD)
#' @param max_tentativas Número máximo de tentativas
#' @return Dataframe com informações dos membros da Comissão para uma data de início
.fetch_membros_comissao_camara_with_backoff <- function(orgao_id, sigla_comissao, data_inicio = '2019-12-22', max_tentativas = 10) {
  library(tidyverse)
  
  print(paste0('Baixando informações dos membros da comissão ', sigla_comissao, ' na casa camara'))
  url <- paste0('https://dadosabertos.camara.leg.br/api/v2/orgaos/',
                orgao_id, 
                '/membros?dataInicio=', 
                data_inicio, 
                '&itens=100')
  
  links <- (RCurl::getURL(url) %>% jsonlite::fromJSON())$links
  
  last_page <- links %>% 
    filter(rel == "last") %>% 
    pull(href) %>% 
    str_match("pagina=(.*?)&") %>% 
    tibble::as_tibble(.name_repair = c("universal")) %>% 
    pull(`...2`)
  
  membros <- tibble(page = 1:as.numeric(last_page)) %>%
    mutate(data = map(
      page,
      .fetch_membros_comissao_camara_by_page,
      url,
      sigla_comissao,
      max_tentativas
    )) %>% 
    unnest(data)
  
  return(membros)
}

#' @title Recupera informações dos membros de uma Comissão em uma data de inicio específica
#' @description A partir de um id, retorna os membros daquela comissão a partir da data de início especificada
#' @param page Página a ser requisitada
#' @param url Url da requisição
#' @param max_tentativas Número máximo de tentativas
#' @return Dataframe com informações dos membros da Comissão para uma data de início
.fetch_membros_comissao_camara_by_page <- function(page = 1,  url, sigla_comissao, max_tentativas = 10) {
  library(tidyverse)
  
  url_paginada <- paste0(url, '&pagina=', page)
  
  for (tentativa in seq_len(max_tentativas)) {
    
    membros <- tryCatch(
      {
        membros <-
          (RCurl::getURL(url_paginada) %>% 
             jsonlite::fromJSON())$dados %>% 
          as_tibble()
        
        if (nrow(membros) == 0) {
          return(tibble::tribble(~ cargo, ~ id, ~ nome, ~ partido, ~ uf, ~ situacao))
        } else {
          
          membros <- membros %>% 
            dplyr::select(cargo = titulo, id, nome, partido = siglaPartido, uf = siglaUf) %>%
            dplyr::mutate(sigla = sigla_comissao, 
                          casa = "camara",
                          cargo = dplyr::case_when(
                            startsWith(cargo, "Presidente") ~ "PRESIDENTE",
                            startsWith(cargo, "Titular") ~ "TITULAR",
                            startsWith(cargo, "1º Vice-Presidente") ~ "PRIMEIRO VICE-PRESIDENTE",
                            startsWith(cargo, "Suplente") ~ "SUPLENTE",
                            startsWith(cargo, "2º Vice-Presidente") ~ "SEGUNDO VICE-PRESIDENTE",
                            startsWith(cargo, "3º Vice-Presidente") ~ "TERCEIRO VICE-PRESIDENTE"
                          ),
                          situacao = if_else(cargo == 'SUPLENTE', 'Suplente', 'Titular'))
          membros <- membros[!duplicated(membros$id) | membros$cargo %in% 
                               c("PRESIDENTE", "VICE-PRESIDENTE", "SEGUNDO VICE-PRESIDENTE", "TERCEIRO VICE-PRESIDENTE"),,drop=FALSE]
        }
        return(membros)
      }, error = function(e) {
        print(e)
        return(tibble::tribble(~ cargo, ~ id, ~ nome, ~ partido, ~ uf, ~ situacao))
      }
    )
    
    if (nrow(membros) == 0) {
      backoff <- runif(n = 1, min = 0, max = 2 ^ tentativa - 1)
      message("Backing off for ", backoff, " seconds.")
      Sys.sleep(backoff)
    } else {
      break
    }
  }
  return(membros)
}