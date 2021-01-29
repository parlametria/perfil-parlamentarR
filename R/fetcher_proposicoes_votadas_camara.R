#' @title Lista de proposições votadas em um determinado ano
#' @description Lista as proposições votadas em plenário para um determinado ano
#' @param ano Ano de ocorrência das votações
#' @return Dataframe contendo id da proposição, nome e data da votação
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

#' @title Recupera xml com votações de uma proposição específica
#' @description A partir do id da prosição recupera o xml com as votações em plenário da proposição
#' @param id_proposicao ID da proposição
#' @return XML com as votações da proposição
#' @examples
#' votacoes_mpv8712019 <- fetch_xml_api_votacao_camara(2190355)
fetch_xml_api_votacao_camara <- function(id_proposicao) {
  library(tidyverse)
  library(RCurl)
  library(xml2)
  
  proposicao <- get_sigla_by_id_camara(id_proposicao) %>%
    select(siglaTipo, numero, ano)
  
  url <- paste0("https://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterVotacaoProposicao?tipo=",
                proposicao$siglaTipo, "&numero=", proposicao$numero, "&ano=", proposicao$ano)
  
  print(paste0("Baixando votação da ", proposicao$siglaTipo, " ", proposicao$numero, "/", proposicao$ano))
  
  xml <- getURL(url) %>%
    read_xml()
  
  return(xml)
}

#' @title Recupera informações de votações a partir de um xml
#' @description A partir do xml das votações recupera dados de todas as votações disponíveis
#' @param id_proposicao ID da proposição
#' @param xml xml com votações. Se Null então o xml é carregado
#' @return Info sobre as votações
#' @examples
#' votacoes_mpv8712019 <- fetch_votacoes_por_proposicao_camara(2190355, xml)
#' @export
fetch_votacoes_por_proposicao_camara <- function(id_proposicao, xml = NULL) {
  library(tidyverse)
  library(xml2)
  
  tryCatch({
    if (is.null(xml)) {
      xml <- fetch_xml_api_votacao_camara(id_proposicao)
    }
    
    votacoes <- xml_find_all(xml, ".//Votacao") %>%
      map_df(function(x) {
        list(
          obj_votacao = xml_attr(x, "ObjVotacao"),
          resumo = xml_attr(x, "Resumo"),
          cod_sessao = xml_attr(x, "codSessao"),
          hora = xml_attr(x, "Hora"),
          data = as.Date(xml_attr(x, "Data"), "%d/%m/%Y")
        )
      })
  }, error = function(e) {
    data <- tribble(~ obj_votacao, ~ resumo, ~ cod_sessao, ~ hora, ~ data)
    return(data)
  })

  return(votacoes)
}