#' @title Recupera informações de votações a partir de um id de votação
#' @description A partir do id de uma votação,
#' recupera seus dados disponíveis
#' @param id_votacao ID da votação
#' @return Info sobre uma votação
#' @example 
#' votacao_2190237_117 <- fetch_votacao("2190237-117")
#' @export
fetch_votacao <- function(id_votacao) {
  library(tidyverse)
  
  url_votacoes <- "https://dadosabertos.camara.leg.br/api/v2/votacoes/%s"
  
  url <- url_votacoes %>% 
    sprintf(id_votacao)
  
  votacao <- tryCatch({
    data <- (RCurl::getURI(url) %>% jsonlite::fromJSON())$dados
    
    votacao <-
      tribble(
        ~ id_votacao,
        ~ data,
        ~ sigla_orgao,
        ~ obj_votacao,
        ~ resumo,
        data$id,
        data$dataHoraRegistro,
        data$siglaOrgao,
        data$descUltimaAberturaVotacao,
        data$descricao
      )
    return(votacao)
    
  }, error = function(e) {
    data <- tribble(~ id_votacao, ~ data, ~sigla_orgao, ~ obj_votacao, ~resumo)
    return(data)
  })
  
  return(votacao)
}