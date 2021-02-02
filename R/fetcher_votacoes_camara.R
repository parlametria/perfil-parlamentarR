#' @title Recupera informações de votações a partir de um id de votação
#' @description A partir do id de uma votação,
#' recupera seus dados disponíveis
#' @param id_votacao ID da votação
#' @return Info sobre uma votação
#' @examples
#' votacao_2190237_117 <- fetch_votacao("2190237-117")
#' @export
fetch_votacao <- function(id_votacao) {
  library(tidyverse)
  
  print(paste0("Baixando dados da votação ", id_votacao))
  
  url_votacoes <- "https://dadosabertos.camara.leg.br/api/v2/votacoes/%s"
  
  url <- url_votacoes %>% 
    sprintf(id_votacao)
  
  votacao <- tryCatch({
    data <- (RCurl::getURI(url) %>% jsonlite::fromJSON())$dados
    
    votacao <-
      tribble(
        ~ id_votacao,
        ~ id_proposicao,
        ~ data,
        ~ obj_votacao,
        ~ resumo,
        data$id,
        data$proposicoesAfetadas$id,
        data$dataHoraRegistro,
        data$descUltimaAberturaVotacao,
        data$descricao
      )
    
    votacao <- votacao %>% 
      mutate(obj_votacao = if_else(obj_votacao == "NULL", "", as.character(obj_votacao)))
    
    return(votacao)
    
  }, error = function(e) {
    data <- tribble(~ id_votacao, ~id_proposicao, ~ data, ~ obj_votacao, ~resumo)
    return(data)
  })
  
  return(votacao)
}
