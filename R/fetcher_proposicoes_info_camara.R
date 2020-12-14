#' @title Recupera informações de uma proposição
#' @description A partir do id, recupera dados de uma proposição na Câmara dos Deputados,
#' como nome, data_apresentacao, ementa, autor, indexacao, tema e uri_tramitacao
#' @param id_prop ID de uma proposição
#' @return Dataframe contendo informações de uma proposição
#' @examples
#' proposicao <- fetch_info_proposicao_camara(2193540)
#' @export
fetch_info_proposicao_camara <- function(id_prop) {
  library(tidyverse)
  print(paste0("Baixando informações da proposição ", id_prop))
  
  url <-
    paste0(
      "https://www.camara.leg.br/proposicoesWeb/prop_autores?idProposicao=",
      id_prop
    )
  
  autor <- httr::GET(url, httr::accept_json()) %>%
    httr::content('text', encoding = 'utf-8') %>%
    xml2::read_html()  %>%
    rvest::html_nodes('#content') %>%
    rvest::html_nodes('span') %>%
    rvest::html_text()
  
  temas  <- tryCatch({
    url <-
      paste0("https://dadosabertos.camara.leg.br/api/v2/proposicoes/",
             id_prop,
             "/temas")
    data <- (RCurl::getURI(url) %>%
               jsonlite::fromJSON())$dados %>%
      as.data.frame() %>%
      select(tema)
    
  }, error = function(e) {
    return(dplyr::tribble(~ tema))
  })
  
  proposicao <- rcongresso::fetch_proposicao_camara(id_prop) %>%
    mutate(
      nome = paste0(siglaTipo, " ", numero, "/", ano),
      data_apresentacao = lubridate::ymd_hm(gsub("T", " ", dataApresentacao)) %>%
        format("%d/%m/%Y"),
      id = as.character(id),
      autor = paste(autor[3:length(autor)], collapse = ', ') ,
      tema = paste(unlist(temas$tema), collapse = ', '),
      uri =
        paste0(
          "https://camara.gov.br/proposicoesWeb/fichadetramitacao?idProposicao=",
          id_prop
        )
    ) %>%
    select(
      id,
      nome,
      data_apresentacao,
      ementa,
      autor,
      indexacao = keywords,
      tema,
      uri_tramitacao = uri
    )
  
  return(proposicao)
}