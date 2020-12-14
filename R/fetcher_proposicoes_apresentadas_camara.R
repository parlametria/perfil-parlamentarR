#' @title Proposições de um tema que foram apresentadas na Câmara
#' @description Recupera lista de proposições de um tema que foram apresentadas a partir de uma determinada data 
#' na Câmara dos deputados.
#' @param data_inicio Data de início para apresentação das proposições (formato yyyy-mm-dd)
#' @param data_final Data de fim para apresentação das proposições (formato yyyy-mm-dd)
#' @param tema Código do tema das proposições. Lista disponível em: https://dadosabertos.camara.leg.br/api/v2/referencias/proposicoes/codTema
#' @return Dataframe com informações das proposições
#' @examples
#' fetch_proposicoes_apresentadas_camara(data_inicio = "2019-02-01", data_final = "2019-12-31", tema = "48")
#' @export
fetch_proposicoes_apresentadas_camara <- function(data_inicio = "2019-02-01", data_final = "2019-12-31",
                                                  tema = "48") {
  
  proposicoes <- fetch_proposicoes_tramitadas_camara(data_inicio, data_final, tema, 
                                                     apenas_apresentada = TRUE)
  
  return(proposicoes)
}