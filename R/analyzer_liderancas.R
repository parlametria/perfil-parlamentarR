#' @title Processa dados de lideranças da Câmara e do Senado
#' @description Executa funções que recuperam dados de liderança na Câmara e no Senado.
#' @return Dataframe contendo informações das lideranças
#' @examples
#' liderancas <- processa_liderancas()
#' @export
processa_liderancas <- function(casa = NULL) {
  library(tidyverse)
  library(here)
  
  source(here("R/fetcher_liderancas_camara.R"))
  source(here("R/fetcher_liderancas_senado.R"))
  
  if (is.null(casa)) {
    liderancas_camara <- .fetch_liderancas_camara()
    liderancas_senado <- .fetch_liderancas_senado()
    
    liderancas <- liderancas_camara %>%
      rbind(liderancas_senado)
    
    return(liderancas)
  } else if (tolower(casa) == "camara") {
    return(.fetch_liderancas_camara())
    
  } else if (tolower(casa) == "senado") {
    return(.fetch_liderancas_senado())
    
  } else {
    stop("Argumento 'casa' inválido.")
  }
  
  
}