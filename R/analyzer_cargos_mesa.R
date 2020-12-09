#' @title Captura informações de deputados e senadores atualmente 
#' integrantes da Mesa na Câmara de Deputados e no Senado Federal
#' @description Com base na API da Câmara e do Senado,
#'  captura informações dos integrantes da Mesa Diretora em cada casa.
#' @return Dataframe contendo deputados senadores com cargos na Mesa Diretora da
#' Câmara e Senado.
#' @examples
#' processa_cargos_mesa()
#' @export
processa_cargos_mesa <- function(casa = NULL) {
  
  if (is.null(casa)) {
    cargos_camara <- fetch_cargos_mesa_camara()
    cargos_senado <- fetch_cargos_mesa_senado()
    
    cargos <- cargos_camara %>% 
      rbind(cargos_senado)
    
    return(cargos)
  } else if (tolower(casa) == 'camara') {
    return(fetch_cargos_mesa_camara())
  } else if(tolower(casa) == "senado") {
    return(fetch_cargos_mesa_senado())
  } else {
    stop("Argumento 'casa' inválido.")
  }
}
