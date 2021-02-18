library(tidyverse)

#' @title Importa e processa dados das orientações das bancadas na Câmara dos Deputados
#' @description Recebe informações da votação específica para obtenção dos votos
#' @param id_votacao Id da votação
#' @return Dataframe contendo id da votação, id e voto dos deputados que participaram de cada votação
#' @examples
#' votacoes <- fetch_orientacoes_votacao_camara("2265603-43")
#' @export
fetch_orientacoes_votacao_camara <-function(id_votacao) {
    source(here("R/votacoes/utils_votacoes.R"))
    
    url <-
      paste0(
        "https://dadosabertos.camara.leg.br/api/v2/votacoes/",
        id_votacao,
        "/orientacoes"
      )
    
    print(
      paste0(
        "Baixando orientações da votação ",
        id_votacao,
        "..."
      )
    )

    tryCatch({
      data <- (RCurl::getURL(url) %>% 
                 jsonlite::fromJSON())$dados
      
      orientacoes <- data %>% 
        mutate(id_votacao = id_votacao) %>% 
        select(id_votacao,
               orientacao = orientacaoVoto,
               tipo_lideranca = codTipoLideranca,
               partido_bloco = siglaPartidoBloco)
      
      return(orientacoes)
    }, error = function(e) {
      return(tibble(id_votacao = character(),
                     orientacao = character(),
                     tipo_lideranca = character(),
                     partido_bloco = character()))
    })    
   
  }

#' @title Importa e processa dados das orientações das bancadas na Câmara dos Deputados para uma proposição
#' @description A partir do ID da proposição recupera votações que ocorreram no ano passado como parâmetro
#' @param id_proposicao Id da prposição para obtenção das orientações
#' @return Dataframe contendo id da votação, id e voto dos deputados que participaram de cada votação
#' @examples
#' orientacoes <- fetch_orientacoes_por_proposicao_camara(2265603)
#' @export
fetch_orientacoes_proposicao_camara <- function(id_proposicao) {
  
  print(paste0("Baixando orientações das votações da proposição ", id_proposicao, "..."))
  
  votacoes <- 
    perfilparlamentar::fetch_votacoes_por_proposicao_camara(id_proposicao) %>% 
    distinct(id_votacao)
  
  orientacoes <- purrr::map_df(votacoes$id_votacao,
                               ~ fetch_orientacoes_votacao_camara(.x))
    
  return(orientacoes)
}
