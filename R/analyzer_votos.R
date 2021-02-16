library(tidyverse)
#' @title Recupera dados dos votos para um conjunto de votações.
#' @description Recebe um caminho para o dataframe de votações e retorna todos os
#' votos relacionados.
#' @param votos Dataframe com os votos no Senado
#' @param senadores_df Dataframe dos senadores
#' @examples 
#' votos <- fetch_votos_por_proposicao_senado(136635)
#' senadores_df <- read_csv("https://raw.githubusercontent.com/parlametria/relatorio-bianuario-ma/a5d55b0bda60b8a4f6f8317adc9e29148b1ab0a0/data/raw/leggo_data/entidades.csv")
#' senadores_df <- (senadores_df %>% 
#'       filter(casa == 'senado', is_parlamentar == 1) %>% 
#'       select(id_parlamentar = id_entidade, nome_eleitoral = nome))
#' votos_com_id_senador <- process_votos_senado(votos, senadores_df)
#' @return Datafrane com os votos.
#' @export
process_votos_senado <- function(votos, senadores_df) {
  
  votos <- votos %>%
    select(id_proposicao, id_votacao, nome_eleitoral = senador, partido, voto)
  
  votos <- votos %>% 
    mutate(nome_eleitoral = str_remove(nome_eleitoral, "^[:space:]*|[:space:]$"))
  
  senadores_df <- senadores_df %>%
    select(id = id_parlamentar,
           nome_eleitoral)
  
  votos_alt <-
    perfilparlamentar::mapeia_nome_eleitoral_to_id_senado(senadores_df, votos) %>%
    select(id_proposicao, id_votacao, id_parlamentar = id, partido,voto) %>% 
    distinct()
  
  return(votos_alt)
}