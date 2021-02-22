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
processa_votos_senado <- function(votos, senadores_df) {
  
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

#' @title Processa todos os votos da Câmara para um conjunto de anos
#' @description A partir de uma lista de anos recupera os votos da câmara
#' @param anos Lista de anos para recuperação dos votos
#' @return Dataframe votos dos parlamentares
#' @examples
#' processa_votos_camara_anos(anos = c(2020))
#' @export
processa_votos_camara_anos <- function(anos = c(2019, 2020)) {
  message("Essa captura pode demorar bastante!")
  proposicoes_votadas <-
    pmap_df(
      list(anos = anos),
      ~ perfilparlamentar::fetch_proposicoes_votadas_camara(..1)
    ) %>% 
    ungroup() %>% 
    mutate(ano = lubridate::year(data_votacao)) %>% 
    select(id_proposicao = id, ano) %>% 
    distinct(id_proposicao, ano)
  
  votos <- 
    pmap_df(
      list(proposicoes_votadas$id_proposicao,
           proposicoes_votadas$ano),
      ~ perfilparlamentar::fetch_votos_por_ano_camara(..1, ..2)
    )
  
  votos_alt <- votos %>% 
    mutate(id_proposicao = gsub("-.*$", "", id_votacao)) %>% 
    select(id_proposicao, id_votacao, id_parlamentar, partido, voto)
  
  return(votos_alt)
}

#' @title Processa todos os votos do Senado para um conjunto de anos
#' @description A partir de uma lista de anos recupera os votos do Senado
#' @param anos Lista de anos para recuperação dos votos
#' @return Dataframe votos dos parlamentares
#' @examples
#' processa_votos_senado_anos(anos = c(2020))
#' @export
processa_votos_senado_anos <- function(anos = c(2019, 2020)) {
  
  data_inicial <- paste0("01/01/", anos[1])
  data_final <- paste0("31/12/", anos[-1])
  
  proposicoes_votadas <-
    perfilparlamentar::fetch_proposicoes_votadas_senado(data_inicial, data_final) %>% 
    filter(votacao_secreta == 0) %>% 
    distinct(id_proposicao)
  
  votos <- 
    pmap_df(
      list(proposicoes_votadas$id_proposicao),
      ~ perfilparlamentar::fetch_votos_por_proposicao_senado(..1)
    )
  
  senadores <- parlamentares %>% 
    filter(casa == "senado") %>% 
    select(id_parlamentar = id_entidade, nome_eleitoral = nome, uf) %>% 
    distinct(id_parlamentar, nome_eleitoral, uf) %>% 
    mutate(nome_eleitoral = padroniza_texto(nome_eleitoral) %>% trimws(which = "both"))
  
  votos_alt <- votos %>% 
    mutate(data = as.Date(data)) %>% 
    mutate(ano = lubridate::year(data)) %>% 
    filter(ano >= anos[1], ano <= anos[-1]) %>% 
    mutate(senador = padroniza_texto(senador) %>% trimws(which = "both")) %>% 
    left_join(senadores,
              by = c("senador" = "nome_eleitoral", "uf" = "uf")) %>% 
    select(id_proposicao, id_votacao, id_parlamentar, partido, voto)
  
  return(votos_alt)
}
