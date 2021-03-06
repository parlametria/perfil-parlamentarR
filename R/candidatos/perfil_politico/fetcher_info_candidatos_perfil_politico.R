#' @title Recupera IDs dos candidatos (Depudato Federal e Senador) da API do perfil político (https://api-perfilpolitico.serenata.ai)
#' @description A partir da API do perfil político recupera IDs dos candidatos da plataforma.
#' @param ano Ano da eleição para captura dos dados
#' @return Dataframe com IDs (na api do perfil político) de todos os candidatos a deputado federal e senador nas eleições de 2018 
#' @examples
#' ids_candidatos_2018 <- fetch_ids_perfil_politico_camara_senado(2018)
fetch_ids_perfil_politico_camara_senado <- function(ano = 2018) {
  library(tidyverse)
  
  ufs <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", 
           "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", 
           "SC", "SE", "SP", "TO")
  
  candidatos_camara <- purrr::pmap_dfr(list(ufs), 
                                       ~ fetch_ids_perfil_politico_por_uf_cargo(ano, ..1, "deputado-federal"))
  
  candidatos_senado <- purrr::pmap_dfr(list(ufs), 
                                       ~ fetch_ids_perfil_politico_por_uf_cargo(ano, ..1, "senador"))
  
  candidatos <- candidatos_camara %>% 
    rbind(candidatos_senado)
  
  return(candidatos)
}

#' @title Recupera IDs dos candidatos da API do perfil político (https://api-perfilpolitico.serenata.ai)
#' @description A partir da API do perfil político recupera IDs dos candidatos dada uma UF e um Cargo
#' @param ano Ano da eleição para captura dos dados
#' @param uf UF para captura dos dados
#' @param cargo Cargo dos candidatos. Ex: deputado-federal, senador.
#' @return Dataframe com IDs (na api do perfil político) de todos os candidatos a deputado federal e senador nas eleições de 2018 
#' @examples
#' ids_candidatos_camara_pb_2018 <- fetch_ids_perfil_politico_por_uf_cargo(ano = 2018, uf = "pb", cargo = "deputado-federal")
fetch_ids_perfil_politico_por_uf_cargo <- function(ano = 2018, uf = "pb", cargo = "deputado-federal") {
  library(tidyverse)
  library(jsonlite)
  
  print(paste0("Recuperando dados de candidatos a ", cargo, "(", ano, " - ", uf, ")"))
  
  url <- paste0("https://api-perfilpolitico.serenata.ai/api/candidate/", 
                ano, "/", 
                uf, "/", 
                cargo, "/")
  
  candidatos <- fromJSON(url) %>% 
    as.data.frame()
  
  candidatos_alt <- candidatos %>% 
    select(id = objects.id, nome = objects.name, partido = objects.party, uf = objects.state,
           cargo = objects.post)
    
  return(candidatos_alt)
}

#' @title Recupera informações de candidatos da API do perfil político via ID
#' @description A partir da API do perfil político recupera informações de um candidato
#' @param id Id do candidato na API perfil político
#' @return Dataframe com informações específicas de um candidato
#' @examples
#' candidato <- fetch_info_perfil_politico_por_id("2314405")
fetch_info_perfil_politico_por_id <- function(id) {
  library(tidyverse)
  library(jsonlite)
  
  print(paste0("Recuperando informações do candidato de id ", id))
  Sys.sleep(3) ## API não suporta muitas requisições acumuladas. Remova se a carga de requisições for pequena.
  
  url <- paste0("https://api-perfilpolitico.serenata.ai/api/candidate/", id)
  
  candidato <- fromJSON(url)
  
  candidato_alt <- tibble(id = candidato$id, 
                          nome = candidato$name,
                          uf = candidato$state,
                          partido = candidato$party_abbreviation,
                          data_nascimento = candidato$date_of_birth,
                          cargo = candidato$post)

  return(candidato_alt)  
}

#' @title Processa dados da api do perfil político para recuperar informações detalhadas dos candidatos à Câmara e ao
#' Senado
#' @description A partir da API do perfil político recupera informações dos candidatos à Câmara e ao Senado
#' @return Dataframe com IDs (na api do perfil político) de todos os candidatos a deputado federal e senador nas eleições de 2018 
#' @examples
#' candidatos <- process_info_perfil_politico()
process_info_perfil_politico <- function() {
  library(tidyverse)
  
  candidatos_senadores_2014 <- fetch_ids_perfil_politico_camara_senado(2014) %>% 
    filter(cargo == "SENADOR")
  
  candidatos_2018 <- fetch_ids_perfil_politico_camara_senado(2018)
  
  candidatos_ids <- candidatos_2018 %>% 
    rbind(candidatos_senadores_2014) %>% 
    distinct(id) %>% 
    pull(id)
  
  candidatos_info <- purrr::pmap_dfr(list(candidatos_ids), 
                                          ~ fetch_info_perfil_politico_por_id(..1))
    
  return(candidatos_info)
}

#' @title Recupera dados da api do perfil político para recuperar informações detalhadas dos candidatos à Câmara e ao
#' Senado nas eleições de 2018 e de 2014 (para senadores)
#' @description A partir da API do perfil político recupera informações dos candidatos à Câmara e ao Senado
#' @return Dataframe com IDs (na api do perfil político) de todos os candidatos a deputado federal e senador nas eleições de 2018 e candidatos 
#' a senadores em 2014.
#' @examples
#' candidatos <- fetch_ids_perfil_politico()
fetch_ids_perfil_politico <- function() {
  library(tidyverse)
  
  candidatos_senadores_2014 <- fetch_ids_perfil_politico_camara_senado(2014) %>% 
    filter(cargo == "SENADOR")
  
  candidatos_2018 <- fetch_ids_perfil_politico_camara_senado(2018)
  
  candidatos <- candidatos_2018 %>% 
    rbind(candidatos_senadores_2014) %>% 
    distinct(id, .keep_all = TRUE)
  
  return(candidatos)
}
