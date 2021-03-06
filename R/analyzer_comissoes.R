#' @title Padroniza nomenclatura do cargo de um parlamentar numa Comissão
#' @description Padroniza nomenclatura do cargo de um parlamentar numa Comissão
#' @param cargo Cargo para padronização
#' @return String com Nome Padronizado
#' @examples
#' .padroniza_cargo_comissao("Titular")
.padroniza_cargo_comissao <- function(cargo) {
  library(tidyverse)
  
  cargo_padronizado = dplyr::case_when(tolower(cargo) == tolower(.PRESIDENTE) ~ "Presidente",
                                       tolower(cargo) == tolower(.VICE_PRESIDENTE) ~ "Vice-presidente",
                                       tolower(cargo) == tolower(.PRIMEIRO_VICE_PRESIDENTE) ~ "Primeiro Vice-presidente",
                                       tolower(cargo) == tolower(.SEGUNDO_VICE_PRESIDENTE) ~ "Segundo Vice-presidente",
                                       tolower(cargo) == tolower(.TERCEIRO_VICE_PRESIDENTE) ~ "Terceiro Vice-presidente",
                                       startsWith(tolower(cargo), .TITULAR) ~ "Titular",
                                       startsWith(tolower(cargo), .SUPLENTE) ~ "Suplente")
  
  return(cargo_padronizado)
}

#' @title Atribui um peso ao cargo do parlamentar na Comissão
#' @description Classifica cargos da Comissão de acordo com o nível do cargo
#' @param cargo Cargo para padronização
#' @param situacao Situação do parlamentar na Comissão
#' @return Valor atribuído ao cargo
#' @examples
#' .enumera_cargo_comissao("Titular")
.enumera_cargo_comissao <- function(cargo, situacao) {
  library(tidyverse) 

  peso = dplyr::case_when(tolower(cargo) == tolower(.PRESIDENTE) ~ 7,
                          tolower(cargo) == tolower(.VICE_PRESIDENTE) ~ 6,
                          tolower(cargo) == tolower(.PRIMEIRO_VICE_PRESIDENTE) ~ 6,
                          tolower(cargo) == tolower(.SEGUNDO_VICE_PRESIDENTE) ~ 5,
                          tolower(cargo) == tolower(.TERCEIRO_VICE_PRESIDENTE) ~ 4,
                          startsWith(tolower(cargo), .TITULAR) ~ 3,
                          startsWith(tolower(cargo), .SUPLENTE) ~ 2,
                          is.na(tolower(cargo)) & situacao == .TITULAR ~ 1,
                          TRUE ~ 0)
  
  return(peso)
}

#' @title Recupera informações da Comissão
#' @description Utiliza o rcongresso para recuperar informações sobre uma Comissão específica
#' @param sigla Sigla da Comissão
#' @return Dataframe com informações da Comissão
#' @examples
#' fetch_comissao_info("CCJC", "camara")
#' @export
fetch_comissao_info <- function(sigla, casa) {
  
  if (tolower(casa) == "camara") {
    return(fetch_comissao_info_camara(sigla))
  } else if (tolower(casa) == "senado") {
    return(fetch_comissao_info_senado(sigla))
  } else {
    stop("Argumento 'casa' inválido.")
  }
}

#' @title Recupera informações das Comissões e de suas composições
#' @description Retorna dados de Comissões da Câmara dos Deputados e também suas composições
#' @return Lista com dois Dataframes: comissões e composição das comissões
#' @examples
#' processa_comissoes()
#' @export
processa_comissoes_e_composicoes <- function(casa = NULL) {
  library(tidyverse)
  library(here)
  
  if (is.null(casa)) {
    comissao_composicao_camara <- fetch_comissoes_composicao_camara()
    
    comissao_composicao_senado <- fetch_comissoes_composicao_senado()
    
    comissao_composicao <- comissao_composicao_camara %>% 
      rbind(comissao_composicao_senado)
    
  } else if (tolower(casa) == "camara") {
    comissao_composicao <- fetch_comissoes_composicao_camara()
    
  } else if (tolower(casa) == "senado") {
    comissao_composicao <- fetch_comissoes_composicao_senado()
    
  } else {
    stop("Argumento 'casa' inválido.")
  }
  
  lista_comissao <- comissao_composicao %>% 
    dplyr::distinct(casa, sigla) %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(dados = purrr::map2(sigla, 
                                      casa,
                                      fetch_comissao_info)) %>% 
    tidyr::unnest(dados) %>% 
    dplyr::mutate(nome_comissao = stringr::str_to_title(nome_comissao)) %>% 
    dplyr::filter(!str_detect(toupper(nome_comissao), 'ESPECIAL'))
  
  ## Composição das Comissões
  composicao_comissoes <- comissao_composicao %>% 
    dplyr::left_join(lista_comissao, by = c("sigla", "casa")) %>% 
    dplyr::filter(!is.na(comissao_id)) %>% 
    dplyr::mutate(peso_cargo = .enumera_cargo_comissao(tolower(cargo), tolower(situacao))) %>% 
    dplyr::mutate(cargo = .padroniza_cargo_comissao(tolower(cargo))) %>% 
    
    dplyr::group_by(comissao_id, id) %>% 
    dplyr::mutate(maximo = max(peso_cargo)) %>%
    dplyr::filter(maximo == peso_cargo) %>%
    ungroup() %>% 
    dplyr::select(comissao_id, casa, id_parlamentar = id, cargo, situacao) %>% 
    dplyr::filter(!is.na(comissao_id))

  ## Informações das Comissões
  comissoes <- lista_comissao %>%
    dplyr::select(id = comissao_id, casa, sigla, nome = nome_comissao)
  
  return(list(comissoes, composicao_comissoes))
}