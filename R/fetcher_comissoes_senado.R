#' @title Recupera informações de composição das comissões usando o LeggoR
#' @description Utiliza o LeggoR para recuperar informações sobre a composição dos membros das composições do Congresso
#' @return Dataframe com parlamentares membros da comissão e seus respectivos cargos
#' @examples
#' fetch_comissoes_composicao_senado()
#' @export
fetch_comissoes_composicao_senado <- function() {
  library(tidyverse)
  library(agoradigital)
  # devtools::install_github('analytics-ufcg/leggoR', force = T)
  
  comissoes <- agoradigital::fetch_all_composicao_comissao() %>% 
    dplyr::filter(casa == "senado") %>% 
    dplyr::filter(nome != "", nome != "VAGO", id != "") %>%
    dplyr::mutate(cargo = dplyr::if_else(is.na(cargo), 
                                         situacao,
                                         cargo)) %>% 
    dplyr::mutate(cargo = dplyr::if_else(cargo == "RELATOR",
                                         situacao,
                                         cargo)) %>% # Checagem adicional que irá sair quando o erro da API (Senado) for corrigido 
    dplyr::select(id, nome, cargo, situacao, sigla, casa) %>% 
    dplyr::distinct()
  
  return(comissoes)
}

#' @title Recupera informações da Comissão no Senado Federal
#' @description Utiliza o rcongresso para recuperar informações sobre uma Comissão específica no Senado
#' @param sigla Sigla da Comissão
#' @param orgaos_senado Dataframe com o mapeamento sigla para id
#' @return Dataframe com informações da Comissão
#' @examples
#' fetch_comissao_info_senado("CAE", orgaos_senado)
#' @export
fetch_comissao_info_senado <- function(sigla_arg, orgaos_senado) {
  library(tidyverse)
  
  comissao <- tryCatch({
    comissao_id <- orgaos_senado %>% 
      filter(sigla == sigla_arg) %>% 
      head(1) %>% 
      pull(id)
    
    url <- paste0("https://legis.senado.leg.br/dadosabertos/composicao/comissao/", comissao_id)
    
    xml <- RCurl::getURL(url) %>% xml2::read_xml()
    data <- xml2::xml_find_all(xml, ".//ComposicaoComissao/IdentificacaoComissao") %>% 
      map_df(function(x) {
        list(
          comissao_id = xml2::xml_find_first(x, ".//CodigoComissao") %>% 
            xml2::xml_text(),
          sigla = xml2::xml_find_first(x, ".//SiglaComissao") %>% 
            xml2::xml_text(),
          nome_comissao = xml2::xml_find_first(x, ".//NomeComissao") %>% 
            xml2::xml_text()
        )
      }) %>% 
      dplyr::mutate(comissao_id = as.numeric(comissao_id)) %>% 
      dplyr::select(comissao_id, nome_comissao)
  }, error = function(e) {
    data <- tribble(
      ~ comissao_id, ~ nome_comissao)
    return(data)
  })
  
  return(comissao)
}