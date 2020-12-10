#' @title Captura informações de senadores atualmente integrantes da Mesa no Senado Federal
#' @description Com base na API da Senado captura informações de senadores integrantes da Mesa
#' @return Dataframe contendo senadores com cargos na Mesa do Senado.
#' @examples
#' senadores_mesa <- fetch_cargos_mesa_senado()
fetch_cargos_mesa_senado <- function() {
  library(tidyverse)
  
  url <- "http://legis.senado.leg.br/dadosabertos/dados/MesaSenado.xml"
  
  senadores <- tryCatch({
    xml <- RCurl::getURL(url) %>% xml2::read_xml()
    data <- xml2::xml_find_all(xml, ".//Cargo") %>%
      map_df(function(x) {
        list(
          id = xml2::xml_find_first(x, ".//Http") %>% 
            xml2::xml_text(),
          nome = xml2::xml_find_first(x, ".//NomeParlamentar") %>% 
            xml2::xml_text(),
          bancada = xml2::xml_find_first(x, ".//Bancada") %>% 
            xml2::xml_text(),
          cargo = xml2::xml_find_first(x, ".//Cargo") %>%
            xml2::xml_text()
        )
      }) %>% 
      distinct() %>% 
      mutate(bancada = gsub("[()]", "", bancada)) %>% 
      separate(bancada, into = c("sg_partido", "uf"), sep = "-") %>% 
      filter(!is.na(id)) %>% 
      mutate(data_inicio = NA, data_fim = NA, 
             legislatura = 56, cargo = str_to_title(cargo)) %>% ## Legislatura atual é a 56
      select(id, nome, sg_partido, uf, cargo, data_inicio, data_fim, legislatura) %>% 
      .check_cargos() %>% 
      mutate(casa = "senado")
    
  }, error = function(e) {
    print(e)
    data <- tribble(
      ~ id, ~ nome, ~ sg_partido, ~ uf, ~ cargo,
      ~ data_inicio, ~ data_fim, ~ legislatura, ~ casa)
    return(data)
  })
  
  return(senadores)
}