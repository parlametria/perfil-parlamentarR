#' @title Padroniza os nomes, retirando acentos, cedilhas e colcoando todas as letras em uppercase
#' @description Recebe um nome e o padroniza no formato: sem acentos, cedilhas, letras maiúsculas
#' @param nome Nome a ser padronizado
#' @return Nome padronizado
#' @examples
#' padroniza_nome("çíço do álcórdéón")
padroniza_nome <- function(nome) {
  library(tidyverse)
  
  return(nome %>% 
           iconv(to="ASCII//TRANSLIT") %>% 
           toupper() %>% 
           trimws(which = c("both")))
}

#' @title Padroniza texto, retirando links, menções, pontuações, retirando acentos, números,
#' cedilhas e colocando todas as letras em lowercase
#' @description Recebe um texto e o padroniza no formato: sem acentos, cedilhas, letras maiúsculas, links, números e menções
#' @param texto Texto a ser padronizado
#' @return Texto padronizado
#' @examples
#' padroniza_texto("çíço do álcórdéón")
padroniza_texto <- function(texto) {
  library(tidyverse)
  texto <- 
    padroniza_nome(texto)
  
  texto <-  
    gsub('HTTP\\S+\\s*|@([A-Z|0-9|_])*|[[:punct:]]|[0-9]*|R$',
         "", 
         texto) %>% 
    tolower()
  
  
  return(texto)
} 

#' @title Recebe uma URL para um pdf e retorna o texto raspado do conteúdo
#' @description Recebe uma URL para um pdf e retorna o texto raspado do seu conteúdo
#' @param url URL contendo o pdf
#' @return Texto do conteúdo do pdf
#' @examples
#' extract_text_from_pdf_url("https://www.camara.leg.br/proposicoesWeb/prop_mostrarintegra?codteor=1709372")
extract_text_from_pdf_url <- function(url) {
  library(pdftools)
  
  print(paste0("Baixando o conteúdo do texto do pdf da url ", url))
  
  content <- tryCatch({
    content <- pdf_text(url)
    
    if(length(content) > 1) {
      content <- paste(content, collapse = '')
    }
    
    content <- gsub('\n', '', content)
  }, error = function(e) {
    print(e)
    return('')
  })
    
  return(content)
} 

#' @title Checa se cargos da mesa se encaixam na lista adotada como padrão
#' @description avalia se cargos presentes como coluna em um dataframe se encaixam na lista adotada como padrão.
#' Lança um erro caso seja encontrado um cargo que não se encaixe na lista de cargos possíveis.
#' @param df Dataframe com pelo menos uma coluna chamada cargo
#' @return Dataframe com mesmo conteúdo que o passado como parâmetro.
.check_cargos <- function(df) {
  library(tidyverse)
  library(here)
  
  source(here("R/utils/constants/cargos_mesa.R"))
  
  lista_cargos <- c(.SECRETARIO_1, .SECRETARIO_2, .SECRETARIO_3, .SECRETARIO_4,
                    .VICE_PRESIDENTE_1, .VICE_PRESIDENTE_2, .PRESIDENTE, 
                    .SUPLENTE_SECRETARIO_1, .SUPLENTE_SECRETARIO_2, .SUPLENTE_SECRETARIO_3, .SUPLENTE_SECRETARIO_4,
                    .SUPLENTE_1, .SUPLENTE_2, .SUPLENTE_3, .SUPLENTE_4)
  
  df_check <- df %>% 
    mutate(check = if_else(cargo %in% lista_cargos, TRUE, FALSE))
  
  if(FALSE %in% (df_check %>% pull(check))) {
    stop("Dataframe contém cargo de mesa inválido")
  } else {
    return(df)
  }
}