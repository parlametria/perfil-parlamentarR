#' @title Baixa um PDF a partir de uma url e um caminho de destino
#' @description A partir de uma url e de o caminho de destino + nome para o pdf, baixa e salva este arquivo
#' @param url URL da requisição
#' @param dest_path Caminho + nome do arquivo PDF que será baixado.
.download_pdf <- function(url, dest_path = "votacao_senado.pdf") {
  download.file(url, dest_path, mode="wb")
}

#' @title Raspa os dados de votos de um pdf
#' @description A partir do caminho do pdf, raspa as informações referentes aos votos dos senadores
#' @param url URL da requisição
#' @param dest_path Caminho + nome do arquivo PDF que será baixado.
#' @return Dataframe com informações de votos dos senadores
.scrap_votos_from_pdf_senado <- function(pdf_filepath) {
  library(tidyverse)
  
  pdf <- pdftools::pdf_text(pdf_filepath)
  
  votos <- purrr::map_df(pdf, function(x) {
    content <-
      str_extract(x, "(.|\n)+?(?=(\\s Legenda|PRESENTES))") %>%
      str_extract("SENADO.*UF.*VOTO(.|\n)*") %>%
      str_split("\n")
    
    data <- content[[1]] %>%
      str_split("\\s{2,}") %>%
      as.data.frame() %>%
      t() %>%
      as.data.frame()
    
    rownames(data) <- NULL
    
    if(ncol(data) > 1) {
      colnames(data) <- c('senador', 'uf', 'partido', 'voto')
      return(data %>% 
               slice(2:nrow(data)))
      
    } else{
      
      return(tribble(~ senador, ~ uf, ~ partido, ~ voto))
    }
   
  })
  
    return(votos)
} 

#' @title Deleta um arquivo
#' @description A partir do caminho de um arquivo, deleta-o do computador.
#' @param filepath Caminho do arquivo a ser removido.
.delete_file <- function(filepath) {
  file.remove(filepath)
}

#' @title Extrai informações de votos dos senadores a partir dos ids de proposicao e de votacao
#' @description A partir de uma url base, extrai os dados de votos dos senadores por meia de seus ids
#' @param id_proposicao id da proposicao requerida
#' @param id_votacao id da votacao requerida
#' @return Dataframe com informações de votos dos senadores
#' @export
fetch_votos_por_proposicao_votacao_senado <- function(id_proposicao, id_votacao) {
  library(RCurl)
  library(rvest)
  library(xml2)
 
  url_default <- 'https://rl.senado.gov.br/reports/rwservlet?legis&report=/forms/parlam/vono_r01.RDF&paramform=no&p_cod_materia_i=%s&p_cod_materia_f=%s&p_cod_sessao_votacao_i=%s&p_cod_sessao_votacao_f=%s&p_order_by=nom_parlamentar'
  
  url <- url_default %>% sprintf(id_proposicao, id_proposicao, id_votacao, id_votacao)
  
  print(paste0("Extraindo informações de votação de id ", id_votacao))
  
  pdf_filepath <- "votacao_senado.pdf"
  
  .download_pdf(url, pdf_filepath)
  
  votos <- .scrap_votos_from_pdf_senado(pdf_filepath)
  
  .delete_file(pdf_filepath)
  
  
  return(votos)
} 

#' @title Extrai informações de votos dos senadores a partir de um conjutno de votações
#' @description A partir de um dataframe de votações, extrai os dados de votos dos senadores.
#' @param votacoes_senado_filepath Caminho para o csv das votações
#' @return Dataframe com informações de votos dos senadores
#' @examples 
#' votos <- fetch_votos_por_proposicao_senado(136635)
#' @export
fetch_votos_por_proposicao_senado <- function(id_proposicao) {
  library(tidyverse)
  
  votacoes <- 
    fetcher_votacoes_por_proposicao_senado(id_proposicao)
  
  votos <- 
    tibble::tibble(
      id_proposicao = votacoes$id_proposicao,
      id_votacao = votacoes$codigo_sessao,
      data = votacoes$datetime,
      casa = "senado") %>% 
    mutate(dados = purrr::map2(id_proposicao, id_votacao, fetch_votos_por_proposicao_votacao_senado)) %>% 
    unnest(dados) %>% 
    filter(senador != '')
  
  return(votos)
  
}
