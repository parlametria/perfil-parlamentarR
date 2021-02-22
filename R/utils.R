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

#' @title Extrai o texto de um nó a partir do xpath
#' @description Recebe um nó XML e um xpath e retorna o texto do conteúdo
#' @param node Nó do XML
#' @param xpath Xpath onde o texto está
#' @return Texto extraído
extract_text_from_node <- function(node, xpath) {
  library(tidyverse)
  return(
    xml2::xml_find_first(node, xpath) %>%
      xml2::xml_text()
  )
}

#' @title Padroniza siglas de partidos
#' @description Recebe uma sigla de partido como input e retorna seu valor padronizado
#' @param sigla Sigla do partido
#' @return Dataframe com sigla do partido padronizada
#' @export
padroniza_sigla <- function(sigla) {
  library(tidyverse)
  
  sigla = toupper(sigla)
  
  sigla_padronizada <- case_when(
    str_detect(tolower(sigla), "ptdob") ~ "AVANTE",
    str_detect(tolower(sigla), "pcdob") ~ "PCdoB",
    str_detect(tolower(sigla), "ptn") ~ "PODEMOS",
    str_detect(tolower(sigla), "pps") ~ "CIDADANIA",
    str_detect(tolower(sigla), "pmdb") ~ "MDB",
    str_detect(tolower(sigla), "patri") ~ "PATRIOTA",
    str_detect(tolower(sigla), "pc do b") ~ "PCdoB",
    tolower(sigla) == "pr" ~ "PL",
    str_detect(sigla, "SOLID.*") ~ "SOLIDARIEDADE",
    str_detect(sigla, "PODE.*") ~ "PODEMOS",
    str_detect(sigla, "GOV.") ~ "GOVERNO",
    str_detect(sigla, "PHS.*") ~ "PHS",
    TRUE ~ sigla
  ) %>%
    stringr::str_replace("REPR.", "") %>% 
    stringr::str_replace_all("[[:punct:]]", "") %>% 
    trimws(which = c("both"))
  
  return(sigla_padronizada)
}

#' @title Recupera o título de uma proposição a partir de seu id
#' @description Recebe o id de uma proposição na câmara e retorna seu título
#' @param id_proposicao Id da proposição
#' @return Título da proposicao (ex: MPV 867/2018)
#' @examples
#' get_sigla_by_id_camara(2190237) // "MPV 867/2018"
#' @export
get_sigla_by_id_camara <- function(id_proposicao) {
  library(tidyverse)
  library(RCurl)
  library(xml2)
  
  url <- paste0("https://www.camara.leg.br/SitCamaraWS/Proposicoes.asmx/ObterProposicaoPorID?IdProp=", id_proposicao)
  
  xml <- getURL(url) %>%
    read_xml()
  
  atributos <- xml_attrs(xml, "id") %>% 
    as.list() %>% 
    data.frame(stringsAsFactors = F) %>% 
    mutate(tipo = trimws(tipo, which = c("both"))) %>% 
    select(siglaTipo = tipo, numero, ano)
  
  return(atributos)
}

#' @title Retorna um NA caso o campo seja uma lista nula
#' @description Recebe um valor e retorna NA se for uma lista nula ou
#' o próprio valor, caso contrário.
#' @param valor valor a ser checado
#' @export
replace_empty_list_for_na <- function(valor) {
  if (is.list(valor)) {
    return(NA)
  }
  
  return(valor)
}

#' @title Enumera votações
#' @description Recebe um dataframe com coluna voto e enumera o valor para um número
#' @param df Dataframe com a coluna voto
#' @return Dataframe com coluna voto enumerada
#' @examples
#' enumera_votacoes(df)
#' @export
enumera_voto <- function(df) {
  df %>%
    mutate(
      voto = case_when(
        str_detect(voto, "Não") ~ -1,
        str_detect(voto, "Sim") ~ 1,
        str_detect(voto, "Obstrução|P-OD") ~ 2,
        str_detect(voto, "Abstenção") ~ 3,
        str_detect(voto, "Art. 17|art. 51 RISF|Artigo 17") ~ 4,
        str_detect(voto, "Liberado") ~ 5,
        #TODO: Tratar caso P-NRV: Presente mas não registrou foto
        TRUE ~ 0
      )
    )
}


#' @title Mapeia um nome eleitoral para id correspondente
#' @description Recebe dois dataframes contendo nome eleitoral e um deles com informação de id
#' @param senadores_df Dataframe com as informações de nome_eleitoral e id
#' @param target_df Dataframe a receber o id do parlamentar
#' @return Dataframe target_df contendo coluna id
#' @export
mapeia_nome_eleitoral_to_id_senado <- function(senadores_df, target_df) {
  library(tidyverse)
  
  result <- 
    target_df %>% 
    left_join(
      senadores_df %>%
        select(nome_eleitoral, id), 
      by=c("nome_eleitoral"))
  
  return(result)
}