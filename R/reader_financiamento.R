library(tidyverse)

#' @title Lê csv de parlamentares doadores
#' @param parlamentares_doadores_path Caminho para o CSV com informação das doações recebidas por parlamentares
#' durante a campanha eleitoral. 
#' O csv de 2018 pode ser obtido aqui: 
#' https://raw.githubusercontent.com/parlametria/perfil-parlamentar-dados/master/parlametria/raw_data/receitas/parlamentares_doadores.csv
#' @return 
#' @examples
#' read_parlamentares_doadores("https://raw.githubusercontent.com/parlametria/perfil-parlamentar-dados/master/parlametria/raw_data/receitas/parlamentares_doadores.csv")
#' @export
read_parlamentares_doadores <- function(parlamentares_doadores_path) {
  readr::read_csv(parlamentares_doadores_path,
           col_types = cols(valor_receita = col_number(),
                            .default = col_character()))
}

#' @title Lê csv de sócios de empresas que doaram para parlamentares
#' @param empresas_doadores_path Dataframe com informação dos sócios de empresas que doaram para
#' parlamentares na campanha eleitoral. 
#' O csv de 2018 pode ser obtido aqui: 
#' https://raw.githubusercontent.com/parlametria/perfil-parlamentar-dados/master/parlametria/raw_data/empresas/empresas_doadores_todos_parlamentares.csv
#' @return Dataframe com doações feitas
#' @examples
#' read_empresas_doadores("https://raw.githubusercontent.com/parlametria/perfil-parlamentar-dados/master/parlametria/raw_data/empresas/empresas_doadores_todos_parlamentares.csv")
#' @export
read_empresas_doadores <- function(empresas_doadores_path) {
  readr::read_csv(empresas_doadores_path,
           col_types = c(valor_doado = col_number(), 
                         .default = col_character()))
}

#' @title Lê csv informações das empresas que tiveram sócios que doaram para parlamentares
#' @param info_empresas_path Dataframe com informação das empresas que tiveram sócios que doaram para
#' parlamentares na campanha eleitoral. 
#' O csv de 2018 pode ser obtido aqui: 
#' https://raw.githubusercontent.com/parlametria/perfil-parlamentar-dados/master/parlametria/raw_data/empresas/info_empresas_doadores_todos_parlamentares.csv
#' @return Dataframe com doações feitas
#' @examples
#' read_info_empresas("https://raw.githubusercontent.com/parlametria/perfil-parlamentar-dados/master/parlametria/raw_data/empresas/info_empresas_doadores_todos_parlamentares.csv")
#' @export
read_info_empresas <- function(info_empresas_path) {
  readr::read_csv(info_empresas_path,
           col_types = cols(capital_social = col_number(), 
                            porte = col_number(),
                            .default = col_character()))
}
