library(tidyverse)

cnae_classificacao <- read_csv(here::here("data-raw/cnae_classificacao.csv"))
usethis::use_data(cnae_classificacao, overwrite = TRUE)

cnae_descricao <- read_csv(here::here("data-raw/cnae_descricao.csv"), 
                           col_types = cols(Subclasse = "c"))
usethis::use_data(cnae_descricao, overwrite = TRUE)

grupos_cnaes <- 
  jsonlite::fromJSON(here::here("data-raw/constants.json"))$grupos_cnae
usethis::use_data(grupos_cnaes, overwrite = TRUE)

parlamentares <- read_csv(here::here("data-raw/parlamentares.csv"), 
                           col_types = cols(id_entidade = "c",
                                            id_entidade_parlametria = "c"))
usethis::use_data(parlamentares, overwrite = TRUE)

