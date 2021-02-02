library(tidyverse)
library(here)
library(jsonlite)

#' @title Processa doações por setor
#' @description Processa doações feitas por setores econômicos para parlamentares em 2018.
#' @param info_empresas Dataframe com informação das empresas que tiveram sócios que doaram para
#' parlamentares na campanha eleitoral.
#' @param empresas_doadores Dataframe com informação dos sócios de empresas que doaram para
#' parlamentares na campanha eleitoral.
#' @return Dataframe com doações feitas
#' @examples
#' processa_socios_empresas_setores()
processa_socios_empresas_setores <- function(
  info_empresas,
  empresas_doadores
) {
  lista_cnaes <- cnae_descricao %>%
    filter(!is.na(Subclasse)) %>% 
    select(cnae = Subclasse, cnae_descricao = `Descrição`) %>% 
    mutate(cnae = str_replace_all(cnae, "[[:punct:]]", ""))
  
  lista_cnaes_classificacao <- lista_cnaes %>% 
    left_join(cnae_classificacao, 
              by = "cnae_descricao")
  
  empresas_info <- info_empresas %>% 
    mutate(capital_social = as.numeric(capital_social),
           porte = as.numeric(porte)) %>% 
    select(cnpj, razao_social, cnae_codigo) %>% 
    mutate(cnae_codigo = str_pad(cnae_codigo, 7, side = "left", pad = "0")) %>% 
    mutate(classe_cnae = substr(cnae_codigo, 1, 2)) %>% 
    distinct() %>% 
    left_join(lista_cnaes_classificacao, by = c("cnae_codigo" = "cnae")) %>% 
    mutate(cnae_descricao = if_else(is.na(cnae_descricao), "Atividade não informada", cnae_descricao))

  grupos_cnaes <- grupos_cnaes %>% 
    as.data.frame() %>% 
    unnest(cols = c(codigos)) %>% 
    select(codigos, descricao_atividade = nome, grupo_geral)
  
  empresas_filtro <- empresas_info %>% 
    left_join(grupos_cnaes, by = c("classe_cnae" = "codigos"))
  
  socios_doadores <- empresas_doadores %>% 
    mutate(cnpj_empresa = str_pad(cnpj_empresa, 14, side = "left", pad = "0")) %>% 
    select(cnpj_empresa, cpf_cnpj_socio, nome_socio) %>% 
    distinct()
  
  socios_doadores_filtro <- socios_doadores %>% 
    inner_join(empresas_filtro, by = c("cnpj_empresa" = "cnpj")) %>% 
    select(cpf_cnpj_socio, nome_socio, cnpj_empresa, razao_social, cnae_codigo, cnae_descricao,
           tipo, descricao_atividade, grupo_geral)
  
  return(socios_doadores_filtro)
}

#' @title Processa doações por sócios de empresas
#' @description Processa doações feitas por sócios de empresas
#' @param parlamentares_doadores Dataframe com informação das doações recebidas por parlamentares
#' durante a campanha eleitoral.
#' @param info_empresas Dataframe com informação das empresas que tiveram sócios que doaram para
#' parlamentares na campanha eleitoral.
#' @param empresas_doadores Dataframe com informação dos sócios de empresas que doaram para
#' parlamentares na campanha eleitoral.
#' @return Dataframe com doações de sócios de empresas
#' Cada linha do dataframe contém um sócio de empresa (cpf_cnpj_socio) e um parlamentar que recebeu a doação
#' @examples
#' processa_doacoes_socios_setores()
processa_doacoes_socios_setores <- function(
  parlamentares_doadores,
  info_empresas,
  empresas_doadores) {
  
  cpfs_socios_filtro <- processa_socios_empresas_setores(info_empresas, empresas_doadores) %>% 
    distinct(cpf_cnpj_socio) %>% 
    pull(cpf_cnpj_socio)
  
  parlamentares_doacoes_filtro <- parlamentares_doadores %>% 
    filter(cpf_cnpj_doador %in% cpfs_socios_filtro) %>% 
    select(cpf_cnpj_doador, nome_doador, id_parlamentar = id, casa_parlamentar = casa, 
           cpf_parlamentar = cpf, nome_eleitoral, uf, sg_partido, valor_receita)
  
  return(parlamentares_doacoes_filtro)
}

#' @title Processa dados de financiamento dos parlamentares
#' @description Processa doações feitas por setores econômicos para parlamentares em 2018.
#' ATENÇÃO: Se uma mesma doação foi feita por um sócio de empresa ligada a mais de uma
#' atividade econômica, o valor da doação será dividido por igual para cada atividade.
#' @param parlamentares_doadores Dataframe com informação das doações recebidas por parlamentares
#' durante a campanha eleitoral. O csv de 2018 pode ser obtido aqui: https://github.com/parlametria/perfil-parlamentar-dados/blob/master/parlametria/raw_data/receitas/parlamentares_doadores.csv
#' @param info_empresas Dataframe com informação das empresas que tiveram sócios que doaram para
#' parlamentares na campanha eleitoral. O csv de 2018 pode ser obtido aqui: https://github.com/parlametria/perfil-parlamentar-dados/blob/master/parlametria/raw_data/empresas/info_empresas_doadores_todos_parlamentares.csv
#' @param empresas_doadores Dataframe com informação dos sócios de empresas que doaram para
#' parlamentares na campanha eleitoral. O csv de 2018 pode ser obtido aqui: https://github.com/parlametria/perfil-parlamentar-dados/blob/master/parlametria/raw_data/empresas/empresas_doadores_todos_parlamentares.csv
#' @return Dataframe com doações feitas
#' @examples
#' processa_financiamento_setores()
#' @export
processa_financiamento_por_setores <-
  function(parlamentares_doadores,
           info_empresas,
           empresas_doadores) {

  socios_empresas <- processa_socios_empresas_setores(info_empresas, empresas_doadores)
  
  socios_filtro_n_cnaes <- socios_empresas %>% 
    group_by(cpf_cnpj_socio) %>% 
    summarise(n_cnaes = n_distinct(cnae_descricao)) %>% 
    ungroup()
  
  doacoes_filtro <- processa_doacoes_socios_setores(parlamentares_doadores,
                                                    info_empresas, 
                                                    empresas_doadores)
  
  doacoes_filtro_alt <- doacoes_filtro %>% 
    left_join(socios_filtro_n_cnaes, by = c("cpf_cnpj_doador" = "cpf_cnpj_socio")) %>% 
    mutate(valor_receita_dividido = valor_receita / n_cnaes)
  
  socios_filtro_cnaes <- socios_empresas %>% 
    distinct(cpf_cnpj_socio, cnae_descricao, tipo, descricao_atividade, grupo_geral)
  
  doacoes_filtro_merge <- doacoes_filtro_alt %>% 
    left_join(socios_filtro_cnaes, by = c("cpf_cnpj_doador" = "cpf_cnpj_socio")) 
  
  parlamentares_doacoes_total <- parlamentares_doadores %>% 
    mutate(valor_receita = as.double(valor_receita)) %>% 
    group_by(id, casa) %>% 
    summarise(valor_total_recebido = sum(valor_receita)) %>% 
    ungroup()
  
  doacoes_filtro_final <- doacoes_filtro_merge %>% 
    left_join(parlamentares_doacoes_total, by = c("id_parlamentar" = "id",
                                                  "casa_parlamentar" = "casa")) %>% 
    select(cpf_cnpj_doador, nome_doador, id_parlamentar, casa_parlamentar, cpf_parlamentar,
           nome_eleitoral, uf, sg_partido, cnae_descricao, tipo, descricao_atividade, grupo_geral,
           valor_receita_atividade = valor_receita_dividido,
           valor_receita_campanha = valor_total_recebido)
  
  return(doacoes_filtro_final) 
}
