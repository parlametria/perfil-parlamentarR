#' @title Calcula os dados de aderência do deputado a um partido ou ao governo
#' @description Retorna dados de aderência dos deputados por votação e de forma sumarizada
#' @param deputados_votos Dataframe de deputados com os votos do deputado e os do partido (pode ser GOVERNO)
#' @param filtrar TRUE se deseja filtrar fora os partidos com menos de 5 membros e os deputados com menos de 10 votações. 
#' FALSE se quiser capturar todos os deputados
#' @return Lista com dataframes com informações dos deputados e seus dados de aderência sumarizados e por votação
#' @examples
#' dados_aderencia <- processa_calculo_aderencia(votos, filtrar = FALSE)
processa_calculo_aderencia <- function(parlamentares_votos, parlamentares, filtrar = TRUE) {
  library(tidyverse)
  
  source(here("crawler/votacoes/aderencia/calcula_aderencia.R"))
  
  if (filtrar) {
    minimo_votacoes_por_deputado <- 10
    minimo_membros_partido <- 5
  } else {
    minimo_votacoes_por_deputado <- 0
    minimo_membros_partido <- 0
  }
  
  parlamentares_votos_match <- parlamentares_votos %>%
    rowwise() %>% 
    mutate(match = compara_voto_com_orientacao(voto_parlamentar, voto_partido)) %>% 
    ungroup()
  
  parlamentares_summary_long <- parlamentares_votos_match %>% 
    group_by(id, partido, match) %>% 
    summarise(n = n()) %>% 
    mutate(match = case_when(
      match == -2 ~ "faltou",
      match == -1 ~ "nao_seguiu",
      match == 0 ~ "nao_calculado",
      match == 1 ~ "seguiu",
      match == 2 ~ "partido_liberou"
    )) %>% 
    ungroup() %>% 
    left_join(parlamentares %>% select(id, nome_eleitoral, uf), by = "id") %>% 
    filter(!is.na(nome_eleitoral)) %>% 
    mutate(nome = if_else(partido == "GOVERNO", nome_eleitoral, 
             paste0(str_to_title(nome_eleitoral), " - ", partido, "/", uf))) %>% 
    select(id, nome, partido, match, n) %>% 
    filter(!is.na(id))
  
  cols <- c(faltou = NA_integer_, partido_liberou = NA_integer_, nao_seguiu = NA_integer_, seguiu = NA_integer_)
  
  parlamentares_summary_freq_wide <- parlamentares_summary_long %>% 
    spread(key = match, value = n) %>% 
    add_column(!!!cols[!names(cols) %in% names(.)]) %>% 
    replace(is.na(.), 0) %>% 
    mutate(total_votacoes = seguiu + nao_seguiu) %>% 
    filter(total_votacoes >= minimo_votacoes_por_deputado) %>% 
    mutate(freq = (seguiu / (seguiu + nao_seguiu)) * 100) %>%
    mutate(freq = if_else(is.nan(freq), -1, freq)) %>% 
    mutate(freq = if_else(total_votacoes >= 3, freq, -1)) %>% 
    arrange(freq) %>% 
    select(id, nome, partido, faltou, partido_liberou, nao_seguiu, seguiu, total_votacoes, freq)
  
  partidos_count <- parlamentares_summary_freq_wide %>% 
    group_by(partido) %>% 
    summarise(n = n()) %>% 
    filter(n >= minimo_membros_partido) %>% 
    pull(partido)
  
  parlamentares_summary_freq_wide <- parlamentares_summary_freq_wide %>% 
    filter(partido %in% partidos_count)
  
  return(list(parlamentares_votos_match, parlamentares_summary_freq_wide))
}

#' @title Processa os dados de aderência do deputado ao partido
#' @description Retorna dados de aderência dos deputados por votação e de forma sumarizada
#' @param votos Dataframe de votos
#' @param orientacao Dataframe de orientações
#' @param parlamentares Dataframe de parlamentares
#' @param filtrar TRUE se deseja filtrar fora os partidos com menos de 5 membros e os parlamentares com menos de 10 votações. 
#' FALSE se quiser capturar todos os parlamentares
#' @return Lista com dataframes com informações dos parlamentares e seus dados de aderência sumarizados e por votação
#' @examples
#' dados_aderencia <- processa_dados_deputado_aderencia(votos, orientacao, parlamentares)
processa_dados_deputado_aderencia <- function(votos, orientacao, parlamentares, filtrar = TRUE) {
  library(tidyverse)
  
  parlamentares_votos <- votos %>% 
    left_join(orientacao, 
              by = c("id_proposicao", "id_votacao", "partido")) %>% 
    rename(voto_parlamentar = voto.x,
           voto_partido = voto.y) %>% 
    mutate(id = as.character(id_parlamentar),
           partido_parlamentar = partido)

  return(processa_calculo_aderencia(parlamentares_votos, parlamentares, filtrar))
}


#' @title Processa dados de orientacao do governo considerando também o voto do líder do Governo
#' @description Retorna as orientações do Governo e usa o voto do líder como segunda opção para determinar orientação
#' @param votos Dataframe de votos
#' @param orientacao Dataframe de orientações
#' @return Dataframe com orientações do Governo para votações em plenário
#' @examples
#' orientacao_governo <- adiciona_hierarquia_orientacao_governo(votos, orientacao)
adiciona_hierarquia_orientacao_governo <- function(votos, orientacao) {
  library(tidyverse)
  library(here)
  
  lider_governo <- read_csv(here("crawler/raw_data/liderancas.csv")) %>% 
    filter(tolower(bloco_partido) == "governo", tolower(cargo) == "líder", casa == "camara") %>% 
    pull(id)
  
  if (!is.na(lider_governo)) {
    voto_lider <- votos %>% 
      filter(id_parlamentar == lider_governo)
    
    orientacao_governo <- orientacao %>% 
      filter(tolower(partido) == "governo") %>% 
      mutate(id_proposicao = as.character(id_proposicao),
             id_votacao = as.character(id_votacao),
             voto = as.integer(voto)) 
    
    orientacao_merge <- voto_lider %>% 
      mutate(id_proposicao = as.character(id_proposicao),
             id_votacao = as.character(id_votacao),
             voto = as.integer(voto)) %>% 
      select(ano, id_proposicao, id_votacao, id_parlamentar, voto) %>% 
      full_join(orientacao_governo, by = c("id_proposicao", "id_votacao")) %>% 
      mutate(voto_governo = if_else(is.na(voto.y), 
                                    if_else(voto.x != 0, 
                                            voto.x, 
                                            voto.y), 
                                    voto.y)) %>% 
      mutate(partido = "GOVERNO") %>% 
      mutate(casa = "camara") %>% 
      select(ano = ano.x, id_proposicao, id_votacao, partido, voto = voto_governo, casa)
    
  } else {
    return(orientacao %>% filter(tolower(partido) == "governo"))
  }
}

#' @title Processa os dados de aderência do deputado ao Governo
#' @description Retorna dados de aderência dos parlamentares por votação e de forma sumarizada
#' @param votos Dataframe de votos
#' @param orientacao Dataframe de orientações
#' @param parlamentares Dataframe de parlamentares
#' @param filtrar TRUE se deseja filtrar fora os partidos com menos de 5 membros e os parlamentares com menos de 10 votações. 
#' FALSE se quiser capturar todos os parlamentares
#' @param casa Pode ser "camara" ou "senado"
#' @return Lista com dataframes com informações dos parlamentares e seus dados de aderência sumarizados e por votação
#' @examples
#' dados_aderencia <- processa_dados_deputado_aderencia_governo(votos, orientacao, parlamentares, filtrar = FALSE)
processa_dados_deputado_aderencia_governo <- function(votos, orientacao, parlamentares, filtrar = TRUE, casa = "camara") {
  library(tidyverse)
  
  if(tolower(casa) == "camara") {
    orientacao <- adiciona_hierarquia_orientacao_governo(votos, orientacao)
  } else {
    orientacao <- orientacao %>% 
      filter(tolower(partido) == "governo")
  }
  
  parlamentares_votos <- votos %>% 
    left_join(orientacao,
              by = c("id_proposicao", "id_votacao")) %>% 
    rename(voto_parlamentar = voto.x,
           voto_partido = voto.y,
           partido_parlamentar = partido.x,
           partido = partido.y,
           ano = ano.y
           ) %>% 
    mutate(id = as.character(id_parlamentar),
           partido = "GOVERNO") %>% 
    filter(!is.na(id))
    
  return(processa_calculo_aderencia(parlamentares_votos, parlamentares, filtrar))
}

#' @title Processa os dados de aderência de parlamentares ao Governo Por TEMA
#' @description Retorna dados de aderência dos deputados por votação e de forma sumarizada 
#' considerando o tema passado como parâmetro
#' @param tema_id Id do Tema para cálculo de aderência
#' @param proposicoes Dataframe com duas colunas (id_proposicao, id_tema). Servirá como lista para filtragem das votações.
#' @param votos Dataframe de votos nominais realiados em plenário
#' @param orientacao Dataframe de orientações dadas pelos partidos (e pelo Governo) em votações nominais em plenário.
#' @param deputados Dataframe de deputados com informações sobre o mesmo
#' @param filtrar TRUE se deseja filtrar fora os partidos com menos de 5 membros e os deputados com menos de 10 votações. 
#' FALSE se quiser capturar todos os deputados
#' @param casa Pode ser "camara" ou "senado"
#' @return Lista com dataframes com informações dos deputados e seus dados de aderência sumarizados e por votação
#' @examples
#' dados_aderencia_meio_ambiente <- processa_dados_aderencia_por_tema(0, proposicoes_temas, votos, orientacao, deputados, filtrar = FALSE)
processa_dados_aderencia_por_tema <- function(tema_id, proposicoes_temas, 
                                              votos, orientacoes, parlamentares, filtrar = TRUE,
                                              casa = "camara") {
  library(tidyverse)
  
  message(paste0("Calculando Aderência para o tema: ", tema_id))
  lista_proposicoes <- proposicoes_temas %>% 
    filter(id_tema == tema_id) %>% 
    pull(id_proposicao)

  if (length(lista_proposicoes) == 0) {
    return(tribble(~ id, ~ nome, ~ partido, ~ faltou, ~ partido_liberou,
                   ~ nao_seguiu, ~ seguiu, ~ total_votacoes, ~ freq) %>% 
             mutate(id = as.character(id),
                    nome = as.character(nome),
                    partido = as.character(partido),
                    faltou = as.numeric(faltou),
                    partido_liberou = as.numeric(partido_liberou),
                    nao_seguiu = as.numeric(nao_seguiu),
                    seguiu = as.numeric(seguiu),
                    total_votacoes = as.numeric(total_votacoes),
                    freq = as.numeric(freq)))
  }
  
  votos <- votos %>% 
    filter(id_proposicao %in% lista_proposicoes)
  
  orientacao <- orientacoes %>% 
    filter(id_proposicao %in% lista_proposicoes)
  
  aderencia_partido <- processa_dados_deputado_aderencia(votos, orientacao, parlamentares, filtrar)[[2]]
  
  orientacao_governo <- orientacao %>% 
    filter(tolower(partido) == "governo")
  
  aderencia_governo <- processa_dados_deputado_aderencia_governo(votos, orientacao_governo, parlamentares, filtrar, casa)[[2]]
  
  aderencia_tema <- aderencia_partido %>% 
    rbind(aderencia_governo) %>% 
    distinct()
  
  return(aderencia_tema)
}

#' @title Processa os dados de aderência de parlamentares para cada tema do Voz ativa
#' @description Retorna dados de aderência dos deputados por votação e de forma sumarizada 
#' considerando cada tema selecionado
#' @param proposicoes_temas Dataframe com duas colunas (id_proposicao, id_tema). Mapeamento entre prosições e temas.
#' @param temas Dataframe com informações dos temas
#' @param votos Dataframe de votos nominais realiados em plenário
#' @param orientacoes Dataframe de orientações dadas pelos partidos (e pelo Governo) em votações nominais em plenário.
#' @param deputados Dataframe de deputados com informações sobre o mesmo
#' @param filtrar TRUE se deseja filtrar fora os partidos com menos de 5 membros e os deputados com menos de 10 votações. 
#' FALSE se quiser capturar todos os deputados
#' @param casa Pode ser "camara" ou "senado"
#' @return Lista com dataframes com informações dos deputados e seus dados de aderência sumarizados e por votação
#' @examples
#' dados_aderencia_temas <- processa_dados_aderencia_temas(proposicoes_temas, temas, votos, orientacao, deputados, filtrar = FALSE)
processa_dados_aderencia_temas <- function(proposicoes_temas, temas, 
                                           votos, orientacoes, deputados, filtrar = FALSE,
                                           casa = "camara") {
  library(tidyverse)
  
  temas_lista <- temas %>% pull(id_tema)

  aderencia_temas <- tibble::tibble(id_tema = temas_lista) %>% 
    mutate(dados = purrr::map(
      id_tema, 
      processa_dados_aderencia_por_tema,
      proposicoes_temas,
      votos,
      orientacoes,
      deputados, 
      filtrar,
      casa
    )) %>% 
    unnest(dados)
 
  return(aderencia_temas) 
}
