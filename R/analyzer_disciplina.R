#' @title Processa votações sem consenso
#' @description Processa as votações para filtrar quais não há consenso
#' @param votos Dataframe de votos. Os votos devem ser de uma mesma casa.
#' Ou seja ou câmara ou senado. Devem ter pelo menos 2 colunas: id_votacao e voto.
#' @return Dataframe com votações e as proporções de consenso
#' @examples
#' processa_votacoes_sem_consenso(votos)
processa_votacoes_sem_consenso <- function(votos, limite_consenso = 0.9) {
  votacoes_proporcao <- votos %>%
    filter(voto != 0) %>%
    group_by(id_votacao) %>%
    mutate(votos_validos = n()) %>%
    ungroup() %>%
    group_by(id_votacao, voto, votos_validos) %>%
    summarise(contagem = n()) %>%
    ungroup() %>%
    mutate(proporcao = contagem/votos_validos)
  
  votacoes_id <- votacoes_proporcao %>%
    group_by(id_votacao) %>%
    summarise(proporcao_max = max(proporcao)) %>%
    filter(proporcao_max < limite_consenso)
  
  return(votacoes_id)
}

#' @title Participou votações
#' @description Analisa se o parlamentar participou em mais de 10 votações
#' @param votos Dataframe de votos.
#' @param parlamentar_id Id de um parlamentar.
#' @return TRUE caso o parlamentar tenha participado em mais de 10 votações
#' @examples
#' participou_votacoes(285, votos)
participou_votacoes <- function(parlamentar_id, votos) {
  quantidade <- votos %>%
    filter(id_parlamentar == parlamentar_id) %>%
    nrow()
  
  if(quantidade > 10) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @title Processa bancada suficiente
#' @description Processa os parlamentares e filtra aqueles que não bancada
#' suficiente na camara/senado.
#' @return Dataframe com partidos e se sua bancada é suficiente
#' @examples
#' processa_bancada_suficiente()
processa_bancada_suficiente <- function(minimo_deputados = 5, minimo_senadores = 3) {
  partido_atual <- parlamentares %>%
    filter(is_parlamentar == 1, legislatura == 56) %>%
    group_by(partido, casa) %>%
    summarise(num_parlamentares = n_distinct(id_entidade)) %>%
    ungroup() %>%
    mutate(bancada_suficiente = if_else((casa == "camara" &
                                           num_parlamentares >= minimo_deputados) |
                                          (casa == "senado" &
                                             num_parlamentares >= minimo_senadores),
                                        TRUE,
                                        FALSE
    )) %>%
    select(partido, casa, bancada_suficiente) %>% 
    filter(!is.na(partido))
  
  return(partido_atual)
}

#' @title Get parlamentares info
#' @description Processa os parlamentares e filtra seus dados
#' em sua ultima legislatura 
#' @return Dataframe com dados dos parlamentares
#' @examples
#' get_parlamentares_info()
get_parlamentares_info <- function() {
  parlamentares_info <- parlamentares %>% 
    group_by(id_entidade) %>% 
    mutate(ultima_legislatura = max(legislatura)) %>% 
    filter(is_parlamentar == 1, legislatura == ultima_legislatura) %>% 
    select(id_entidade, id_entidade_parlametria, casa, nome, uf, partido_atual = partido)
  
  return(parlamentares_info)
}

#' @title Processa disciplina partidária
#' @description Processa as votações para filtrar quais não há consenso
#' @param votos Dataframe de votos
#' Os votos devem ter pelo menos 2 colunas: id_votacao e voto.
#' @param orientacoes Dataframe de orientações
#' @param enumera_orientacao Flag indicando se as orientações 
#' precisam ser transformadas em enum.
#' @return Dataframe de parlamentares e sua disciplina partidária
#' @examples
#' processa_disciplina_partidaria(votos, orientacoes)
#' @export
processa_disciplina_partidaria <- function(votos, orientacoes, enumera_orientacao = TRUE) {
  consenso_votacoes <- processa_votacoes_sem_consenso(votos)
  bancada_suficiente <- processa_bancada_suficiente()
  parlamentares_info <- get_parlamentares_info()
  lista_votos_validos <- c(-1, 1, 2, 3, 4)
  
  if (enumera_orientacao) {
    orientacoes <- orientacoes %>% 
      enumera_voto()
  }
  
  votos_filtrados <- votos %>%
    filter(id_votacao %in% (consenso_votacoes %>% pull(id_votacao))) %>%
    distinct(id_votacao, id_parlamentar, .keep_all = TRUE) %>% 
    mutate(partido = padroniza_sigla(partido))
  
  orientacoes_filtradas <- orientacoes %>%
    filter(id_votacao %in% (consenso_votacoes %>% pull(id_votacao))) %>%
    distinct(id_votacao, partido_bloco, .keep_all = TRUE) %>%
    select(id_votacao, voto = orientacao, partido_bloco) %>%
    select(id_votacao, orientacao = voto, partido_bloco) %>% 
    mutate_sigla_bloco() %>% 
    mutate(partido = padroniza_sigla(partido))

  votos_orientados <- votos_filtrados %>%
    left_join(orientacoes_filtradas, by = c("id_votacao"="id_votacao", "partido")) %>%
    distinct() %>%
    mutate(seguiu = if_else(voto == orientacao, 1, 0)) %>%
    mutate(seguiu = if_else(is.na(seguiu), 0, seguiu))
  
  disciplina <- votos_orientados %>% 
    mutate(voto_valido = if_else(voto %in% lista_votos_validos, 1, 0)) %>% 
    mutate(seguiu = if_else(voto_valido == 1, seguiu, 0)) %>% 
    group_by(id_parlamentar, casa, partido) %>% 
    summarise(votos_validos = sum(voto_valido), num_seguiu = sum(seguiu)) %>% 
    ungroup() %>% 
    mutate(disciplina = num_seguiu/votos_validos) %>% 
    mutate(id_parlamentar = as.character(id_parlamentar)) %>% 
    mutate(partido = padroniza_sigla(partido))
  
  df <- disciplina %>% 
    left_join(parlamentares_info %>% select(uf, nome, id_entidade, id_entidade_parlametria, partido_atual, casa), 
              by = c("id_parlamentar"="id_entidade", "casa")) %>% 
    left_join(bancada_suficiente, by = c("partido_atual"="partido", "casa")) %>% 
    mutate(partido_atual = padroniza_sigla(partido_atual)) %>% 
    filter(!is.na(id_parlamentar)) %>% 
    select(id_parlamentar, id_parlamentar_parlametria = id_entidade_parlametria,
           partido_disciplina = partido, partido_atual, casa,
           votos_validos, num_seguiu, disciplina, bancada_suficiente)
  
  return(df)
}
