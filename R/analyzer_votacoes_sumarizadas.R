#' @title Processa votações sumarizadas
#' @description Processa o número de votações por parlamentar com filtros de governismo e disciplina
#' @param votos Dataframe de votos. Devem ter pelo menos 2 colunas: id_votacao e voto.
#' @param orientacoes Dataframe de orientações. 
#' @return Dataframe com ids de parlamentares e quantidade de votos
#' @examples
#' processa_votacoes_sumarizadas(votos)
#' @export
processa_votacoes_sumarizadas <- function(votos, orientacoes) {
  votos <-
    votos %>% filter(!is.na(id_parlamentar_parlametria) |
                       !is.na(id_parlamentar))

  votos_orientados <- processa_votos_orientados(votos, orientacoes, F)
  lista_votos_validos <- c(-1, 1, 2, 3)
  
  parlamentares_info <- get_parlamentares_info() %>%
    ungroup() %>% 
    select(id_entidade_parlametria, partido_atual)
  
  orientacoes_validas <- votos_orientados %>% 
    distinct(id_votacao, partido, casa, orientacao) %>% 
    filter(orientacao %in% lista_votos_validos) %>% 
    group_by(partido, casa) %>% 
    summarise(num_votacoes_totais_disciplina = n_distinct(id_votacao)) %>% 
    ungroup()
  
  votos_disciplina <- votos_orientados %>%
    mutate(voto_valido = if_else(voto %in% lista_votos_validos, 1, 0)) %>%
    mutate(voto_valido_com_orientacao = if_else(voto_valido == 1 & (orientacao %in% lista_votos_validos), 1, 0)) %>% 
    group_by(id_parlamentar, id_parlamentar_parlametria, casa, partido) %>%
    summarise(num_votacoes_parlamentar_disciplina = sum(voto_valido_com_orientacao), .groups = "drop") %>%
    ungroup() %>%
    left_join(orientacoes_validas, by = c("partido", "casa")) %>% 
    inner_join(
      parlamentares_info,
      by = c("id_parlamentar_parlametria" = "id_entidade_parlametria")
    ) %>%
    filter(partido == partido_atual) %>% 
    distinct(
      id_parlamentar,
      id_parlamentar_parlametria,
      num_votacoes_parlamentar_disciplina,
      num_votacoes_totais_disciplina
    )
  
  votos_governismo <- votos %>%
    mutate(voto_valido = if_else(voto %in% lista_votos_validos, 1, 0)) %>%
    group_by(id_parlamentar_parlametria) %>%
    mutate(num_votacoes_parlamentar_governismo = sum(voto_valido)) %>%
    group_by(casa) %>%
    mutate(num_votacoes_totais_governismo = n_distinct(id_votacao)) %>%
    inner_join(
      parlamentares_info,
      by = c("id_parlamentar_parlametria" = "id_entidade_parlametria")
    ) %>%
    distinct(
      id_parlamentar,
      id_parlamentar_parlametria,
      num_votacoes_parlamentar_governismo,
      num_votacoes_totais_governismo
    )
  
  votos_sumarizados <- votos_disciplina %>%
    left_join(votos_governismo,
              by = c("id_parlamentar", "id_parlamentar_parlametria", "casa")) %>%
    filter(!is.na(id_parlamentar)) %>%
    select(
      id_parlamentar,
      id_parlamentar_parlametria,
      casa,
      num_votacoes_parlamentar_governismo,
      num_votacoes_totais_governismo,
      num_votacoes_parlamentar_disciplina,
      num_votacoes_totais_disciplina
    )
  
  return(votos_sumarizados)
}
