#' @title Processa votações sumarizadas
#' @description Processa o número de votações por parlamentar com filtros de governismo e disciplina
#' @param votos Dataframe de votos. Devem ter pelo menos 2 colunas: id_votacao e voto.
#' @return Dataframe com ids de parlamentares e quantidade de votos
#' @examples
#' processa_votacoes_sumarizadas(votos)
#' @export
processa_votacoes_sumarizadas <- function(votos) {
  votos <-
    votos %>% filter(!is.na(id_parlamentar_parlametria) |
                       !is.na(id_parlamentar))
  votacoes_disciplina <- processa_votacoes_sem_consenso(votos)
  lista_votos_validos <- c(-1, 1, 2, 3)
  
  parlamentares_info <- get_parlamentares_info() %>%
    select(id_entidade, id_entidade_parlametria)
  
  votos_disciplina <- votacoes_disciplina %>%
    left_join(votos, by = c("id_votacao", "casa")) %>%
    mutate(voto_valido = if_else(voto %in% lista_votos_validos, 1, 0)) %>%
    group_by(id_parlamentar_parlametria) %>%
    mutate(num_votacoes_parlamentar_disciplina = sum(voto_valido)) %>%
    group_by(casa) %>%
    mutate(num_votacoes_totais_disciplina = n_distinct(id_votacao)) %>%
    inner_join(
      parlamentares_info,
      by = c("id_parlamentar_parlametria" = "id_entidade_parlametria")
    ) %>%
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
