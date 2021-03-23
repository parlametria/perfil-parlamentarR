#' @title Processa votações sumarizadas
#' @description Processa o número de votações por parlamentar com filtros de governismo e disciplina
#' @param votos Dataframe de votos. Devem ter pelo menos 2 colunas: id_votacao e voto.
#' @return Dataframe com ids de parlamentares e quantidade de votos
#' @examples
#' processa_votacoes_sumarizadas(votos)
#' @export
processa_votacoes_sumarizadas <- function(votos) {
  votos <- votos %>% 
    filter(!is.na(id_parlamentar_parlametria))
  
  votacoes_disciplina <- processa_votacoes_sem_consenso(votos) %>% 
    distinct(id_votacao, casa)
  
  lista_votos_validos <- c(-1, 1, 2, 3)

  votos_disciplina <- votacoes_disciplina %>% 
    left_join(votos, by = c("id_votacao", "casa")) %>% 
    mutate(voto_valido = if_else(voto %in% lista_votos_validos, 1, 0)) %>% 
    group_by(id_parlamentar_parlametria, casa) %>% 
    mutate(num_votacoes_parlamentar_disciplina = sum(voto_valido)) %>% 
    ungroup() %>% 
    group_by(casa) %>% 
    mutate(num_votacoes_totais_disciplina = n_distinct(id_votacao)) %>% 
    ungroup() %>% 
    distinct(id_parlamentar, id_parlamentar_parlametria, casa, num_votacoes_parlamentar_disciplina, num_votacoes_totais_disciplina)
  
  votos_governismo <- votos %>% 
    mutate(voto_valido = if_else(voto %in% lista_votos_validos, 1, 0)) %>% 
    group_by(id_parlamentar_parlametria, casa) %>% 
    mutate(num_votacoes_parlamentar_governismo = sum(voto_valido)) %>% 
    ungroup() %>% 
    group_by(casa) %>% 
    mutate(num_votacoes_totais_governismo = n_distinct(id_votacao)) %>% 
    ungroup() %>% 
    distinct(id_parlamentar, id_parlamentar_parlametria, casa, num_votacoes_parlamentar_governismo, num_votacoes_totais_governismo)
  
  votos_sumarizados <- votos_disciplina %>% 
    left_join(votos_governismo, by = c("id_parlamentar", "id_parlamentar_parlametria", "casa"))
  
  return(votos_sumarizados)
}
