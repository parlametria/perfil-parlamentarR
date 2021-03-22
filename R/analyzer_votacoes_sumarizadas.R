#' @title Processa votações sumarizadas
#' @description Processa o número de votações por parlamentar com filtros de governismo e disciplina
#' @param votos Dataframe de votos. Devem ter pelo menos 2 colunas: id_votacao e voto.
#' @return Dataframe com ids de parlamentares e quantidade de votos
#' @examples
#' processa_votacoes_sumarizadas(votos)
#' @export
processa_votacoes_sumarizadas <- function(votos) {
  votacoes_disciplina <- processa_votacoes_sem_consenso(votos)
  votacoes_governismo <- votos %>% 
    distinct(id_votacao)
  lista_votos_validos <- c(-1, 1, 2, 3)
  
  parlamentares_info <- parlamentares %>% 
    group_by(id_entidade) %>% 
    mutate(ultima_legislatura = max(legislatura)) %>% 
    filter(is_parlamentar == 1, legislatura == ultima_legislatura) %>% 
    select(id_entidade, id_entidade_parlametria)
  
  votos_disciplina <- votacoes_disciplina %>% 
    left_join(votos, by = c("id_votacao")) %>% 
    mutate(voto_valido = if_else(voto %in% lista_votos_validos, 1, 0)) %>% 
    group_by(id_parlamentar_parlametria) %>% 
    mutate(num_votacoes_parlamentar_disciplina = sum(voto_valido)) %>% 
    left_join(parlamentares_info, by = c("id_parlamentar_parlametria"="id_entidade_parlametria")) %>% 
    distinct(id_parlamentar, id_parlamentar_parlametria, num_votacoes_parlamentar_disciplina) %>% 
    mutate(num_votacoes_totais_disciplina = votacoes_disciplina %>% distinct(id_votacao) %>% nrow())
  
  votos_governismo <- votacoes_governismo %>% 
    left_join(votos, by = c("id_votacao")) %>% 
    mutate(voto_valido = if_else(voto %in% lista_votos_validos, 1, 0)) %>% 
    group_by(id_parlamentar_parlametria) %>% 
    mutate(num_votacoes_parlamentar_governismo = sum(voto_valido)) %>% 
    left_join(parlamentares_info, by = c("id_parlamentar_parlametria"="id_entidade_parlametria")) %>% 
    distinct(id_parlamentar, id_parlamentar_parlametria, num_votacoes_parlamentar_governismo) %>% 
    mutate(num_votacoes_totais_governismo = votacoes_governismo %>% distinct(id_votacao) %>% nrow())
  
  votos_sumarizados <- votos_disciplina %>% 
    left_join(votos_governismo, by = c("id_parlamentar", "id_parlamentar_parlametria")) %>% 
    filter(!is.na(id_parlamentar))
  
  return(votos_sumarizados)
}
