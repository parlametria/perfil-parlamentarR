processa_votacoes_sumarizadas <- function(votos) {
  votacoes_disciplina <- processa_votacoes_sem_consenso(votos)
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
  
  votos_governismo <- 
}
