library(tidyverse)
library(pscl)

#' @title Calcula Governismo
#' @description Calcula Governismo usando a técnica de ideal points
#' @param votos Dataframe de votos para cálculo do Governismo. Os votos devem ser de uma mesma casa.
#' Ou seja ou câmara ou senado. Devem ter pelo menos 3 colunas: id_votacao, id_parlamentar e voto.
#' @return Dataframe com parlamentares e as dimensões para o Governismo
#' @examples
#' processa_governismo(votos)
#' @export
processa_governismo <- function(votos) {
  
  # Voto ~ código (no dataframe passado como parâmetro)
  # Não ~ -1
  # Sim ~ 1
  # Obstrução|P-OD ~ 2
  # Abstenção ~ 3
  # Art. 17|art. 51 RISF ~ 4
  # Liberado ~ 5
  # Falta ~ 0
  votos_alt <- votos %>% 
    mutate(voto = case_when(
      voto == 1 ~ 1,
      voto == -1 ~ 6,
      voto == 0 ~ 0,
      TRUE ~ 9,
    ))
  
  spread_votos <- votos_alt %>% 
    distinct(id_votacao, id_parlamentar, voto) %>% 
    spread(id_votacao, voto) %>%
    mutate_all(replace_na, 0)
  
  rc <- rollcall(
    spread_votos %>% select(-id_parlamentar),
    yea = 1,
    nay = 6,
    missing = 9,
    notInLegis = 0,
    legis.names = spread_votos %>% pull(id_parlamentar),
    desc = "Brasil",
    vote.names = spread_votos %>% select(-id_parlamentar) %>% colnames()
  )
  
  ideal <- ideal(
    rc,
    d = 2,
    maxiter = 10000,
    thin = 100,
    burnin = 5000,
    impute = FALSE,
    priors = NULL,
    startvals = "eigen",
    store.item = FALSE,
    file = NULL,
    verbose = FALSE
  )
  
  ideal_df <- ideal$xbar %>% as.data.frame()
  
  results <- tibble(id_parlamentar = rownames(ideal_df),
                    D1 = ideal_df %>% pull(D1),
                    D2 = ideal_df %>% pull(D2))
  
  return(results)
}
