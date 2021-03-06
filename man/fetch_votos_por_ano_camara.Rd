% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetcher_votos_camara.R
\name{fetch_votos_por_ano_camara}
\alias{fetch_votos_por_ano_camara}
\title{Recupera informações de votos de todas as votações de uma determinada proposição para um determinado ano}
\usage{
fetch_votos_por_ano_camara(id_proposicao, ano)
}
\arguments{
\item{ano}{Ano para o período de votações}

\item{id_votacao}{ID da votacao}
}
\value{
Votos dos parlametares para a proposição (inclui várias votações)
}
\description{
A partir do id da proposição e do ano recupera votos que aconteceram na Câmara dos Deputados
}
\examples{
votos <- fetch_votos_por_ano_camara(2190237, 2019)
}
