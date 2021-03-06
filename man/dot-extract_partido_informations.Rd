% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetcher_deputados_info.R
\name{.extract_partido_informations}
\alias{.extract_partido_informations}
\title{Extrai informações de um partido a partir de uma URL}
\usage{
.extract_partido_informations(url)
}
\arguments{
\item{URL}{no formato "https://dadosabertos.camara.leg.br/api/v2/partidos/:num"}
}
\value{
Dataframe contendo informações de id e nome dos partidos
}
\description{
Recebe uma URL da câmara que possui o formato '/partidos/:num e extrai id e nome
}
\examples{
.extract_partido_informations("https://dadosabertos.camara.leg.br/api/v2/partidos/36835")
}
