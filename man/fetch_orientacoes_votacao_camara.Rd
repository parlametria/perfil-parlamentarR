% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetcher_orientacoes_camara.R
\name{fetch_orientacoes_votacao_camara}
\alias{fetch_orientacoes_votacao_camara}
\title{Importa e processa dados das orientações das bancadas na Câmara dos Deputados}
\usage{
fetch_orientacoes_votacao_camara(id_votacao)
}
\arguments{
\item{id_votacao}{Id da votação}
}
\value{
Dataframe contendo id da votação, id e voto dos deputados que participaram de cada votação
}
\description{
Recebe informações da votação específica para obtenção dos votos
}
\examples{
votacoes <- fetch_orientacoes_votacao_camara("2265603-43")
}
