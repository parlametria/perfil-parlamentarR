% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyzer_financiamento.R
\name{processa_doacoes_socios_setores}
\alias{processa_doacoes_socios_setores}
\title{Processa doações por sócios de empresas}
\usage{
processa_doacoes_socios_setores(
  parlamentares_doadores,
  info_empresas,
  empresas_doadores
)
}
\arguments{
\item{parlamentares_doadores}{Dataframe com informação das doações recebidas por parlamentares
durante a campanha eleitoral.}

\item{info_empresas}{Dataframe com informação das empresas que tiveram sócios que doaram para
parlamentares na campanha eleitoral.}

\item{empresas_doadores}{Dataframe com informação dos sócios de empresas que doaram para
parlamentares na campanha eleitoral.}
}
\value{
Dataframe com doações de sócios de empresas
Cada linha do dataframe contém um sócio de empresa (cpf_cnpj_socio) e um parlamentar que recebeu a doação
}
\description{
Processa doações feitas por sócios de empresas
}
\examples{
processa_doacoes_socios_setores()
}
