% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reader_financiamento.R
\name{read_empresas_doadores}
\alias{read_empresas_doadores}
\title{Lê csv de sócios de empresas que doaram para parlamentares}
\usage{
read_empresas_doadores(empresas_doadores_path)
}
\arguments{
\item{empresas_doadores_path}{Dataframe com informação dos sócios de empresas que doaram para
parlamentares na campanha eleitoral. 
O csv de 2018 pode ser obtido aqui: 
https://raw.githubusercontent.com/parlametria/perfil-parlamentar-dados/master/parlametria/raw_data/empresas/empresas_doadores_todos_parlamentares.csv}
}
\value{
Dataframe com doações feitas
}
\description{
Lê csv de sócios de empresas que doaram para parlamentares
}
\examples{
read_empresas_doadores("https://raw.githubusercontent.com/parlametria/perfil-parlamentar-dados/master/parlametria/raw_data/empresas/empresas_doadores_todos_parlamentares.csv")
}
