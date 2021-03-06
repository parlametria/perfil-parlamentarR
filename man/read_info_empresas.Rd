% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reader_financiamento.R
\name{read_info_empresas}
\alias{read_info_empresas}
\title{Lê csv informações das empresas que tiveram sócios que doaram para parlamentares}
\usage{
read_info_empresas(info_empresas_path)
}
\arguments{
\item{info_empresas_path}{Dataframe com informação das empresas que tiveram sócios que doaram para
parlamentares na campanha eleitoral. 
O csv de 2018 pode ser obtido aqui: 
https://raw.githubusercontent.com/parlametria/perfil-parlamentar-dados/master/parlametria/raw_data/empresas/info_empresas_doadores_todos_parlamentares.csv}
}
\value{
Dataframe com doações feitas
}
\description{
Lê csv informações das empresas que tiveram sócios que doaram para parlamentares
}
\examples{
read_info_empresas("https://raw.githubusercontent.com/parlametria/perfil-parlamentar-dados/master/parlametria/raw_data/empresas/info_empresas_doadores_todos_parlamentares.csv")
}
