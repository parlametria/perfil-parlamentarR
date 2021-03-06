library(tidyverse)
library(here)
source(here::here("crawler/parlamentares/votacoes-importantes/analyzer_votacoes_importantes.R"))

if(!require(optparse)){
  install.packages("optparse")
  suppressWarnings(suppressMessages(library(optparse)))
}

args = commandArgs(trailingOnly=TRUE)

message("LEIA O README deste diretório")
message("Use --help para mais informações\n")

option_list = list(
  make_option(c("-o", "--out"), type="character", default=here::here("crawler/raw_data/deputados_votacoes_importantes_leg55.csv"), 
              help="nome do arquivo de saída [default= %default]", metavar="character")
) 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

saida <- opt$out

message("Iniciando processamento...")
deputados_votos <- filter_deputados_atuais_votacoes()

message(paste0("Salvando o resultado em ", saida))
readr::write_csv(deputados_votos, saida)

message("Concluído!")
