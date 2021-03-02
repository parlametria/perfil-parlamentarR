# perfil-parlamentarR


### Sobre os dados do pacote

Os dados usados internamente no pacote estão disponíveis no diretório `data-raw`. A função `package_data.R` é usada para comprimir e exportar esses dados para a pasta `data` no [formato recomendado](https://r-pkgs.org/data.html#data-sysdata) para o uso em pacotes. Caso queira atualizar esses dados basta executar essa função. Mais informações sobre esses dados podem ser obtidas no README do `data-raw`.


## Instalação

```
devtools::install_github("parlametria/perfil-parlamentarR@main")
```

Caso dê erro na instalação será preciso ter as dependências necessárias. Se você usa linux(ubuntu, debian) execute:

```
apt-get update
apt-get install -y libjpeg-dev libpoppler-cpp-dev
```

Em seguida instale os pacotes no R:

```
install.packages(c("futile.logger", "pscl", "pdftools", "eeptools"))
```

Volte a instalar o pacote do perfil usando o devtools.
