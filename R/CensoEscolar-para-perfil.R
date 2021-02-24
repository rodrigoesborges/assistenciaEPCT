##Dados do Censo Escolar de Assistência Estudantil
require(data.table)
require(tidyverse)
remotes::install_github("rodrigoesborges/microdadosBrasil")
require(microdadosBrasil)
#remotes::install_github('jimhester/archive')
#require(archive)
library(magrittr)
require(genderBR)
dadoslocais <- "dados/censoescolar"
if(!exists(dadoslocais)) {dir.create(dadoslocais,recursive = T)}
#Baixar dados e descompactar caso não existam na pasta de dados locais
anos <- 2012:2014
i <- 2014
#for (i in anos) {
download_sourceData("CensoEducacaoSuperior",i, unzip = T, root_path = dadoslocais)
#Ler dados de matrícula do censo
censoed <- read_CensoEducacaoSuperior("aluno",i,root_path = dadoslocais)
cursos_es <- read_CensoEducacaoSuperior("graduacao_presencial",i,root_path = dadoslocais)

##verifica número de matriculados nos cursos IFBA
## Código IFBA = 599

cursos_es %>% filter(CO_IES == 599) %>% 
  summarize(matriculados = sum(QT_MATRICULA_CURSO, na.rm = T))

ies <- read_CensoEducacaoSuperior("ies",i, root_path = dadoslocais)

censoed <- censoed %>% 
  left_join(ies %>% select(CO_IES, SGL_IES))


censoed_if <- censoed %>% filter(grepl("^IF",SGL_IES))


nas <- censoed_if %>% 
  select(starts_with("IN_APOIO"),
         starts_with("IN_BOLSA"),
         starts_with("IN_FIN"),
         starts_with("IN_RESERVA")) %>% 
  is.na %>% colSums

res_ap_soc <- censoed_if %>% filter(!is.na(IN_APOIO_SOCIAL)) %>%
  group_by(SGL_IES) %>% summarize(info = sum(IN_APOIO_SOCIAL))

apoio_social <- censoed_if %>% filter(!is.na(IN_APOIO_SOCIAL))

apoio_social %>% group_by(SGL_IES)%>%
  summarize(apoiados=sum(IN_APOIO_SOCIAL))  
#}



###Exemplo tratamento como amostra 
ifba <- apoio_social %>% filter(SGL_IES == "IFBA")

##Dos que são assistidos, sumário por etnia

ifba %>% filter(IN_APOIO_SOCIAL == 1 ) %>% 
  count(DS_COR_RACA_ALUNO)


#No caso de IFBA, APOIO_SOCIAL já maximizou resultados para 131
#|
# IN_APOIO_MORADIA == 1 | 
#   IN_APOIO_BOLSA_PERMANENCIA == 1 | 
#   IN_APOIO_ALIMENTACAO == 1 |
#   IN_APOIO_BOLSA_TRABALHO ==1 |
#   IN_APOIO_MATERIAL_DIDATICO |
#   IN_APOIO_TRANSPORTE

##Sumário dos não assistidos por etnia

ifba %>% filter(IN_APOIO_SOCIAL == 0) %>% 
  count(DS_COR_RACA_ALUNO)





### N. alunos com info de apoio_social 
# desproporcionalmente subrepresentados nos microdados
### Solução - tratar como amostra, atribuir peso 
## de acordo ao total obtido com dados da transparência
#Exemplo, dos 4.000 alunos, 2000 assistidos (transparencia)
#a amostra tem 131 <- 2000/131

####Verificar novamente, pois houve retorno de matriculados correto na última execução - problema aparentemente
###INEXISTENTE
### N. alunos na tabela inferior a matriculados
### Solução - tratar como amostra , 
### atribuir peso  inverso à proporção alunos (n.a.)/(matriculados-assistidos)
####INEXISTENTE

###Problema atual - (2000-131) assistidos figuram no censo como NA
#### alternativa cogitada - IMPUTAÇÃO mantendo estrutura dos 131 - sexo e raça
#### Imputar em linhas preferencialmente sem informação de raça


###Outra possibilidade a cogitar
### Obter selecão aleatória de 10% dos não assistidos com info completa  de raça e sexo
### Colocar peso


### Cobrir problemas em dados com imputação
## Dados de Raça
## Verificar variáveis - covariantes para imputação
## CO_TIPO_ESCOLA_ENS_MEDIO por exemplo
### Verificar possibilidade de cruzar com perfil geral IES
