### importação de dados do portal da transparência

##Instala e carrega ferramentas requeridas
#library(portransp)
#library(dplyr)

#http://www.portaltransparencia.gov.br/download-de-dados/despesas-execucao/
#202001
potr_download_all(opendata = 11, filename = "despesas",destfile = "dados/")

anos = 2010:2019
meses = 1:12

c(paste0(expand.grid(anos,meses)))

dados <- data.frame()

data <- rbind(data,)
