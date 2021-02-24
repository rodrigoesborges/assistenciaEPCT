##Processamento de documentos/recibos de despesa diária do OGU
library(tidyverse)
library(data.table)
library(portransp)
destdados <- "dados/docdespesas/"

#Separada a rotina de baixar dados da e análise para permitir contorno de erros de download/conexão
baixa_diarios <- function(i,mesexpand,destdados) {
  dias_no_mes <- lubridate::days_in_month(as.Date(paste(i,sprintf('%02d',mesexpand),"01",sep = "-")))
  ex_per <- paste0(i,sprintf('%02d',mesexpand),sprintf('%02d',1:dias_no_mes))
  
  baixa_port <- function(x) {
    #No caso do Windows, possivelmente mudar download.file.mode para "wb"
    #Adicionado manejo de erros
    Sys.sleep(1)
    try(potr_download(10,x,paste0(getwd(),"/",destdados,i,"/"), download.file.mode = "auto"),
        outFile = paste0(destdados,"erros_download.log"))
  }
  
  lapply(ex_per,baixa_port)
}

prepara_csvs <- function(i,destdados) {
  zipados <- dir(paste0(destdados,i,"/"),pattern = paste0("*.zip"),full.names = T)
  lapply(zipados,unzip,exdir=paste0(destdados,i),overwrite = T)
  
  ##Caso Windows, pular linhas até OBS LEITURA CONJUNTA
  if(.Platform$OS.type == "unix") {
    
    ###Conserto de codificação
    arqs <- dir(path= paste0(destdados,i), pattern = ".csv")
    cons_cod <- function(arq) {
      arqr <- paste0(destdados,i,"/",arq)
      f <- tempfile()
      system(paste0("iconv -f iso-8859-1 -t utf-8 ",arqr, " -o ",f))
      system(paste0("mv ",f," ",arqr))
    }
    lapply(arqs,cons_cod)
  } 
  
}
##Função para obter informação relevante relativa a assistência estudantil a partir dos docs diários de despesa
estimativa_ass <- function(i,mesexpand, destdados) {
  padraonome <- "_Despesas_Pagamento.csv"
  pad_fav <- "_Despesas_Pagamento_FavorecidosFinais.csv"
  #i <- 2014
  #mesexpand <- 1
  dias_no_mes <- lubridate::days_in_month(as.Date(paste(i,sprintf('%02d',mesexpand),"01",sep = "-")))
  ex_per <- paste0(i,sprintf('%02d',mesexpand),sprintf('%02d',1:dias_no_mes))
  
  #OBS LEITURA CONJUNTA
  arqcsvs <- paste0(destdados,i,"/",ex_per,padraonome)
  pag_dia <- rbindlist(lapply(arqcsvs,read_csv2))
  
  #1. Info passo 1
  print(paste("1. Lidos pagamentos do mês",mesexpand))
  ##Elementos de Despesa Relevantes
  #18 - Auxílio Financeiro a estudantes
  #20 - Auxílio Financeiro a Pesquisadores
  #48 - Outros Auxílios Financeiros a PF
  elementos <- c(18)
  pag_ass <- pag_dia %>% filter(`Código Elemento de Despesa` %in% elementos)
  
  pag_ifes <- pag_ass %>% filter(grepl("^INST.*FED",Gestão,ignore.case = T))
  
  #pag_ifes <- pag_ifes %>% filter(grepl("ASSISTENCIA AO ESTUDANTE DA EDUCACAO",`Plano Orçamentário`))
  pag_ifes %<>% mutate(across(starts_with("Valor"),as.numeric)) 
  
  
  #Equivaler este - pagtos `Valor do Pagamento Convertido pra R$` e este `Valor do Pagamento em R$` - favs
  names(pag_ifes)[33] <- "Valor do Pagamento em R$"
  
  opspag <- unique(pag_ifes$`Código Pagamento`)
  
  #2. Info passo 2
  print(paste("2. Processados e filtrados os pagamentos do mês",mesexpand))
  
  ##LEITURA CONJUNTA DA LISTA DE FAVORECIDOS FINAIS
  arqfav <- paste0(destdados,i,"/",ex_per,pad_fav)
  fav_dia <- rbindlist(lapply(arqfav,read_csv2))
  
  #3. Info passo 3
  print(paste("3. Lidos favorecidosfinais do mês",mesexpand))
  
  fav_dia %<>% filter(`Código Pagamento` %in% opspag)
  
  p_ex <- pag_ifes %>% select(-c(`Valor do Pagamento em R$`,`Data Emissão`, `Código Favorecido`, Favorecido))
  
  fav_dia <- fav_dia %>% left_join(p_ex)
  
  #4. Info passo 4
  print(paste("4. Completada a informação de favorecidos do mês",mesexpand))
  
  ###Adiciona o Cödigo da Lista - atributo de favorecidos, complementando observação
  
  fav_dia %<>% mutate(Observação = paste(Observação,`Código Lista`,"||"),
                      `Valor do Pagamento em R$` = as.double(`Valor do Pagamento em R$`)) %>% 
    select(-`Código Lista`)
  
  ordpagfav <- unique(fav_dia$`Código Pagamento`)
  
  
  #Seleciona linhas de pagamentos que não tinham favorecidos finais distintos especificados
  
  pag_difes <-   pag_ifes %>% filter(!(`Código Pagamento` %in% ordpagfav))
  
  favorecidos_fin <- bind_rows(fav_dia,pag_difes) %>% arrange(`Código Pagamento`, `Data Emissão`)
  
  #5. Info passo 5
  print(paste("5. Unidas informações completas de favorecidos finais do mês",
              mesexpand,
              "com ",
              nrow(favorecidos_fin),
              "linhas com duplicações"))
  
  favorecidos_fin <- favorecidos_fin %>% filter(`Valor do Pagamento em R$` > 0) %>% 
    group_by(Favorecido)%>% 
    summarize(across(where(is.numeric),sum, na.rm = T),
              across(where(is.character),first),
              qtd_auxilios = n())
  #6. Info passo 6
  print(paste("6. Agrupada a informação por favorecido do mês",mesexpand, "enxugado para ",
              nrow(favorecidos_fin),"linhas"))
  
  favorecidos_fin$sexo <- get_gender(str_split_fixed(favorecidos_fin$Favorecido," ",2)[,1], threshold = 0.65)
  #7. Info passo 7
  print(paste("7. Estimado o sexo a partir do nome dos favorecidos no mês",mesexpand))
  
  rm(pag_ifes)
  gc()
  favorecidos_resumo <- favorecidos_fin %>% 
    group_by(sexo,Gestão) %>% 
    summarize(n_assistidos = n(), 
              valor_medio = mean(`Valor do Pagamento em R$`),
              periodo = paste(i,sprintf('%02d',mesexpand),sep = "-"),
              ano = i
    )
}
#Rodar 1 vez completo
meses <- expand.grid(2014:2020,1:12) %>% arrange(Var1)
mapply(baixa_diarios,meses[[1]],meses[[2]],destdados)
#Rodar 1 vez completo
lapply(2014:2020,prepara_csvs,destdados = destdados)

##A verificar, arquivos com tamanho 0
#Econtrados com o seguinte comando de shell
#find . -type f -name "*.zip" -size -10c -exec ls -l {} + | sed -e 's/.*\(\([0-9]\)\{8\}\).*/\1/g'
##Ao que parece não há (segundo webUI para todos de 2015)
arqzeros <- c(20150118,20151122,20151227,20160124,20160724,20161225,20170319,20181230,20190120,20190126,20191124,20200209,20200216,20200223,20200321,20200524,20201108,20201225,20201231)


ass_2014 <- rbindlist(mapply(estimativa_ass,2014,1:12,destdados, SIMPLIFY = F))

ass_2015 <- rbindlist(mapply(estimativa_ass,2015,1:12,destdados, SIMPLIFY = F))
###Agora, com os dados sintetizados, filtrar novamente para retirar Gestões estranhas às IFS e 
# obter mediana de assistidos (moda pouco útil, devido à pouca probabilidade de repetições)


ass_resumo <- ass_2014 %>% 
  group_by(Gestão,sexo) %>% 
  summarize(mediana_assistidos = median(n_assistidos),media_assistidos = mean(n_assistidos))

### limite de assistência nao meritocratica e de 1/2 SM
library(ipeaData)
smcod <- "MTE12_SALMIN12"
teto <- (ipeadata(smcod) %>% filter(ANO == i & MES == mesexpand) %>% select(VALVALOR))/2


