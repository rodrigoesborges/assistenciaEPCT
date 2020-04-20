explora <- read.csv2("dados/despesaOrcamentaria.csv")
#PRograma sempre o mesmo , não puxar
#Categoria Econömica apenas

explora$X..Realizado.do.orçamento..com.relação.ao.orçamento.atualizado...R.. <- as.numeric(
  gsub("%","",explora$X..Realizado.do.orçamento..com.relação.ao.orçamento.atualizado...R..))
)