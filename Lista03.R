material <- function(rota = 290000, amostra = 3000){
  library(triangle)
  
  quantidade <- rota/8
  
  tubulacao <- quantidade * rtriangle(amostra, 725, 790, 740)
  tubulacao
}

maoDeObra <- function(amostra = 3000, rota = 290000){
  library(triangle)
  
  quantidade <- rota/8
  
  escavacao <- rtriangle(amostra, 12, 25, 16)
  custoMaoDeObra <- rtriangle(amostra, 17, 23, 18.5)
  custoTotalEscavacao <- escavacao * custoMaoDeObra
  
  quantidadeSoldagem <- quantidade + 1
  soldagem <- rtriangle(amostra, 4, 5, 4.5)
  custoTotalSoldagem <- (quantidadeSoldagem * soldagem) * custoMaoDeObra
  
  rotaKm <- rota / 1000
  custoAcabamento <- rtriangle(amostra, 14000, 17000, 15000)
  custoTotalAcabamento <- rotaKm * custoAcabamento
  
  cMaoDeObra <- custoTotalEscavacao + custoTotalSoldagem + custoTotalAcabamento
  sMaoDeObra
}

serviço <- function(amostra = 3000, rota = 290000){
  library(triangule)
  
  quantidade <- rota / 8
  
  transporte <- rtriangle(amostra, 6.1, 7.4, 6.6)
  custoTransporte <- quantidade * transporte
  
  filtragem <- rtriangle(amostra, 165000, 188000, 173000)
  
  cServico <- custoTransporte + filtragem
  cServico
}
