quantidade <- function(comprimento){
  # Esta função separa, em um vetor:
  # 1 - a quandidade de tubos necessário para uma obra com o cumprimento da rota preferida
  # 2 - a quantidade de tubos que ultrapassam a rota preferida

  tubos <- c(260000/8, (comprimento - 260000)/8)
  tubos
}

material <- function(rota, amostra, probabilidade){
  # Esta função calcula o gasto de com tubulação
  
  library(triangle)
  
  # vetor de  quantidade de material necessário, dado o padrão de 8m
  quant <- quantidade(rota)
  
  # Preço da tubulação a cada 8m
  preco <- rtriangle(amostra, 725, 790, 740)
  
  # Rota 1 é a rota preferida dos engenheiros
  rota1 <- quant[1] * preco
  # Rota 2 é a rota alternativa composta por uma distribuição bernoulli
  rota2 <- quant[2] * rbinom(amostra, 1, probabilidade) * preco
  tubulacao <- rota1 + rota2
  tubulacao
}

maoDeObra <- function(rota, amostra, probabilidade){
  # Esta função calcula o custo total da mão de obra
  
  library(triangle)
  
  # Quantidade base para o calculo de base referente a 8m
  quant <- quantidade(rota)
  
  tempoEscavacao <- rtriangle(amostra, 12, 25, 16)
  custoEscavacao <- rtriangle(amostra, 17, 23, 18.5)
  rota1Escavacao <- quant[1] * tempoEscavacao * custoEscavacao
  rota2Escavacao <- quant[2] * tempoEscavacao * custoEscavacao * rbinom(amostra, 1, probabilidade)
  custoTotalEscavacao <- rota1Escavacao * rota2Escavacao
  
  tempoSoldagem <- rtriangle(amostra, 4, 5, 4.5)
  custoSoldagem <- rtriangle(amostra, 17, 23, 18.5)
  rota1Soldagem <- ((quant[1] + 1 ) * tempoSoldagem) * custoSoldagem
  rota2Soldagem <- (quant[2] * tempoSoldagem) * custoSoldagem * rbinom(amostra, 1, probabilidade)
  custoTotalSoldagem <- rota1Soldagem + rota2Soldagem
  
  rotaKm <- rota / 1000
  custoAcabamento <- rtriangle(amostra, 14000, 17000, 15000)
  custoTotalAcabamento <- rotaKm * custoAcabamento
  
  cMaoDeObra <- custoTotalEscavacao + custoTotalSoldagem + custoTotalAcabamento
  cMaoDeObra
}

servico <- function(amostra, rota, probabilidade){
  # Esta funcao calcula o custo total do setor de serviços
  library(triangle)
  
  quant <- quantidade(rota)
  
  transporte <- rtriangle(amostra, 6.1, 7.4, 6.6)
  rota1Transporte <- quant[1] * transporte
  rota2Transporte <- quant[2] * transporte * rbinom(amostra, 1, probabilidade)
  custoTransporte <- rota1Transporte + rota2Transporte
  
  filtragem <- rtriangle(amostra, 165000, 188000, 173000) * rbinom(amostra, 1, probabilidade)

  cServico <- custoTransporte + filtragem
  cServico
}

custoTotal <- function(amostra, rota, prob){
  total <- material(rota, amostra, prob) + maoDeObra(rota, amostra, prob) + servico(amostra, rota, prob)
  total
}

q1 <- function(rota = 290000, amostra = 3000, prob = 0.35){
  custo <- custoTotal(amostra, rota, prob)
  
  hist(custo)
  ecdf(custo)
}
