################################# QUESTÃO 1 #################################

moeda <- function(lanc){
  # Esta funcao simula o lancamento de uma moeda e conta a frequencia de caras
  # o argumento lanc representa o número de lancamentos feitos
  # representamos o resultado cara como número 1
  
  x <- sample(0:1, size = lanc, replace = TRUE)
  freq_cara <- sum(x)/lanc
}

q1 <- function(){
  frequenciaDeCaras <- data.frame( lancamentos = c(10, 100, 1000, 10000),
                                   frequencia = c(1:4))
  
  for (i in 1:length(frequenciaDeCaras$lancamentos)){
    frequenciaDeCaras$frequencia[i] <- moeda(frequenciaDeCaras$lancamentos[i])
  }
  
  View(frequenciaDeCaras)
}

################################# QUESTÃO 2 #################################

tcl <- function(tamanhoAmostra){
  # Esta funcao toma uma amostra de uma variável aleatoria X que representa
  # a soma de 12 outras variáveis aleatórias Y uniformemente distribuidas entre 0 e 1
  
  x = numeric(tamanhoAmostra) #variável aleatória 
  medX = numeric(tamanhoAmostra) #médias das variáveis aleatórias xi's
  varX = numeric(tamanhoAmostra) #variância das variáveis aleatórias xi's
  
  for(i in 1:tamanhoAmostra){
    y = runif(12)
    x[i] = sum(y)
    medX[i] = mean(y)
    varX[i] = var(y)
  }
  
  #Média da amostra X
  mediaX = sum(medX)
  #Variância da Variável Aleatória X
  varianciaX = sum(varX)
  
  hist(x, xlab = "Possíveis Valores de X", ylab = "Frequência dos Valores de X",
       main = "Distribuição de X", col = "grey",
       sub = paste("Média de X: ", mediaX, " e Variancia de X: ", varianciaX))
}

monteCarlo <- function(){
  
}

q2 <- function(){
  # Esta função toma por default o tamanho da amostra como 50
  amostra = 50
  tcl(amostra)
  
  
}