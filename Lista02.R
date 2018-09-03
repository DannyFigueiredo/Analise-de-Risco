################################# QUEST�O 1 #################################

moeda <- function(lanc){
  # Esta funcao simula o lancamento de uma moeda e conta a frequencia de caras
  # o argumento lanc representa o n�mero de lancamentos feitos
  # representamos o resultado cara como n�mero 1
  
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

################################# QUEST�O 2 #################################

tcl <- function(tamanhoAmostra){
  # Esta funcao toma uma amostra de uma vari�vel aleatoria que representa
  # a soma de 12 outras vari�veis aleat�rias uniformemente distribuidas entre 0 e 1
  
  x <- replicate(tamanhoAmostra, sum(runif(12)))
  media_x <- mean(x)
  var_x <- var(x)
  
  hist(x, xlab = "Poss�veis Valores de X", ylab = "Frequ�ncia dos Valores de X",
       main = "Distribui��o de X", col = "grey",
       sub = paste("M�dia de X: ", media_x, " e Variancia de X: ", var_x))
}

monteCarlo <- function(){
  
}

q2 <- function(){
  # Esta fun��o toma por default o tamanho da amostra como 50
  amostra = 50
  tcl(amostra)
  
  
}