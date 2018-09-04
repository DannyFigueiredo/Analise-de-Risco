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

  x <- replicate(tamanhoAmostra, sum(runif(12)))
  mediaX <- mean(x)
  varianciaX <- var(x)
  
  hist(x, xlab = "Possíveis Valores de X", ylab = "Frequência dos Valores de X",
       main = "Distribuição de X pelo TCL", col = "grey",
       sub = paste("Média de X: ", mediaX, " e Variancia de X: ", varianciaX))
}

monteCarlo <- function(){
  
}

q2 <- function(amostra = 50){
  # Esta função toma por default o tamanho da amostra como 50
  
  tcl(amostra)
  
  
}

################################# QUESTÃO 3 #################################

produto <- function(variavel1, variavel2){
  z <- variavel1 * variavel2
  
  hist(z, xlab = "Possiveis valores de Z", ylab = "Frequencia dos valores de Z",
       main = "Distribuição do Produto de 2 VA's Normais", col = "lightblue")

}

divisao <- function(variavel1, variavel2){
  z <- x/y
  
  hist(z, xlab = "Possiveis valores de Z", ylab = "Frequencia dos valores de Z",
       main = "Distribuição da Divisão de 2 VA's Normais", col = "lightgreen")
}

a <- function(n, prob){
  #geral duas amostras aleatórias com uma distribuição N(0,1), de tamanho n
  X_norm <- rnorm(n)
  Y_norm <- rnorm(n)
  
  #Temos que Z_norm é uma variável aleatória, tal que, Z= X_norm * Y_norm
  mu_X <- mean(X_norm) #média da VA X_norm
  mu_Y <- mean(Y_norm) #média da VA Y_norm
  sig_X <- var(X_norm) #variância da VA X_norm
  sig_Y <- var(Y_norm) #variância da VA Y_norm
  
  #aproximações da média e da variância da VA Z= X_norm * Y_norm
  mediaZ <- mu_X * mu_Y
  VarianciaZ <- ((mu_Y^2)*(sig_X^2))+((mu_X^2)*(sig_Y^2))
  
  #Função de probabilidade da nova variavel aleatória Z= X_norm * Y_norm
  qnorm(prob, mean=mediaZ, sd=sqrt(VarianciaZ))
}

q3 <- function(tamanhoAmostra = 50){
  x <- rnorm(tamanhoAmostra)
  y <- rnorm(tamanhoAmostra)
  
  produto(x, y)
  
  divisao(x, y)
}

################################# QUESTÃO 4 #################################


################################# QUESTÃO 5 #################################


################################# QUESTÃO 6 #################################


################################# QUESTÃO 7 #################################

