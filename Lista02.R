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
  # Esta funcao toma uma amostra de uma vari�vel aleatoria X que representa
  # a soma de 12 outras vari�veis aleat�rias Y uniformemente distribuidas entre 0 e 1

  x <- replicate(tamanhoAmostra, sum(runif(12)))
  mediaX <- mean(x)
  varianciaX <- var(x)
  
  hist(x, xlab = "Poss�veis Valores de X", ylab = "Frequ�ncia dos Valores de X",
       main = "Distribui��o de X pelo TCL", col = "grey",
       sub = paste("M�dia de X: ", mediaX, " e Variancia de X: ", varianciaX))
}

monteCarlo <- function(){
  
}

q2 <- function(amostra = 50){
  # Esta fun��o toma por default o tamanho da amostra como 50
  
  tcl(amostra)
  
  
}

################################# QUEST�O 3 #################################


################################# QUEST�O 4 #################################

produto <- function(variavel1, variavel2){
  z <- variavel1 * variavel2
  
  hist(z, xlab = "Possiveis valores de Z", ylab = "Frequencia dos valores de Z",
       main = "Distribui��o do Produto de 2 VA's Normais", col = "lightblue")
  
}

divisao <- function(variavel1, variavel2){
  z <- x/y
  
  hist(z, xlab = "Possiveis valores de Z", ylab = "Frequencia dos valores de Z",
       main = "Distribui��o da Divis�o de 2 VA's Normais", col = "lightgreen")
}

a <- function(n, prob){
  #geral duas amostras aleat�rias com uma distribui��o N(0,1), de tamanho n
  X_norm <- rnorm(n)
  Y_norm <- rnorm(n)
  
  #Temos que Z_norm � uma vari�vel aleat�ria, tal que, Z= X_norm * Y_norm
  mu_X <- mean(X_norm) #m�dia da VA X_norm
  mu_Y <- mean(Y_norm) #m�dia da VA Y_norm
  sig_X <- var(X_norm) #vari�ncia da VA X_norm
  sig_Y <- var(Y_norm) #vari�ncia da VA Y_norm
  
  #aproxima��es da m�dia e da vari�ncia da VA Z= X_norm * Y_norm
  mediaZ <- mu_X * mu_Y
  VarianciaZ <- ((mu_Y^2)*(sig_X^2))+((mu_X^2)*(sig_Y^2))
  
  #Fun��o de probabilidade da nova variavel aleat�ria Z= X_norm * Y_norm
  qnorm(prob, mean=mediaZ, sd=sqrt(VarianciaZ))
}

q3 <- function(tamanhoAmostra = 50){
  x <- rnorm(tamanhoAmostra)
  y <- rnorm(tamanhoAmostra)
  
  produto(x, y)
  
  divisao(x, y)
}

################################# QUEST�O 5 #################################

q5 <- function(tamanho = 30){
  # Esta fun��o escolhe o maior valor entre as posi��es 2, 5 e 10 do vetor X
  # X � um vetor randomico de tamanho 10 de uma vari�vel aleat�ria que segue Normal(0,1)
  
  y <- numeric(tamanho)
  
  for (i in 1:tamanho){
    x <- rnorm(10)
    y[i] = max(x[2], x[5], x[10])
  } 
  
  hist(y, xlab = "Poss�veis Valores do M�ximo", ylab = "Frequ�ncia", col = "purple",
       main = "Fun��o de Probabilidade de uma VA M�ximo")
}

################################# QUEST�O 6 #################################

q6 <- function(g_liberdade,tamanho){
  # Esta fun��o obtem uma aproxima��o emp�rica para chi_quadrado
  
  chi_quadrado = numeric(tamanho)
  
  for (i in 1:g_liberdade){
    normal = rnorm(tamanho)^2
    chi_quadrado = chi_quadrado+normal
  }
  
  hist(chi_quadrado, xlab = "Valores de Chi", ylab = "Frequ�ncia", main = "Aproxim��o para Distribui��o Chi",
       col = "indianred2")
}

################################# QUEST�O 7 #################################

q7<- function(prob){
  #amosra aleat�ria de uma N(0,1) com tamanho 10000
  norm7 <- rnorm(10000)
  #Z=exp(X)
  Z_norm7 <- exp(norm7)
  
  hist(Z_norm7, xlab = "Valores de Z", ylab = "Frequ�ncia", col = "darkcyan",
       main = "Distri��o de e^N(0,1)")
  
  #M�dia e desvio padr�o de Z=exp(X)
  media_Znorm7 = mean(Z_norm7)
  sd_Znorm7 = sd(Z_norm7)
  
  #fun��o de probabilidade de Z=exp(X), sendo X~N(0,1)
  qnorm(prob, mean =media_Znorm7, sd =sd_Znorm7 )
}
