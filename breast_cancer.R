#Exemplo com 30 neurônios na camada de entrada; 1 camada oculta com 3 neurônios e 1 neurônio na camada de saída.
#Dados obtidos de https://archive.ics.uci.edu/ml/index.php
#Algoritmo batch gradient descent.

sigmoid = function(soma){
  return(1 / (1 + exp(-soma)))
}

sigmoidDerivada = function(sig){
  return(sig * (1 - sig))
}

a = sigmoid(50)
b = sigmoidDerivada(a)

#Carregando arquivos
entradas = read.csv("entradas.csv")
entradas = data.matrix(entradas)
saidas = read.csv("saidas.csv")
saidas = data.matrix(saidas)

#Definindo pesos iniciais aleatórios
pesos0 = matrix(runif(90, min = -1, max = 1), nrow = 30, ncol = 3, byrow = T)
pesos1 = matrix(runif(3, min = -1, max = 1), nrow = 3, ncol = 1, byrow = T)

epocas = 10000

momento = 1
taxaAprendizagem = 0.5

for (j in 1:epocas) {
  camadaEntrada = entradas
  somaSinapse0 = camadaEntrada %*% pesos0
  camadaOculta = sigmoid(somaSinapse0)
  
  somaSinapse1 = camadaOculta %*% pesos1
  camadaSaida = sigmoid(somaSinapse1)
  
  erroCamadaSaida = saidas - camadaSaida
  mediaAbsoluta = mean(abs(erroCamadaSaida))
  print(paste('Erro: ', mediaAbsoluta,'[iteração: ', j , ']'))
  
  derivadaSaida = sigmoidDerivada(camadaSaida)
  deltaSaida = erroCamadaSaida * derivadaSaida
  
  pesos1Transposta = t (pesos1)
  deltaSaidaXPeso = deltaSaida %*% pesos1Transposta
  deltaCamadaOculta = deltaSaidaXPeso * sigmoidDerivada(camadaOculta)
  
  camadaOcultaTransposta = t(camadaOculta)
  pesosNovo1 = camadaOcultaTransposta %*% deltaSaida
  pesos1 = (pesos1 * momento) + (pesosNovo1 * taxaAprendizagem)
  
  camadaEntradaTransposta = t(camadaEntrada)
  pesosNovo0 = camadaEntradaTransposta %*% deltaCamadaOculta
  pesos0 = (pesos0 * momento) + (pesosNovo0 * taxaAprendizagem)
}
