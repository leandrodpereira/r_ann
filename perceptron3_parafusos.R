#Classificação de parafurso pelo diâmetro
#Entradas para treinamento
entradas = matrix(c(4, 3, 1, 1, 2, 2, 2, 3, 1, 2, 5, 3, 3, 3), nrow = 7, ncol = 2, byrow = T)
#Saída - Classificação (a = 0; b = 1)
saidas = matrix(c(0, 0, 1, 1, 0, 0, 1))
pesos = matrix(c(0.0, 0.0))
taxaAprendizagem = 0.1

stepFunction = function(soma) {
  if (soma >= 1) {
    return (1)
  }
  return (0)
}

calculaSaida = function(registro) {
  soma = registro %*% pesos
  return (stepFunction(soma))
}

erroTotal = 1
while (erroTotal != 0) {
  erroTotal = 0
  for (i in 1:length(saidas)) {
    saidaCalculada = calculaSaida(c(entradas[i,]))
    erro = saidas[i] - saidaCalculada
    erroTotal = erroTotal + erro
    for (j in 1:length(pesos)) {
      pesos[j] = pesos[j] + (taxaAprendizagem * entradas[i,j] * erro)
      print(paste('Peso atualizado: ', pesos[j]))
    }
  }
  print(paste('Total de erros: ', erroTotal))
}

print('Rede neural treinada')
entradasTeste = matrix(c(4, 8, 7, 1, 2, 2, 3, 4), nrow = 4, ncol = 2, byrow = T)

print(calculaSaida(c(entradasTeste[1,])))
print(calculaSaida(c(entradasTeste[2,])))
print(calculaSaida(c(entradasTeste[3,])))
print(calculaSaida(c(entradasTeste[4,])))
