entradas = matrix(c(0, 0, 0, 1, 1, 0, 1, 1), nrow = 4, ncol = 2, byrow = T)
#AND
saidas = matrix(c(0, 0, 0, 1))
#OR
#saidas = matrix(c(0, 1, 1, 1))
#XOR
#saidas = matrix(c(0, 1, 1, 0))
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
print(calculaSaida(c(entradas[1,])))
print(calculaSaida(c(entradas[2,])))
print(calculaSaida(c(entradas[3,])))
print(calculaSaida(c(entradas[4,])))


