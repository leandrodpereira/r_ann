entrada = c(1, 7, 5)
pesos = c(0.8, 0.1, 0)

soma = function(e, p){
  s = 0
  for (i in 1:3) {
    s = s + (e[i] * p[i]) 
  }
  return(s)
}

somaDot = function(e, p) {
  return(e %*% p) #dot product, produto escalar
}

s = somaDot (entrada, pesos)

stepFunction = function(soma){
  if(soma >= 1)  return(1)
  return(0)
}

r = stepFunction(s)



