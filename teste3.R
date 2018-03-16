########################################
# Teste 3 - INF-0612          
# Nome(s): 
########################################


## 1 - Maximo Divisor Comum

gcd2 <- function(x, y) {
  if (y == 0) {
    return(x)
  } else {
    return(gcd2(y, x %% y))
  }
}

gcd <- function (...) {
  # Ordenar os valores ajuda no cálculo
  values <- sort(c(...))
  
  if(length(values) <= 1) {
    warning('Falta valores para calcular o GCD')
  } else if (length(values) == 2) {
    return(gcd2(values[1], values[2]))
  } else {
    mdc <- gcd2(values[1], values[2])
    
    for (i in 3:length(values)) {
      mdc <- gcd2(mdc, values[i])
    }
    
    mdc
  }
}




## 2 - Moda da Idade da Turma

count <- function(vector, element) {
  count <- 0
  for (i in vector) {
    if (i == element) {
      count <- count + 1
    }
  }
  return(count)
}





## 3 - Binario para Decimal





## 4 - Ocorrencia de palavras





