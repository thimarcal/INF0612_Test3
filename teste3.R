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
      mdc <- gcd2dim(b) (mdc, values[i])
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

mode <- function(vec) {
  numbers <- unique(vec)
  counts <- vector(length=length(numbers))
  i <- 1
  for (element in numbers) {
    counts[i] <- count(vec, element)
    i <- i+1
  }
  # Mostrar os maiores numeros
  numbers[is.element(counts, max(counts))]
}



## 3 - Binario para Decimal





## 4 - Ocorrencia de palavras





