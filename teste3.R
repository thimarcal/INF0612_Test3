########################################
# Teste 3 - INF-0612          
# Nome(s): Rodrigo Chaves Sim
#          Thiago Gomes Marçal Pereira
########################################


## 1 - Maximo Divisor Comumrep
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
    ## warning('Falta valores para calcular o GCD')
    ## Não sabemos se poderíamos utilizar a warning, pois não foi formalmente 
    ## ensinada em sala, então está comentada
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
binToDec <- function (...) {
  binario <- list(...)
  
  decimal <- vector(length=length(binario))
  j <- 1
  for (element in binario) {
    number <- 0
    for (i in 1:length(element)) {
      if (element[i] > 1) {
        warning('Invalid binary digit')
        return(0)
      }
      number <- number + element[i]*2^(length(element)-i)
    }
    decimal[j] <- number
    j <- j+1
  }
  decimal
}



## 4 - Ocorrencia de palavras
wordCount <- function(word, sentence) {
  words <- strsplit(tolower(sentence), "[.,!? ]", fixed = FALSE)
  word <- gsub(x=tolower(word), pattern = "[.,!? ]", replacement="")
  count(is.element(words[[1]], word), TRUE)
  
  ## ou
  ## sum(is.element(words[[1]], word))
  ## que produz o mesmo resultado
}




