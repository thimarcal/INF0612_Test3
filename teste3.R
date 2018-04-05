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

gcd <- function(...) {
  
  values <- c(...)
  valuesLen <- length(values)
  
  #if more than 1 value, calc base mdc with
  #first 2 values
  if(valuesLen > 1 ){
    mdc <- gcd2(values[1], values[2])
  }
  #if only 1 value, return itself
  else{
    return(values[1])
  }
  
  #If length => 3, calc mdc for each value with
  # base mdc and update base mdc
  i <- 3
  while(i <= valuesLen) {
    mdc <- gcd2(values[i], mdc)
    i <- i + 1
  }
  return(mdc)
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




