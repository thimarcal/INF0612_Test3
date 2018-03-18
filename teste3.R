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

mode <- function(vector) {

  #Initialize moda with first element
  if(length(vector) > 0){
    totalCount <- 1
    moda <- vector[1]
  }

  for(i in vector) {

    icount <- count(vector, i)
    #if count of i is the bigest untill now, then update moda
    if(icount > totalCount) {
      moda <- i
      totalCount <- icount
    }
    #if already have a number with same # of occurrences
    #add value to moda vector
    if(icount == totalCount) {
      if(!is.element(i, moda)) {
        moda <- c(moda, i)
      }
    }
  }
  return(moda)
  
}



## 3 - Binario para Decimal





## 4 - Ocorrencia de palavras





