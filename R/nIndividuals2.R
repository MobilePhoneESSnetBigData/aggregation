nIndividuals2 <- function(categories, cellNames){
  
  catPerDevice <- Reduce(rbind, lapply(as.character(categories), function(str) {
    
    as.numeric(strsplit(str, split = '-')[[1]])
  }))

  nDev <- length(categories)
  n<-length(cellNames)
  cells <- sort(unique(catPerDevice[, 1]))
  #if (!all(cells %in% cellNames)) stop('[nIndividuals2] cells')
  
  output <- numeric(n)
  names(output) <- cellNames
  cellsChar <- as.character(cells)
  cc<-as.character(catPerDevice[, 1])
  zeros<-numeric(n)
  names(zeros) <- cellsChar
  for (dev_index in 1:nDev){
    cell <- catPerDevice[dev_index, 1]
    tempVec <- zeros
    #names(tempVec) <- cellsChar
    tempVec[cc[dev_index]] <- catPerDevice[dev_index, 2]
    output <- output + tempVec    
  }

  return(output)
}
