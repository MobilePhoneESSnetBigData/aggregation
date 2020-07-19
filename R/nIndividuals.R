nIndividuals <- function(categories){
  
  catPerDevice <- Reduce(rbind, lapply(as.character(categories), function(str) {
    
    as.numeric(strsplit(str, split = '-')[[1]])
  }))
  nDev <- length(categories)
  cells <- sort(unique(catPerDevice[, 1]))
  nCells <- max(cells)
  output <- numeric(nCells)
  names(output) <- cells
  for (dev_index in 1:nDev){

    cell <- catPerDevice[dev_index, 1]
    devCount <- catPerDevice[dev_index, 2]
    tempVec <- numeric(nCells)
    tempVec[cell] <- devCount
    output <- output + tempVec    
  }

  return(output)
}