#' @title Computes the number of individuals moving from a region to another as a sum of multinomial variables.
#'
#' @description Computes the number of individuals moving from a region to another as a sum of multinomial varibales. It
#'   is an internal function that is called during the process of generating the Poisson multinomial distributed random
#'   values.
#'
#' @param categories a list of categories in the form \code{region-devCount}.
#'
#' @param cellNames A vector with the names of the regions.
#'
#' @return The sum of the multinomial variates.
#'
#' @import data.table
nIndividuals2 <- function(categories, cellNames){
  
  catPerDevice <- Reduce(rbind, lapply(as.character(categories), function(str) {
    
    as.numeric(strsplit(str, split = '-')[[1]])
  }))
  
  nDev <- length(categories)
  cells <- sort(unique(catPerDevice[, 1]))
  if (!all(cells %in% cellNames)) stop('[nIndividuals] cells')
  output <- numeric(length(cellNames))
  names(output) <- cellNames
  for (dev_index in 1:nDev){
    
    cell <- catPerDevice[dev_index, 1]
    devCount <- catPerDevice[dev_index, 2]
    tempVec <- numeric(length(cellNames))
    names(tempVec) <- as.character(cells)
    tempVec[as.character(cell)] <- devCount
    output <- output + tempVec    
  }
  
  return(output)
}
