#' @title Generates random values according to a Poisson multinomial probability distribution.
#'
#' @description Generates random values according to a Poisson multinomial probability distribution. A point estimation
#' derived from this distribution (mean, mode) represents an estimation of the number of individuals in a region.
#'
#' @param n The number of values to be generated
#'
#' @param dupFileName The name of the .csv file with the duplicity probability for each device
#'
#' @param regsFileName The name of the .csv file defining the regions. It has two columns: \code{ tile, region}. The
#'   first column contains the IDs of each tile in the grid while the second contains the number of a region.
#'   
#' @param times An optional vector with the time instants when the events were registered.
#'
#' @param postLocPath The path where the files with the posterior location probabilities for each devices can be found.
#'   A file with the location probabilities should have the name \code{postLocDevice_ID.csv} where \code{ID} is replaced
#'   with the device ID.
#'
#'
#' @import parallel
#' @import doParallel
#' @import data.table
#' @import deduplication
#' @export
rNnetEvent <- function(n, dupFileName, regsFileName, postLocPath, times = NULL) {
  # 1. read duplicity probs.
  
  if (!file.exists(dupFileName))
    stop(paste0(dupFileName, " does not exists!"))
  
  dupProbs <- fread(
    dupFileName,
    sep = ',',
    header = TRUE,
    stringsAsFactors = FALSE
  )
  devices<-as.numeric(dupProbs[,1][[1]])
  
  # 2. read regions
  if (!file.exists(regsFileName))
    stop(paste0(regsFileName, " does not exists!"))
  
  regions <- fread(
    regsFileName,
    sep = ',',
    header = TRUE,
    stringsAsFactors = FALSE
  )
  
  # 3. read posterior location probabilities
  ndevices <- nrow(dupProbs)
  postLoc<-list(length = ndevices)
  for( i in 1:ndevices) {
    postLoc[[i]] <- readPostLocProb(postLocPath, dupProbs[i,1])
  }
  
  T <- ncol(postLoc[[1]])
  if(!is.null(times))
    if(T != length(times))
      stop("Inconsistent data provided: the length of times vector is not the same as the number of time instants computed from the posterior location probabilities files")

  nTiles<-nrow(postLoc[[1]])
  
  
  # 4. computation begins ...
  #### parallel  
  # build cluster

  if (Sys.info()[['sysname']] == 'Linux' |
      Sys.info()[['sysname']] == 'Darwin') {
    cl <- makeCluster(detectCores(), type = "FORK")
  } else {
    cl <- makeCluster(detectCores())
    clusterEvalQ(cl, library("data.table"))
    clusterEvalQ(cl, library("deduplication"))
    clusterEvalQ(cl, library("extraDistr"))
    clusterExport(cl, c('postLoc', 'devices', 'ndevices', 'nTiles', 'dupProbs', 'regions', 'rNnet_Event', 'nIndividuals'), envir = environment())
  }
  ichunks <- clusterSplit(cl, 1:T)
  
  res <-
    clusterApplyLB(
      cl,
      ichunks,
      doAggr,
      n,
      ndevices,
      nTiles,
      postLoc,
      devices,
      dupProbs,
      regions
    )
  stopCluster(cl)
  result <- rbindlist(res)
  if(!is.null(times)) {
    indices <- result$time 
    result$time <- times[indices]
  }
  return (result)
}



