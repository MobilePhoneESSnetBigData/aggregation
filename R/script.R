
rNnetEvent <- function() {
# 1. read duplicity probs.
dupFileName <- 'data/duplicity.csv'

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
regFileName <-'data/regions.csv'

if (!file.exists(regFileName))
  stop(paste0(regFileName, " does not exists!"))

regions <- fread(
  regFileName,
  sep = ',',
  header = TRUE,
  stringsAsFactors = FALSE
)

# 3. read posterior location probabilities
ndevices <- nrow(dupProbs)
postLoc<-list(length = ndevices)
path <- 'data'
for( i in 1:ndevices) {
  postLoc[[i]] <- readPostLocProb(path, dupProbs[i,1])
}

T <- ncol(postLoc[[1]])
nTiles<-nrow(postLoc[[1]])


# 4. computation begins ...
#### parallel  
# build cluster
library(parallel)
  
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
    ndevices,
    nTiles,
    postLoc,
    devices,
    dupProbs,
    regions
  )
stopCluster(cl)
result <- rbindlist(res)
return (result)
}



