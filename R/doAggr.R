doAggr <- function(ichunks, n, ndevices, nTiles, postLoc, devices, dupProbs, regions) {
  nIndividualsT <- list(length=length(ichunks))
  k<-1
  for(t in ichunks) {
    dedupLoc <- data.table()
    for(j in 1:ndevices) {
      x <- cbind((0:(nTiles-1)), postLoc[[j]][,t], rep(devices[j], times = nTiles))
      dedupLoc <- rbind(dedupLoc, x)
      x <- NULL
    }
    colnames(dedupLoc)<-c('tile', 'eventLoc', 'device')
    dedupLoc2_1 <- merge(dedupLoc, dupProbs, by.x = 'device', by.y = 'deviceID', all.x = TRUE)
    dedupLoc1_1 <- copy(dedupLoc2_1)[ , singleP := 1- dupP][, dupP := NULL]
    dedupLoc2_1[,prob := eventLoc * dupP][, devCount := 0.5][, eventLoc := NULL][,dupP := NULL]
    dedupLoc1_1[,prob := eventLoc * singleP][, devCount := 1][, eventLoc := NULL] [, singleP := NULL]
    dedupProbs <- rbind(dedupLoc1_1, dedupLoc2_1)
    rm(dedupLoc2_1)
    rm(dedupLoc1_1)
    dedupProbs <- merge( dedupProbs, regions, by = c('tile'))
    dedupProbs <- dedupProbs[ ,list(prob = sum(prob)), by = c('device', 'region', 'devCount')]
    setnames(dedupProbs, 'region', 'cell')
    
    nIndividuals_MNO_RSS <- as.data.table(rNnet_Event(n, dedupProbs))
    nIndividuals_MNO_RSS_molten <- melt( nIndividuals_MNO_RSS, variable.name = 'region', value.name = 'N')
    #nIndividuals_MNO_RSS_molten <- nIndividuals_MNO_RSS_molten[,-1]
    nIndividuals_MNO_RSS_molten <- cbind(rep(t, times = nrow(nIndividuals_MNO_RSS_molten)), nIndividuals_MNO_RSS_molten)
    colnames(nIndividuals_MNO_RSS_molten)<-c('time', 'region', 'N')
    nIndividualsT[[k]]  <- nIndividuals_MNO_RSS_molten
    k<-k+1
  }
  return(rbindlist(nIndividualsT) )
}