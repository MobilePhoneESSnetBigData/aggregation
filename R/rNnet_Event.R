rNnet_Event <- function(n, prob.dt){
  
  
  if (!all(c('device', 'cell', 'devCount', 'prob') %in% names(prob.dt))) {
    
    stop('[rNnet_Event] prob.dt must have columns device, cell, devCount, prob.\n')
  }
  
  probSums <- prob.dt[, list(totalProb = sum(prob)), by = 'device']$totalProb
  if (!all(abs(probSums - 1) < 1e-8)) {
    
    stop('[rNnet_Event] The sum of probabilities per device is not 1.\n')
  }
  
  x1 <- prob.dt[
      , c('device', 'cell', 'devCount', 'prob'), with = FALSE][
        , categories := paste0(cell, '-', devCount)]
  x2 <- x1[
    , list(category = rcat(n, prob, categories)), by = 'device'][
      , nSim := 1:n, by = 'device']
  x3 <- dcast(x2, device ~ nSim, value.var = 'category')
  cells <- names(nIndividuals(x3[[2]]))
  x4 <- t(as.matrix(x3[
    , lapply(.SD, nIndividuals), .SDcols = names(x3)[-1]]))
  dimnames(x4)[[2]] <- cells
  return(x4)
}
