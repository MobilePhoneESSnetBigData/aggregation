#' @title Generates random values from a Poisson multinomial distribution needed for the origin destination matrices.
#'
#' @description Generates random values from a Poisson multinomial distribution needed for the origin destination
#'   matrices. This function is only called internally.
#'
#' @param n The number of random values to generate.
#'
#' @param prob.dt A data.table object with the following columns:\code{device, cell, devCount, prob}.
#'
#' @return A matrix object with the random values generated according to a Poisson multinomial distribution.
#'
#' @import data.table
#' @import extraDistr
#' 
#' 
rNnetCond_Event <- function(n, prob.dt, cellNames){
  
  if (!all(c('device', 'region_from', 'region_to', 'devCount', 'prob') %in% names(prob.dt))) {
    stop('[rNnetCond_E] prob.dt must have columns device, region_from, region_to, devCount, prob.\n')
  }
  
  probSums <- prob.dt[, list(totalProb = sum(prob)), by = c('device', 'region_from')]$totalProb
  if (!all(abs(probSums - 1) < 1e-5)) {
    stop('[rNnetCond_E] The sum of probabilities per device is not 1.\n')
  }
  
  x1 <- prob.dt[
    , c('device', 'region_from', 'region_to', 'devCount', 'prob'), with = FALSE][
      , categories := paste0(region_from, '-', devCount)]

  x2 <- x1[
    , list(category = rcat(n, prob, categories)), by = c('device', 'region_from')][
      , nSim := 1:n, by = c('device', 'region_from')]

  x3 <- dcast(x2, device + region_from ~ nSim, value.var = 'category')

  x3.list <- split(x3, x3$region_from)

  
  x4.list <- lapply(x3.list, function(DT){

    region_from <- unique(DT[[2]])
    x4 <- DT[
      , lapply(.SD, nIndividuals2, cellNames = cellNames), .SDcols = names(DT)[-(1:2)]]
    x4.dt <- data.table(t(as.matrix(x4)))
    setnames(x4.dt, as.character(cellNames))
    x4_molten.dt <- melt(x4.dt[
      , region_from := region_from], id.vars = 'region_from', 
      variable.name = 'region_to', value.name = 'Nnet', variable.factor = FALSE)
    x4_molten.dt
  })

  x5.dt <- rbindlist(x4.list)
  return(x5.dt)
}
