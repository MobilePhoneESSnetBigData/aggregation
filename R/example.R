#' Example of using the aggregation package
#'
#' # This is just an example on how to use this package to generate a Poisson multinomial distribution in order to obtain
#' a point estimate of the number of individuals detected by mobile network in a region.
#'
#' @examples
#'
#' # set the folder where the necessary input files are stored
#' path      <- 'extdata'
#'
#' prefix='postLocDevice'
#'
#' # set the grid file name, i.e. the file the parameters of the grid
#' grFile<-system.file(path, 'grid.csv', package = 'aggregation')
#' 
#' # set the duplicity probabilities file name, i.e. the file with duplicity probability for each device
#' dpFile<-system.file(path, 'duplicity.csv', package = 'aggregation')
#' 
#' # set the regions file name, i.e. the file defining the regions for wich we need the estimation of the number
#' # of individuals detected by network.
#' rgFile<-system.file(path, 'regions.csv', package = 'aggregation')
#' 
#' # generate n random values
#' n <- 1e3
#' nNet <- rNnetEvent(n, grFile, dpFile, rgFile, system.file(path, package = 'aggregation'), prefix)
#'
#' # print the mean number of detected individuals for each region, for each time instant
#' regions <- as.numeric(unique(nNet$region))
#' times <- unique(nNet$time)
#'
#' for(r in regions) {
#'     print(paste0("region: ", r))
#'     for(t in times) {
#'         print(paste0("time instant: ", t, " number of individuals: " , mean(nNet[region == r][time ==t]$N)))
#'     }
#' }
#' 
#' 
#' # For the origin-destination matrix we proceed in a similar way
#' prefixJ <- 'postLocJointProbDevice'
#' nnetOD <- rNnetEventOD(n, dpFile, rgFile, system.file(path, package = 'aggregation'), prefixJ)
#' 
#' # The origin-destination matrix can be computed now very simple
#' First we choose two time instants
#' t1 <- 0
#' t2 <- 10
#' # The we extract the regions:
#' regions_from <- sort(as.numeric(unique(nnetOD$cell_from)))
#' regions_to <- sort(as.numeric(unique(nnetOD$cell_to)))
#' 
#' # Now we compute the origin-destination matrix:
#' ODmat<-matrix(nrow = length(regions_from), ncol = length(regions_to))
#' for(r1 in regions_from) {
#'    for(r2 in regions_to) {
#'        ODmat[r1,r2] <- round(mean(nnetOD[time_from==t1][time_to==t2][region_from==r1][region_to==r2]$Nnet))
#'    }
#' }    
#' ODmat
#' 
#' 
example <- function() {}
