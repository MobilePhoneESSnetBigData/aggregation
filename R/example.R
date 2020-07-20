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
#' # set the duplicity probabilities file name, i.e. the file with duplicity probability for each device
#' dpFile<-system.file(path, 'duplicity.csv', package = 'aggregation')
#' 
#' # set the regions file name, i.e. the file defining the regions for wich we need the estimation of the number
#' # of individuals detected by network.
#' rgFile<-system.file(path, 'regions.csv', package = 'aggregation')
#' 
#' # generate n random values
#' n <- 1e3
#' vals <- rNnetEvent(n, dpFile, rgFile, system.file(path, package = 'aggregation'))
#'
#' # print the number of detected individuals for each region, for each time instant
#' regions <- as.numeric(unique(vals$region))
#' times <- unique(vals$time)
#'
#' for(r in regions) {
#'     print(paste0("region: ", r))
#'     for(t in times) {
#'         print(paste0("time instant: ", t, " number of individuals: " , mean(vals[region == r][time ==t]$N)))
#'     }
#' }
example <- function() {}
