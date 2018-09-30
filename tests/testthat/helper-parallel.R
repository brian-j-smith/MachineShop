with_parallel <- function(code) {
  if(require("doParallel", quietly = TRUE)) {
    cores <- max(parallel::detectCores() - 1, 1)
    doParallel::registerDoParallel(cores = cores)
    on.exit(doParallel::stopImplicitCluster())
  }
  
  code
}
