with_parallel <- function(code) {
  cores <- as.numeric(Sys.getenv("TEST_CORES"))
  if (is.na(cores)) cores <- 1
  if (require("doParallel", quietly = TRUE) && cores > 1) {
    doParallel::registerDoParallel(cores = cores)
    on.exit(doParallel::stopImplicitCluster())
  }

  code
}
