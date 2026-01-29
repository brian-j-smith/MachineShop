with_parallel <- function(code) {

  if (utils::packageVersion("bartMachine") >= "1.4") {
    options(
      java.parameters = c(
        "-Xmx20g", "--add-modules=jdk.incubator.vector", "-XX:+UseZGC"
      )
    )
  } else {
    options(java.parameters = "-Xmx20g")
  }

  cores <- as.numeric(Sys.getenv("TEST_CORES"))
  if (is.na(cores)) cores <- 1
  if (require("doParallel", quietly = TRUE) && cores > 1) {
    doParallel::registerDoParallel(cores = cores)
    on.exit(doParallel::stopImplicitCluster())
  }

  code
}
