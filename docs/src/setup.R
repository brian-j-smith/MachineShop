knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 7,
  fig.height = 4,
  fig.align = "center"
)

library(kableExtra)
library(ggplot2)

rdoc_url <- function(name) {
  version <- packageVersion("MachineShop")
  url <- paste0("https://www.rdocumentation.org/packages/MachineShop/versions",
              "/", version$major, ".", version$minor, ".0",
              "/topics/")
  paste0("[`", name, "`](", url, name, ")")
}
