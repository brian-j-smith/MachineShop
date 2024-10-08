knitr::opts_chunk$set(
  strip.white = FALSE,
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 7,
  fig.height = 4,
  fig.align = "center"
)


options(knitr.table.format = "html")


library(kableExtra)


rdoc_url <- function(names, rdoc_names = NULL) {
  version <- packageVersion("MachineShop")
  url <- paste0("https://www.rdocumentation.org/packages/MachineShop/versions",
              "/", version$major, ".", version$minor, ".0",
              "/topics/")

  if (is.null(rdoc_names)) {
    rdoc_names <- sapply(names, function(x) as.character(str2lang(x))[1])
  }

  paste0("[`", names, "`](", url, rdoc_names, ")")
}


median_range <- function(x, prefix = NULL, ...) {
  formatted_median <- format(median(x), trim = TRUE, ...)
  formatted_range <- format(range(x), trim = TRUE, ...)
  paste0(prefix, formatted_median,
         " (", toString(paste0(prefix, formatted_range)), ")")
}


n_perc <- function(x, digits = 2) {
  paste0(sum(x), " (", round(100 * mean(x), digits), "%)")
}


summary_kbl <- function(x, data) {
  vals <- sapply(unlist(unname(x), recursive = FALSE), function(el) {
    eval(el[[2]], envir = data)
  })

  kbl <- data.frame(Characteristic = names(vals), Value = vals) %>%
    kable(row.names = FALSE,
          align = c("l", "c")) %>%
    kable_styling(c("striped", "condensed"), full_width = FALSE,
                  position = "center")

  start_row <- 1
  for (i in seq(x)) {
    group_label <- names(x)[i]
    group_length <- length(x[[i]])
    if (nzchar(group_label)) {
      kbl <- kableExtra::group_rows(kbl, group_label, start_row,
                                    start_row + group_length - 1)
    }
    start_row <- start_row + group_length
  }

  kbl
}
