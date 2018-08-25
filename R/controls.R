setClass("BootControl",
  slots = c(number = "numeric"),
  contains = "MLControl"
)

BootControl <- function(number = 25, ...) {
  new("BootControl", number = number, ...)
}


setClass("CVControl",
  slots = c(folds = "numeric", repeats = "numeric"),
  contains = "MLControl"
)

CVControl <- function(folds = 10, repeats = 1, ...) {
  new("CVControl", folds = folds, repeats = repeats, ...)
}
