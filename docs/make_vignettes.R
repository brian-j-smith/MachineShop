dest <- "vignettes"

out <- file(file.path(dest, "UsersGuide.Rmd"), "w")

sourceLines <- function(file) {
  c(readLines(file.path("docs", "src", file)), "\n")
}

files <- c(
  "UsersGuide.Rmd",
  "overview.Rmd",
  "using_example.Rmd",
  "using_fit.Rmd",
  "using_variables.Rmd",
  "using_responses.Rmd",
  "using_metrics.Rmd",
  "using_resample.Rmd",
  "using_analyses.Rmd",
  "using_strategies.Rmd",
  "using_settings.Rmd",
  "using_extensions.Rmd",
  "reference_models.Rmd",
  "reference_metrics.Rmd"
)

for (file in files) {
  writeLines(sourceLines(file), out)
}

writeLines("# References", out)

close(out)

file.copy("docs/src/setup.R", file.path(dest, "setup.R"), overwrite = TRUE)
file.copy("docs/src/img/FigModelDAG.png", file.path(dest, "img/FigModelDAG.png"), overwrite = TRUE)
file.copy("docs/src/img/FigNestedCV.png", file.path(dest, "img/FigNestedCV.png"), overwrite = TRUE)
file.copy("docs/src/bibliography.bib", file.path(dest, "bibliography.bib"),
          overwrite = TRUE)
