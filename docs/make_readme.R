rmarkdown::render("docs/src/README.Rmd", output_dir = ".")
file.remove("README.html")
