library(testthat)

# Analysis Kit is a Shiny application rather than a package, so the functions
# under test are sourced directly. Everything in R/ is pure and Shiny-free by
# design, which is what makes this possible.
#
# functions/ is sourced too when it holds anything: the pipeline tests light up
# automatically once the analysis functions land there, and skip until then.
for (dir in c("../R", "../functions")) {
  for (f in list.files(dir, pattern = "[.][Rr]$", full.names = TRUE)) {
    source(f)
  }
}

test_dir("testthat", env = globalenv())
