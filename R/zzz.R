# nocov start
# utils::globalVariables(c(".", "%>%", ":=", "%like%", "%chin%", "set"))

.onLoad <- function(lib, pkg) {
  requireNamespace("tidyverse")
  requireNamespace("sf")
  requireNamespace("geodist")
}

# nocov end
