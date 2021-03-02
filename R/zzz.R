# nocov start
# utils::globalVariables(c(".", "%>%", ":=", "%like%", "%chin%", "set"))
utils::globalVariables(c(".", "%>%"))

.onLoad <- function(lib, pkg) {
  requireNamespace("sf")
  requireNamespace("dplyr")
}

# nocov end
