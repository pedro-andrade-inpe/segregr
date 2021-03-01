# nocov start
# utils::globalVariables(c(".", "%>%", ":=", "%like%", "%chin%", "set"))

.onLoad <- function(lib, pkg) {
  requireNamespace("sf")
}

# nocov end
