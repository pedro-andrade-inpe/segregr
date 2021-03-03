# nocov start
utils::globalVariables(c(".", "%>%", ":=", "%like%", "%chin%", "set"))

.onLoad <- function(lib, pkg) {
  requireNamespace("sf")
  requireNamespace("dplyr")
  requireNamespace("data.table")
  requireNamespace("geodist")
}


#' @importFrom data.table := %between% fifelse %chin% set
NULL

# nocov end
