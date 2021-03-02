# nocov start
utils::globalVariables(c(".", "%>%", ":=", "%like%", "%chin%", "set"))

.onLoad <- function(lib, pkg) {
  requireNamespace("sf")
  requireNamespace("dplyr")
  requireNamespace("data.table")
}


#' @importFrom data.table := %between% fifelse %chin% set
#' @importFrom methods is signature
NULL

# nocov end
