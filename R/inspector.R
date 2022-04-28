#' Evaluate Validators safely
#' @param  L A list resulting from combining several validators
#' @export
evalidators <- function(L) {
  o <- try(rbindlist(L, fill = TRUE), silent = TRUE)

  if (all(c("rowid", "variable", "reason") %in% names(o))) {
    o <- o[, .(rowid = paste(rowid, collapse = ",")), by = .(variable, reason)]
  } else {
    o <- data.frame(
      rowid = NA, variable = NA,
      reason = "Validators are not working at the moment!"
    )
  }

  o
}


#' Data police
#'
#' Inspectors are usually a collection of validators
#'
#' @title   data inspector
#' @param   x a data.table with its (S3) class extended by the database table name (see server.R)
#' @export
#'
#' @examples
#' require(data.table)
#' require(magrittr)
#' x <- data.table(
#'   v1        = c(NA, NA, as.character(Sys.time() - 3600 * 24 * 10)),
#'   datetime_ = c("2016-11-23 25:23", as.character(Sys.time() - 100), as.character(Sys.time() + 100))
#' )
#' class(x) <- c(class(x), "tablex")
#'
#' inspector.tablex <- function(x) {
#'   list(
#'     x[, .(datetime_)] %>% POSIXct_validator(),
#'     x %>% is.na_validator()
#'   )
#' }
#'
#' evalidators(inspector(x))
#'
inspector <- function(x) {
  UseMethod("inspector", x)
}


inspector.default <- function(x) {
  data.frame(rowid = NA, variable = NA, reason = "Validators are not available for this table!")
}




#' @title   Inspector loader
#' @description Loads inspectors from a file.
#' @param path The path to the files containing the inspector functions.
#' @note This function is just sourcing the file but it does not return an error when something goes wrong.
#' @export
#' @examples
#' p <- system.file("inspectors", "DataEntry.R", package = "DataEntry.validation")
#' inspector_loader(p)
#'
inspector_loader <- function(path) {
  o <- try(source(path, local = .GlobalEnv), silent = TRUE)

  if (inherits(o, "try-error")) {
    warning("Could not source", basename(path), "in .GlobalEnv")
  }
}