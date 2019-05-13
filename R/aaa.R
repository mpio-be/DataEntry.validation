
.onAttach <- function (lib, pkgname="DataEntry.validation") {
   dcf <- read.dcf(file=system.file('DESCRIPTION', package=pkgname) )
    packageStartupMessage(paste('This is', pkgname, dcf[, 'Version'] ))

}


#' @import methods  data.table glue stringr magrittr
NULL