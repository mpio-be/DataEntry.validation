#' @name meltall
#' @title melt all columns in a data.table
#' @param x  a data.table
#' @param na.rm  TRUE by default
#' @export
meltall <- function(x, na.rm = TRUE) {
    if(! 'rowid' %in% names(x)) {
        x[, rowid := .I]
        message("rowid is missing from x so it will be added now. If x is a subset then rowid does not reflect the row position in the non-subsetted x")
        }

    
    suppressWarnings(data.table::melt(x, id.vars = 'rowid', variable.factor = FALSE, value.factor = FALSE, na.rm = na.rm))
    
    }



#' @name strp_date_or_time
#' @title strp datetime or time (Mysql compatible)
#' @param x  strip datetime or date
#' @export
#' @examples
#' require(magrittr)
#' x = c(Sys.Date() %>% as.character, Sys.time()%>% as.character )
#' strp_date_or_time(x)

strp_date_or_time <- function(x) {
 s1 = strptime(x, "%Y-%m-%d %H:%M")
 s2 = strptime(x, "%Y-%m-%d")
 o = data.frame(s1, s2)

 o[is.na(o$s1), 's1'] = o[is.na(o$s1), 's2']
 
 as.POSIXct(o$s1)

}