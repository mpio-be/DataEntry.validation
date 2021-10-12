#' ==========================================================================
#' validators used by aviaR package.
#' require(aviaR)
#' require(glue)
#' require(stringr)
#' require(foreach)
#' require(DataEntry.validation)
#'
#'
#' tabs = c('ADULTS', 'CHICKS', 'AVIARIES', 'FOUNDERS')
#'
#' o = foreach(i = 1:length(tabs), .combine = rbind) %do% {
#'
#'  x = sdb::dbq(q = glue('select * from RUFFatSEEWIESEN.{tabs[i]} ') )
#'  class(x) = c(class(x), tabs[i] )
#'
#'  inspector_loader(package = 'aviaR')
#'
#'  ii = inspector(x)
#'  evalidators(ii)[, table :=  tabs[i] ]
#'
#'  }
#'
#'
#' ==========================================================================

inspector.ADULTS <- function(x, ...){
  x[ , rowid := .I]

  list(

  # GENERAL #################################
    # Mandatory to enter
    x[, .(ID,av_ID, author, date, time)] %>%
    is.na_validator,

     # Correct format?
    x[, .(date)]  %>% date_validator(),
    x[, .(time)]  %>% hhmm_validator(),

      x[ , .(author)] %>%
    is.element_validator(v = data.table(variable = "author",
    set = list(dbq(q = "SELECT initials ii FROM RUFFatSEEWIESEN.AUTHORS")$ii   )  ) ),



    # Entry should be within specific interval
    x[, .(tarsus)]     %>%
    interval_validator( v = data.table(variable = "tarsus", lq = 20, uq = 24 ),
    "Measurement out of typical range" ),

    x[, .(culmen)] %>%
    interval_validator( v = data.table(variable = "culmen",     lq = 20, uq = 25 ),
    "Measurement out of typical range" ),

    x[, .(weight)]     %>%
    interval_validator( v = data.table(variable = "weight",   lq = 40, uq = 72 ),
    "Measurement out of typical range" )


  )


  }
