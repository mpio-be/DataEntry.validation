#' ==========================================================================
#' validators used by aviaR package. 
#' require(wadeR)
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
#'  x = sdb::dbq(q = glue('select * from {tabs[i]} ') )
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
    set = list(dbq(q = "SELECT initials ii FROM RUFFatSEEWIESEN.AUTHORS")$ii   )  )),

  

    x[ , .(gps_id)]   %>% 
    interval_validator( v = data.table(variable = "gps_id",    lq = 1, uq = 13),     
    "GPS ID not in use" ),

    x[, .(gps_point)]  %>% 
    interval_validator( v = data.table(variable = "gps_point", lq = 1, uq = 999),     
    "GPS waypoint is over 999?" ),

    # Entry would be duplicate

    # TODO combo_validator
   

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
