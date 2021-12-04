# TODO move to UI

#' ==========================================================================
#' validators used by bib2 package. 
#'
#' require(bib2)
#' x = bibq('select * from ADULTS  ')
#' class(x) = c(class(x), 'ADULTS')

#' inspector_loader(package = 'bib2')
#' 
#' ii = inspector(x)
#' evalidators(ii)
#'
#' ==========================================================================


inspector.ADULTS <- function(x) {

    measures      = bibq( 'select tarsus, weight, P3 from BTatWESTERHOLZ.ADULTS')
    measures      = melt(measures)[!is.na(value)]
    measures      = measures[, .(lq = quantile(value, 0.005), uq = quantile(value, 0.995)), by = variable]
    Nchar         = data.table(variable = c('ID', 'UL', 'LL', 'UR', 'LR', 'age', 'sex'), 
                                      n = c(   7, 1, 1, 1, 1,1, 1) )

    transponders  = bibq( "select LEFT(transponder , 6) x  from COMMON.TRANSPONDERS_LIST")$x

    preDefined1   = data.table( variable = 'transponder', set = c(list(transponders)))
    preDefined2   = data.table( variable = 'box',         set = c(list(1:277)))

    list(

    x[, .(date_time_caught, author)] %>% 
    is.na_validator , 

    x[is.na(recapture), .(age,tarsus,weight,P3,transponder)] %>% 
    is.na_validator('Mandatory at first recapture') , 


    x[ , .(date_time_caught)] %>% 
    POSIXct_validator , 


    x[ , .(handling_start,handling_stop,release_time)] %>% 
    hhmm_validator, 


    x[ , .(tarsus,P3, weight)] %>% 
        interval_validator(measures) ,


    x[ , .(ID,UL,LL,UR,LR,age,sex)] %>% 
    nchar_validator(Nchar), 
    
    x %>% 
        is.element_validator(preDefined1, reason = 'transponder does not exist!'), 

     x %>% 
        is.element_validator(preDefined2, reason = 'box does not exist!')


    )

    }


inspector.NESTS <- function(x) { list(

    x[, .(date_time, author, box, nest_stage)] %>% 
    is.na_validator, 

    x[ , .(date_time)]  %>% 
    POSIXct_validator,

    x[ , .(nest_stage)] %>% 
    is.element_validator(data.table(variable = 'nest_stage', 
    set = list(c('NOTA','WSP', 'U', 'LT','R','B','BC','C','LIN','E','Y')) )) , 

    x[ , .(nest_failed)] %>% 
    is.element_validator(data.table(variable = 'nest_failed', set =  list( c('R', 'P', 'D', 'H', 'U') ) )) , 

    x[ , .(author)] %>% 
    is.element_validator(data.table(variable = 'authors', set = list( 
    bibq('SELECT initials from AUTHORS UNION 
                        SELECT distinct initials from BTatWESTERHOLZ.AUTHORS' )$initials
     ) )) ,


    x[, .(box)] %>% 
    interval_validator(v = data.table(variable = 'box', lq = 1, uq = 277 ) ) ,


    x[, .(femaleLeft, warm_eggs,eggs_covered)] %>% 
    interval_validator(data.table(variable = c('femaleLeft', 'warm_eggs','eggs_covered') , 
    lq = 0, uq = 1 ) ) , 

    x[, .(eggs, chicks, age_chicks_processing,  collect_eggs, dead_eggs, dead_chicks)] %>% 
    interval_validator(  
    data.table(variable = c('eggs', 'chicks', 'age_chicks_processing',  'collect_eggs', 'dead_eggs', 'dead_chicks') , 
    lq = c(1,1, 13, 1, 1, 1), uq = c(14,14, 15, 15, 15, 15) )  ) , 

    x[, .(female_inside_box)] %>% 
     interval_validator( 
     data.table(variable = 'female_inside_box' , lq = 1, uq = 2 ) ), 

    x[, .(herbs, guessed)] %>%
    is.identical_validator(
    data.table(variable = c('herbs', 'guessed') , x = 1) ) 

    )}
