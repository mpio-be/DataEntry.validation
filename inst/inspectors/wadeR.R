#' ==========================================================================
#' validators used by wadeR package. 
#' require(wadeR)
#' require(glue)
#' require(stringr)
#' require(foreach)
#' require(DataEntry.validation)
#' 
#' 
#' tabs = c('CAPTURES', 'RESIGHTINGS', 'NESTS', 'EGGS_CHICKS_field', 'DEVICES')
#' 
#' o = foreach(i = 1:length(tabs), .combine = rbind) %do% {
#' 
#'  x = wadeR::idbq( glue('select * from {tabs[i]} ') )
#'  class(x) = c(class(x), tabs[i] )
#'  
#'  inspector_loader(package = 'wadeR')
#'  
#'  ii = inspector(x)
#'  evalidators(ii)
#'  
#'  }
#' 
#'
#' ==========================================================================



inspector.CAPTURES <- function(x, ...){
  x[ , rowid := .I]

  list(

  # GENERAL #################################  
    # Mandatory to enter
    x[, .(date_, form_id, author, gps_id, gps_point, ID, recapture, capture_meth, weight, blood_dna)] %>% 
    is.na_validator,

    x[recapture == 0, .(tarsus, culmen, total_head, wing)]  %>% 
    is.na_validator("Mandatory at first capture"),


    # Correct format?
    x[, .(date_)]  %>% POSIXct_validator(),

    x[, .(caught_time, released_time)]  %>% hhmm_validator(),

    x[ , .(author)] %>% 
    is.element_validator(v = data.table(variable = "author", 
    set = list(idbq("SELECT initials ii FROM AUTHORS")$ii   )  )),


    x[ , .(gps_id)]   %>% 
    interval_validator( v = data.table(variable = "gps_id",    lq = 1, uq = 13),     
    "GPS ID not in use" ),

    x[, .(gps_point)]  %>% 
    interval_validator( v = data.table(variable = "gps_point", lq = 1, uq = 999),     
    "GPS waypoint is over 999?" ),

    # Entry would be duplicate
    x[recapture == 0 & !is.na(ID), .(ID)] %>% is.duplicate_validator(v = data.table(variable = "ID", 
        set = list( c(str_sub(idbq("SELECT ID FROM CAPTURES")$ID, -5),70101:70427   )  ) ), "
                  Metal band already in use! Recapture?" ),

    x[, .( UL, LL, UR, LR, recapture)] %>% 
    combo_validator(validSet  = 
        c(idbq('select CONCAT(UL, "-", LL, "|",UR, "-", LR) combo FROM CAPTURES' )$combo,
          idbq('select CONCAT(UL, "-", LL, "|",UR, "-", LR) combo FROM FIELD_2017_REPHatBARROW.CAPTURES' )$combo,
          idbq('select CONCAT(UL, "-", LL, "|",UR, "-", LR) combo FROM FIELD_2018_REPHatBARROW.CAPTURES' )$combo
          ), "Color Combo already in use (in CAPTURES)! Recapture?"),

    # x[, .(start_capture, caught_time)] %>% 
    # time_order_validator(time1 = 'start_capture', time2 = 'caught_time', time_max = 60),

    #x[, .(caught_time, bled_time)] %>% 
    #time_order_validator(time1 = 'caught_time', time2 = 'bled_time', time_max = 60),

    x[, .(caught_time, released_time)] %>% 
    time_order_validator(time1 = 'caught_time', time2 = 'released_time', time_max = 60),

    x[!is.na(nest), .(nest)] %>% 
    is.element_validator(v = data.table(variable = "nest", set = list(idbq("SELECT * FROM NESTS")$nest  ) ), 
    "Nest not found in NESTS!" ),


  # SPECIES SPECIFIC: REPH ##################  
    x[species == 'REPH', .( UL, LL, UR, LR, recapture = 1)]  %>%  
    combo_validator(validSet = colorCombos() , 'Combo not within the valid set of combinations?'),
  
    x[species == 'REPH', .(sex)]  %>% 
    is.element_validator(v = data.table(variable = "sex", set = list(c("M", "F"))  )),
  
    x[species == 'REPH', .(LL, LR, UL, UR, cam_id, haema, behav)] %>% 
    is.na_validator,

    x[sex == 2 & species == 'REPH', .(carries_egg)] %>% 
    is.na_validator("Mandatory for females"),


    x[species == 'REPH', .(ID)] %>% 
    is.element_validator(v = data.table(variable = "ID", 
    set = list(c(70001:71000, 45001:46000)) ), "Wrong REPH band" ),


    # Entry should be within specific interval
    x[species == 'REPH', .(tarsus)]     %>% 
    interval_validator( v = data.table(variable = "tarsus", lq = 20, uq = 24 ),   
    "Measurement out of typical range" ),

    x[species == 'REPH', .(culmen)] %>% 
    interval_validator( v = data.table(variable = "culmen",     lq = 20, uq = 25 ),   
    "Measurement out of typical range" ),

    x[species == 'REPH', .(total_head)] %>% 
    interval_validator( v = data.table(variable = "total_head", lq = 43.5, uq = 50 ),
    "Measurement out of typical range" ),

    x[species == 'REPH', .(wing)]       %>% 
    interval_validator( v = data.table(variable = "wing",       lq = 128, uq = 145 ),
    "Measurement out of typical range" ),

    x[species == 'REPH', .(weight)]     %>% 
    interval_validator( v = data.table(variable = "weight",   lq = 40, uq = 72 ),   
    "Measurement out of typical range" )


  )


  }


inspector.RESIGHTINGS <- function(x, ...){
  x[ , rowid := .I]

  list(

  # GENERAL #################################   
    x[, .(author, gps_id, gps_point)] %>% 
    is.na_validator,

    x[, .(LR)]%>% 
    is.na_validator( "Ring combo, NOBA or COBA mandatory" ),


    x[, .(behav_cat)] %>% 
    is.na_validator("Resighting or capture?"),


    x[ , .(author)] %>% 
    is.element_validator(v = data.table(variable = "author", 
    set = list(idbq("SELECT initials ii FROM AUTHORS")$ii   )  )),

    x[, .(sex)]       %>% 
    is.element_validator(v = data.table(variable = "sex",      
    set = list(c("M", "F"))  )),

    x[, .(gps_id)]  %>% 
    interval_validator( v = data.table(variable = "gps_id",     lq = 1, uq = 20),  
    "GPS ID not found?" ),

    x[, .(gps_point)] %>% 
    interval_validator( v = data.table(variable = "gps_point",  lq = 1, uq = 999), 
    "GPS waypoint is over 999?" ),

    x[, .( UL, LL, UR, LR, recapture = 1)] %>% 
    combo_validator( validSet  = c(idbq('select CONCAT(UL, "-", LL, "|",UR, "-", LR) combo FROM CAPTURES' )$combo,
              idbq('select CONCAT(UL, "-", LL, "|",UR, "-", LR) combo FROM FIELD_2017_REPHatBARROW.CAPTURES' )$combo,
              idbq('select CONCAT(UL, "-", LL, "|",UR, "-", LR) combo FROM FIELD_2018_REPHatBARROW.CAPTURES' )$combo
              ), "Color combo does not exist in CAPTURES" ), 

  # SPECIES SPECIFIC: REPH ##################  

    x[species == 'REPH', .(habitat)] %>% 
    is.na_validator( "No habitat? Please remember to note it" ),


    x[species %in% c('REPH', 'PESA'), .(sex)] %>% 
    is.na_validator("Sex not identified?"),


    x[species == 'REPH', .(habitat)]   %>% 
    is.element_validator(v = data.table(variable = "habitat",  
    set = list(c("W", "G"))  )),

    x[species == 'REPH', .(aggres)]    %>% 
    is.element_validator(v = data.table(variable = "aggres",   
    set = list(c("D", "D0", "D1", "F","F0", "F1", "B", "B0", "B1", "O", "O0","O1", NA, ""))  )),

    x[species == 'REPH', .(displ)]     %>% 
    is.element_validator(v = data.table(variable = "displ",    
    set = list(c("K", "K0", "K1", "F", "F0", "F1", "P", "P0", "P1", NA, ""))  )),

    x[species == 'REPH', .(cop)]       %>% 
    is.element_validator(v = data.table(variable = "cop",      
    set = list(c("S", "S0", "S1", "A", "A0", "A1", NA, ""))  )),

    x[species == 'REPH', .(flight)]    %>% 
    is.element_validator(v = data.table(variable = "flight",   
    set = list(c("F", "F0", "F1","C", "C0", "C1", "CF", "CF0", "CF1", NA, ""))  )),

    x[species == 'REPH', .(voc)]       %>% 
    is.element_validator(v = data.table(variable = "voc",      
    set = list(c("Y", "N", NA, ""))  )),

    x[species == 'REPH', .(maint)]     %>% 
    is.element_validator(v = data.table(variable = "maint",    
    set = list(c("F", "R", "P", "A", "BW", NA, "", "F,P", "P,F", "F,R", "F,A", "P,A", "O"))  )),

    x[species == 'REPH', .(spin)]      %>% 
    is.element_validator(v = data.table(variable = "spin",     
    set = list(c("C", "AC", "B", NA, ""))  )),


    x[!is.na(min_dist) && species == 'REPH', .(min_dist)] %>%
    interval_validator(v = data.table(variable = "min_dist",   lq = 0, uq = 25 ), 
    "Other individuals more than 25 m away? - Individuals really together?" )


  )

  }


inspector.NESTS <- function(x, ...){  
  x[ , rowid := .I]

  list(
  
    # GENERAL #################################  

    x[ , .(date_)]     %>% POSIXct_validator     , 
    
    x[ , .(time_appr)] %>% hhmm_validator      , 
    
    x[ , .(time_left)] %>% hhmm_validator       , 

    x[, .(author, nest, date_, time_appr,  nest_state)]  %>% 
    is.na_validator("Mandatory entry is missing!"), 
    
    x[, .(time_left)] %>% 
    is.na_validator("Please remember to note the time you leave the nest!"), 
 

    x[, .(time_appr, time_left)]  %>% 
    time_order_validator( time1 = 'time_appr', time2 = 'time_left', time_max = 60), 


    x[ , .(author)] %>% 
    is.element_validator(v = data.table(variable = "author", 
    set = list(idbq("SELECT initials ii FROM AUTHORS")$ii   )  ))  ,

    x[ , .(nest_state)] %>% 
    is.element_validator(v = data.table(variable = "nest_state",    
    set = list(c("F", "C", "I", "pP", "P", "pD", "D", "H", "notA"))  ))  , 

    x[nest_state == "F" | nest_state == "C", .(clutch_size)]  %>% 
    is.na_validator("Clutch size is missing?")  , 
  
    x[nest_state == "F", .(nest)] %>% 
    is.duplicate_validator(v = data.table(variable = "nest", 
      set = list(idbq("SELECT nest FROM NESTS")$nest  ) ), 
    "Nest is already assigned! Nest number given twice or nest_state is not F?" )  , 
  
    x[nest_state != "F", .(nest)] %>% 
    is.element_validator(v = data.table(variable = "nest", 
    set = list( 
        c( idbq("SELECT nest FROM NESTS WHERE nest_state = 'F' ")$nest, x[nest_state == "F"]$nest )
        ) 
     ), "Nest does not exist in NESTS as found!, first time nest is found should be entered as F" )  , 
  

    x[ , .(m_behav)] %>% 
    is.element_validator( v = data.table(variable = "m_behav",
    set = list(c("INC", "DF", "BW", "O", "INC,DF", "INC,O", "INC,BW"))  )), 

    x[, .(gps_id)]  %>% 
    interval_validator( v = data.table(variable = "gps_id",  lq = 1, uq = 13), 
     "GPS ID not found?" ), 

    x[, .(gps_point)] %>% 
    interval_validator( v = data.table(variable = "gps_point",  lq = 1, uq = 999), 
    "GPS waypoint is over 999?" ), 

    x[!is.na(clutch_size), .(clutch_size)]  %>% 
    interval_validator(  v = data.table(variable = "clutch_size", lq = 0, uq = 4 ),  
    "No eggs or more than 4?" ), 


    x[, .(nest)]  %>% 
    is.element_validator(  v = data.table(variable = "nest", 
      set = list( 
        c(paste0("R", 101:999), 
          paste0("N", 101:999), 
          paste0("P", 101:999), 
          paste0("S", 101:999), 
          paste0("L", 101:999)  ) ), 
    "Nest ID does not exist, in wrong format or GPS ID > 13!") ), 

  # SPECIES SPECIFIC: REPH ##################  

    x[!is.na(msr_id) && nest2species(nest)== 'REPH' , .(msr_state)]  %>% 
    is.na_validator("MSR state is missing?"), 

    x[!is.na(msr_state) && nest2species(nest)== 'REPH', .(msr_id)]    %>% 
    is.na_validator("MSR ID is missing!") , 


    x[nest2species(nest)== 'REPH' , .(f_behav)] %>% 
    is.element_validator( v = data.table(variable = "f_behav", 
    set = list(c("INC", "DF", "BW", "O", ""))  )), 

    x[nest2species(nest)== 'REPH'  , .(msr_state)]    %>% 
    is.element_validator( v = data.table(variable = "msr_state",     
    set = list(c("ON", "OFF", "DD", NA, ""))  )), 



    x[!is.na(male_id) && nest2species(nest)== 'REPH', .(male_id)]  %>% 
    is.element_validator(  v = data.table(variable = "male_id",   
      set = list(  c(str_sub(idbq("SELECT ID FROM CAPTURES")$ID, -5), 
        idbq("SELECT ID FROM FIELD_2017_REPHatBARROW.CAPTURES")$ID,
        idbq("SELECT ID FROM FIELD_2018_REPHatBARROW.CAPTURES")$ID
        )  ) ), "Metal ID not in in CAPTURES!" ), 

    x[!is.na(female_id) && nest2species(nest)== 'REPH', .(female_id)] %>% 
    is.element_validator( v = data.table(variable = "female_id", 
      set = list(  str_sub(idbq("SELECT ID FROM CAPTURES")$ID, -5)  ) ), 
    "Metal ID not in CAPTURES!" ), 

    x[(!is.na(msr_id) | nchar(msr_id) > 0 )&& nest2species(nest)== 'REPH', .(msr_id)] %>% 
    is.element_validator( v = data.table(variable = "msr_id", 
     set = list(idbq("SELECT device_id FROM DEVICES")$device_id)  ), 
    "MSR ID is not existing in DEVICES!" ) 


    )





  }


inspector.EGGS_CHICKS_field <- function(x, ...){
  x[ , rowid := .I]

  list(
  x[, .(nest, arrival_datetime, float_height, float_angle)] %>% 
  is.na_validator,

  x[ , .(arrival_datetime)] %>% 
  POSIXct_validator,

  x[, .(float_angle)] %>% 
  interval_validator(v = data.table(variable = "float_angle", lq = 21, uq = 89),  "Angle has to be between 21 and 89 for the formula" ),

  x[, .(nest)] %>% 
  is.element_validator(v = data.table(variable = "nest", set = list(idbq("SELECT * FROM NESTS")$nest  ) ), "Nest does not exists in NESTS!" )
  


  )
  }


inspector.DEVICES <- function(x, ...){
  x[ , rowid := .I]
  
  list(
  x[, .(device, device_id)] %>% is.na_validator
  )


  }



