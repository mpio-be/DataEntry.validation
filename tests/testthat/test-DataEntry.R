

context("Functions for validation")

    test_that("validators works as expected", {

        # validators should return 3 columns:
        VN = c("rowid", "variable", "reason")

        # is.na    
        x = data.table(v1 = c(1,2, NA, NA), v2  = c(1,2, NA, NA) )
        expect_identical( names(is.na_validator(x)), VN)

        #POSIXct
        x = data.table(
        v1 = c(NA, NA, as.character(Sys.time() - 3600*24*10 )  ) ,
        v2 = c('2016-11-23 25:23', as.character(Sys.time() -100) ,as.character(Sys.time()+100)))
        o = POSIXct_validator(x)

        expect_identical( names(is.na_validator(x)), VN)
        expect_equal(nrow(o), 2)

        # hh:mm
        o = hhmm_validator( data.table(v1 = c('02:04' , '16:56'), v2= c('24:04' , NA)))
        expect_identical( names(is.na_validator(x)), VN)
        expect_equal(nrow(o), 1)

        # datetime
        o = datetime_validator( data.table(v1 = c('2017-07-27 00:00' , '2017-01-21')) )
        expect_identical( names(is.na_validator(x)), VN)
        expect_equal(nrow(o), 1)

        # time-order
        x = data.table(v2 = c('10:10' , '16:30', '02:08'  ), v1 = c('10:04' , '16:40', '01:08'  ) )
        o = time_order_validator(x, 'v1', 'v2')
        expect_identical( names(is.na_validator(x)), VN)
        expect_equal(nrow(o), 1)

        # interval
        x = data.table(box = c(0, 1, 100, 300))
        v = data.table(variable = 'box', lq = 1, uq = 277 )
        o = interval_validator(x,v)
        expect_identical( names(is.na_validator(x)), VN)
        expect_equal(nrow(o), 2)

        # nchar
        x = data.table(v1 = c('x', 'xy', 'x')  , v2 = c('xx', 'x', 'xxx')  )
        v = data.table(variable = c('v1', 'v2'), n = c(1, 2) )
        o = nchar_validator(x, v)
        expect_identical( names(is.na_validator(x)), VN)
        expect_equal(nrow(o), 3)

        #is.element
        x = data.table(v1 = c('A', 'B', 'C')  , v2 = c('ZZ', 'YY', 'QQ')  )
        v = data.table(variable = c('v1', 'v2'), 
        set = c( list( c('A', 'C') ), list( c('YY')  )) )
        o = is.element_validator(x, v)
        expect_equal(nrow(o), 3)

        # is.duplicate
        x = data.table(v1 = c('A', 'B', 'C')  , v2 = c('ZZ', 'YY', 'QQ')  )
        v = data.table(variable = c('v1', 'v2'), 
        set = c( list( c('A', 'C') ), list( c('YY')  )) )
        o = is.duplicate_validator(x, v)
        expect_equal(nrow(o), 3)

        #is.identical
        x = data.table(v1 = 1:3  , v2 = c('a', 'b', 'c')  )
        v = data.table(variable = c('v1', 'v2'),  x = c(1, 'd'))
        o = is.identical_validator(x, v)
        expect_equal(nrow(o), 5)

        #combo_validator
        x = data.table(UL = c('M', 'M')  , LL = c('G,DB', 'G,P'), 
        UR = c('Y', 'Y'), LR = c('R', 'G'), recapture = 1 )
        o =  combo_validator(x, validSet  = list("M-G,DB|Y-R") )
        expect_equal(nrow(o), 1)

        # is.regexp_validator
        x = data.table(id = c("x2-011-05-19", "x2-011-05-2019", "x2-011-5-2019", "x2-011-  5-2019") )
        o = is.regexp_validator(x, regexp = "^x[1-9]-\\d{3}-\\b(?:05|09|11)\\b-19$")
        expect_equal(nrow(o), 3)

        })


    test_that("validators working on subset", {
        
        # expect "rowid is missing from x" when x does not have rowid
        x = data.table(v1 = c(1,1, NA, NA, 1, 1, 1), v2  = c(1,1, 1, NA, 1, 1,1) )
        expect_message( is.na_validator(x[1:3, ]) )

        # without rowid output is wrong on subset
        o = is.na_validator(x[c(1,3,5), ])
        expect_false(o$rowid == 3 )

        # with rowid output is correct on subset
        x[, rowid := .I]
        o = is.na_validator(x[c(1,3,5), ])
        expect_true(o$rowid == 3 )




        })