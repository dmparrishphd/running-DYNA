# RUNNING AVERAGES OVER A TIME SERIES

# SET UP AN EXTENDED DATA FRAME WITH INTERMEDIATE CALCULATIONS
# THAT SPECIFY THE TIMES TO INCLUDE IN THE RUNNING AVERAGE(S)
augment <- function ( X , back , fwd = back) {
	INFO <- data.frame ( X , xmin = X [ , 1 ] - back , xmax = X [ , 1 ] + fwd )
	FUN <- function ( i ) {
		MIN <- INFO [ i , "xmin" ]
		MAX <- INFO [ i , "xmax" ]
		range ( which (
			INFO [ , 1 ] >= MIN &
			INFO [ , 1 ] <= MAX ) ) }
	n <- nrow ( INFO )
	i <- seq_len ( n )
	RANGE <- vapply ( i , FUN , double(2) )
	data.frame ( INFO ,
		xfrom = RANGE [ 1 , ] ,
		xto   = RANGE [ 2 , ] ) }

# EXTRACT THE PORTION OF THE AUGMENTED DATA FRAME THAT CONTAINS THE DATA
# TO USE FOR THE AVERAGES
rextract <- function ( X , i ) X [ , 1:2 ] [ seq (
	from = X [ i , "xfrom" ] ,
	to = X [ i , "xto" ] ) , ]

# COMPUTE RUNNING "AVERAGES" BASED ON THE DATA IN THE VICINITY OF
# GIVEN POINTS
running <- function (
		INFO ,
		FUNS = list () ) {
	n <- nrow ( INFO )
	inner <- function ( k ) vapply (
		X = seq_len ( n ) ,
		FUN.VALUE = double ( 1 ) ,
		USE.NAMES = FALSE ,
		FUN = function ( row )
			FUNS [[ k ]] ( rextract ( INFO , row ) ) )
	vapply (
		X = seq_along ( FUNS ) ,
		FUN = inner ,
		FUN.VALUE = double ( n ) ,
		USE.NAMES = FALSE ) }


		DATES <- seq (
			from = as.Date ( "1980-01-01" ) ,
			to = as.Date ( "2022-01-01" ) ,
			by = "month" )
		
		DF <- data.frame ( x = DATES , y = rnorm ( length ( DATES ) ) )
		plot(DF, col="#00000080", pch = 16, cex = 2)

		X <- augment ( DF , back = 366 )
head ( X )
		rextract ( X , 1 )
		median. <- function ( X ) median ( X [ , 2 ] )
		running ( X , list ( median. ) )
		plot( X [, 1:2] )
		lines ( X [ , 1 ] , running ( X , list ( median. ) ) )

