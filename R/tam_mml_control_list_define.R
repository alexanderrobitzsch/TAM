## File Name: tam_mml_control_list_define.R
## File Version: 0.11

tam_mml_control_list_define <- function(control, envir, tam_fct,
		prior_list_xsi)
{
	con <- list()
	
	#--- set default values	
    con$nodes <- seq(-6,6,len=21)
	con$snodes <- 0 
	con$QMC <- TRUE
    con$convD <- 1E-3 
	con$conv <- 1E-4
	con$convM <- 1E-4
	#* Msteps
	if ( tam_fct %in% c("tam.mml.mfr","tam.mml.3pl")){
		con$Msteps <- 10
	} else {
		con$Msteps <- 4
	}
    con$maxiter <- 1000 
	con$max.increment <- 1 
    con$min.variance <- 1E-3
	con$progress <- TRUE
	con$ridge <- 0
    con$seed <- NULL
	#* xsi.start0
	if ( tam_fct %in% c("tam.mml.mfr")){
		con$xsi.start0 <- 0
	} else {
		con$xsi.start0 <- FALSE
	}
	con$increment.factor <- 1 
	con$fac.oldxsi <- 0
	con$acceleration <- "none" 
	con$dev_crit <- "absolute"
	con$trim_increment <- "half"
	#* maxgamma
	if ( tam_fct %in% c("tam.mml.3pl")){
		con$maxgamma <- 9.99
	}
	#* mstep_intercept_method
	if ( tam_fct %in% c("tam.mml" , "tam.mml.mfr")){
		con$mstep_intercept_method <- "R"
		if ( ! is.null( prior_list_xsi) ){
			con$mstep_intercept_method <- "optim"
		}
	}
	
	#-- overwrite default values if supplied by the user
    con[ names(control) ] <- control  	
	#-- adjust values of fac.oldxsi
	con$fac.oldxsi <- max( 0 , min( c( con$fac.oldxsi , .95 ) ) )  		
	#-- adjust progress
	if ( con$progress == "F" ){ con$progress <- FALSE }
	if ( con$progress == "T" ){ con$progress <- TRUE }
	#-- copy lists
    con1a <- con1 <- con	
	#-- assign elements
	res <- tam_assign_list_elements(con, envir=envir)
	#-- output
	res <- list(con=con, con1a=con1a)
    return(res)
}
