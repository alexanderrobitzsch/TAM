## File Name: tamaanify.proc.lavaanmodel.R
## File Version: 9.02
## File Last Change: 2017-01-24 17:13:52

############################################################
# process lavaan model
tamaanify.proc.lavaanmodel <- function(res , resp ){
    tam1 <- res$tammodel.dfr
	ind1 <- which( paste(tam1$syn) == "LAVAANMODEL:" )
	index1 <- tam1$part_begin[ ind1 ]
	lavmodel <- paste( tam1[ which( tam1$part_begin == index1 )[-1] , "syn" ] )
	lavmodel <- paste0( lavmodel , collapse="\n")
	res$LAVAANMODEL <- lavmodel
	# process lavaan model
	lavres <- lavaanify.IRT( lavmodel=res$LAVAANMODEL , data= resp )	
	res$lavpartable <- lavres$lavpartable	
    return(res)
		}
##########################################################		
