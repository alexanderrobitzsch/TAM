## File Name: predict.tam.R
## File Version: 9.03
## File Last Change: 2017-02-18 22:27:48

##################################################
# predict methods in TAM
predict.tam.mml <- function( object , ... ){
      res <- IRT.predict( object , object$resp )
	  return(res)
}
##################################################	
predict.tam.mml.3pl <- predict.tam.mml
predict.tamaan <- predict.tam.mml
##################################################
