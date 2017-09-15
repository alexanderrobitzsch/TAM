## File Name: tam_mml_3pl_vec_add_increment.R
## File Version: 0.02
## File Last Change: 2017-01-24 18:13:51

tam_mml_3pl_vec_add_increment <- function( vec, h , index ){
	vec[index] <- vec[index] + h
	return(vec)
}	
