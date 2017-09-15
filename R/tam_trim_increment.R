## File Name: tam_trim_increment.R
## File Version: 0.14
## File Last Change: 2017-06-01 17:06:25

tam_trim_increment <- function(increment, max.increment, trim_increment="cut",
	trim_incr_factor = 2, eps = 1E-10, avoid_na=FALSE)
{
	abs_old_increment <- abs(max.increment)
	abs_increment <- abs(increment)
	if (trim_increment=="half"){
        ci <- ceiling( abs_increment / ( abs_old_increment + eps ) )
        increment <- ifelse( abs_increment > abs_old_increment , 
                             increment/(trim_incr_factor*ci), increment )			
	}		
	if (trim_increment=="cut"){	
        increment <- ifelse( abs_increment > abs_old_increment  , 
                             sign(increment) * max.increment , increment )	
	}
	if (avoid_na){
		increment <- ifelse( is.na(increment) , 0 , increment )
	}	
	return(increment)
}
