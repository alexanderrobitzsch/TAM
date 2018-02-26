## File Name: tam_mml_se_quick_verbose.R
## File Version: 0.01

tam_mml_se_quick_verbose <- function(pp, disp_progress, vv )
{
	if ( ( pp==disp_progress[vv] ) & ( vv <=10) ){
		cat("-") ; 
		utils::flush.console() 
		vv <- vv+1
	}
	return(vv)
}
