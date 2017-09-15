## File Name: tam_NA_pattern.R
## File Version: 0.04
## File Last Change: 2017-06-04 17:31:06

###################################################################
# Function for defining different missing response patterns
tam_NA_pattern <- function(x)
{
	x <- is.na(x)
	misspattern <- tam_01_pattern(x=x)
    return( misspattern )
}
