## File Name: tam_NA_pattern.R
## File Version: 0.04

###################################################################
# Function for defining different missing response patterns
tam_NA_pattern <- function(x)
{
	x <- is.na(x)
	misspattern <- tam_01_pattern(x=x)
    return( misspattern )
}
