## File Name: tam_packageinfo.R
## File Version: 0.04
## File Last Change: 2017-09-16 13:41:24

tam_packageinfo <- function(pack)
{
    d1 <- utils::packageDescription(pkg=pack)
	paste( d1$Package , " " , d1$Version , " (" , d1$Date , ")" , sep="")
}
