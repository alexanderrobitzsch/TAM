## File Name: tam_packageinfo.R
## File Version: 0.01
## File Last Change: 2017-04-06 17:04:39

tam_packageinfo <- function(pack)
{
    d1 <- utils::packageDescription(pack)
	paste( d1$Package , " " , d1$Version , " (" , d1$Date , ")" , sep="")
}
