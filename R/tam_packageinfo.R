
tam_packageinfo <- function(pack)
{
    d1 <- utils::packageDescription(pack)
	paste( d1$Package , " " , d1$Version , " (" , d1$Date , ")" , sep="")
}
