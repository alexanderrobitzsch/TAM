## File Name: tam_packageinfo.R
## File Version: 0.05

tam_packageinfo <- function(pack)
{
    d1 <- utils::packageDescription(pkg=pack)
    paste( d1$Package , " " , d1$Version , " (" , d1$Date , ")" , sep="")
}
