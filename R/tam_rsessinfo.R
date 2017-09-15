## File Name: tam_rsessinfo.R
## File Version: 0.01
## File Last Change: 2017-04-06 16:59:30


#***********************************************
# session info
tam_rsessinfo <- function(){
    si <- Sys.info()
    si2 <- utils::sessionInfo()
    paste0( si2$R.version$version.string , " " , si2$R.version$system 
             , " | nodename = " , si["nodename"] , " | login = " , si["login"] )
}
#************************************************

