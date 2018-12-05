## File Name: zzz.R
## File Version: 9.09
#  zzz.R
#
# This function is simply copied from mice package.

#------------------------------.onLoad-------------------------------
#.onLoad <- function(...){
#  d <- packageDescription("TAM")
#  cat("\n............................\n")
#  packageStartupMessage(paste(d$Package," ", d$Version," (",d$Date,")",sep=""))
#  cat("............................\n")
#  return()
#}
version <- function(pkg="TAM"){
    lib <- dirname(system.file(package=pkg))
    d <- utils::packageDescription(pkg)
    return(paste(d$Package,d$Version,d$Date,lib))
}
# on attach TAM
.onAttach <- function(libname,pkgname){
    d <- utils::packageDescription("TAM")
    packageStartupMessage(  # "::...........................::\n",
            paste0("* ", d$Package," ", d$Version," (",d$Date,")" ) )
}

xx <- function(f1=1, f2=1)
{
    v1 <- paste0( rep(" ",f1), collapse="" )
    v2 <- paste0( rep(" ",f2), collapse="" )
    res <- paste0( v1, "=", v2)
    return(res)
}
