## File Name: tamaanify_create_Q.R
## File Version: 9.12



#--- create Q matrix
tamaanify_create_Q <- function( res )
{
    lavpartable <- res$lavpartable
    resp <- res$resp
    ind <- which( lavpartable$op=="=~" )
    #*** set some values in ustart
    lv1 <- lavpartable[ ind, ]
    lv1[ is.na(lv1$ustart), "ustart" ] <- 1
    lavpartable[ ind, ] <- lv1

    facs <- unique( paste( lavpartable$lhs[ind] ))
    NF <- length(facs)
    items <- colnames(resp)
    I <- length(items)
    Q <- matrix( 0, nrow=I, ncol=NF)
    rownames(Q) <- items
    colnames(Q) <- facs
    for (ff in 1:NF){
        ind.ff <- intersect( which( paste(lavpartable$lhs)==facs[ff] ), ind )
        lff <- lavpartable[ ind.ff, ]
        Q[ rownames(Q) %in% paste(lff$rhs), ff ] <- lff$ustart
    }
    res$Q <- Q
    res$lavpartable <- lavpartable
    return(res)
}

