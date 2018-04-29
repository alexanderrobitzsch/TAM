## File Name: tam_mml_mfr_collect_xsi_parameters.R
## File Version: 0.02

tam_mml_mfr_collect_xsi_parameters <- function( xsi.constr, resp, A, xsi, se.xsi ,
        delete.red.items, itemnames, miss.items )
{
    xsiFacet <- as.data.frame( (xsi.constr$xsi.table)[,1:2]    )
    obji <- data.frame( "parameter" = dimnames(A)[[3]] ,
                        "xsi"=xsi , "se.xsi"=se.xsi )
    rownames(obji) <- paste(obji$parameter)
    rownames(xsiFacet) <- paste( xsi.constr$xsi.table[,1] )

    xsi1 <- merge( x = xsiFacet , y= obji , by="parameter" , all=TRUE )
    A1 <- xsi.constr$xsi.constraint %*% xsi

    incl <- match( rownames(xsi.constr$xsi.constraint) , xsi1$parameter)
    xsi1[ incl , "xsi" ] <- A1

    xsi1 <- xsi1[ match( xsiFacet$parameter , xsi1$parameter) , ]
    xsi.facets <- xsi1
    rownames(xsi.facets) <- NULL
    i1 <- grep( "Intercept" , xsi.facets$parameter)

    if ( length(i1) > 0 ){
      xsi.facets <-  xsi.facets[ - i1 , ]
    }

    XX <- xsi.constr$xsi.constraint
    incl <- match( rownames(XX) , xsi.facets$parameter)
    vcov_xsi <- diag( obji$se.xsi^2 )
    se2 <- sqrt( diag( XX %*% vcov_xsi %*% t(XX) ))
    xsi.facets[ incl , "se.xsi"] <- se2

    #*** control xsi.facets
    if( xsi.constr$intercept_included ){
        ind <- which( paste(xsi.facets$facet) == "item" )
        n1 <- length(ind)
        if ( n1 > 0 ){
            itemc <- itemnames[n1]
            itemo <- paste0("item" , n1 )
            g1 <- which( paste(xsi.facets$parameter) == itemo )
            if ( length(g1) > 0 ){
                xsi.facets$parameter[g1] <- itemc
            }
            g1 <- grep( paste0(itemo , ":") , paste(xsi.facets$parameter)  )
            if ( length(g1) > 0 ){
                xsi.facets$parameter <- gsub( paste0(itemo , ":") , paste0(itemc , ":")  ,
                                            paste(xsi.facets$parameter) )
            }
            g1 <- grep( paste0(itemo , "-") , dimnames(A)[[1]]  )
            if ( length(g1) > 0 ){
                dimnames(A)[[1]] <- gsub( paste0(itemo , "-") , paste0(itemc , "-")  ,
                                    dimnames(A)[[1]] )
            }
        }
    }
    #--- tidy xsi
    xsi <- obji[,-1]
    rownames(xsi) <- dimnames(A)[[3]]
    if(delete.red.items){
        resp <- resp[,-miss.items]
    }
    colnames(resp) <- dimnames(A)[[1]]
    #--- OUTPUT
    res <- list(resp=resp, xsi=xsi, xsi.facets=xsi.facets)
    return(res)
}
