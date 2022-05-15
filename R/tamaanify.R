## File Name: tamaanify.R
## File Version: 9.233


#--- tamaanify function
tamaanify <- function( tammodel, resp, tam.method=NULL, doparse=TRUE )
{
    dat <- resp
    if ( doparse ){
        tammodel <- doparse( tammodel )
    }
    tammodel <- gsub( " ", "", tammodel )
    # tammodel <- tammodel[ substring( tammodel, 1,1) !="#"  ]
    tammodel <- gsub( ";", "\n", tammodel  )

    #*** split syntax into parts
    # tam1 <- strsplit( tammodel, split="\n")[[1]]
    tam1 <- unlist( strsplit( tammodel, split="\n") )
    tam1 <- tam1[ tam1 !="" ]
    tam1 <- tam1[ substring( tam1, 1, 1 ) !="#" ]
    # tam1 <- paste0( tam1, collapse="\n")
    tam1 <- data.frame( "index"=seq(1,length(tam1)),"syn"=tam1  )

    #***
    # identify markers
    markers <- c("LAVAANMODEL:", "ITEMTYPE:", "PRIOR:", "ANALYSIS:",
                "MODELCONSTRAINT:", "MODELPRIOR:")
    # skill space definitions

    tam1$part_begin <- 0
    m2 <- match( tam1$syn, markers )
    tam1[, "part_begin"] <- m2
    tam1[ is.na(tam1$part_begin ), "part_begin" ] <- 0
    tam1$section.label <- tam1$part_begin
    tam1$part_begin <- 1*( tam1$part_begin > 0 )
    tam1$part_begin <- cumsum( tam1$part_begin )
    tam1a <- paste0( ifelse( tam1$section.label==0, "  ", ""), tam1$syn )
    res <- list( "tammodel"=paste0( tam1a, collapse="\n") )
    res$tammodel.dfr <- tam1
    res$gammaslope.fixed <- NULL

    #*** process analysis
    res <- tamaanify_proc_analysis(res=res)

    #*** extract lavaan model
    res <- tamaanify_proc_lavaanmodel(res=res, resp=resp)

    #*** item characteristics
    res <- tamaanify_proc_items(res=res, resp=resp)

    #*** item type
    res <- tamaanify_proc_itemtype(res=res)

    #*** include model constraints
    res <- tamaanify_proc_modelconstraint(res=res)

    #*** add response dataset
    res <- tamaanify_proc_resp(res=res, resp=resp)

    #*** define design matrices and model for TAM
    res$method <- "tam.mml.2pl"
    if ( ! is.null(tam.method) ){
        res$method <- tam.method
    }

    #*** A matrix
    res <- tamaanify_create_A(res=res)

    #*** Q matrix
    res <- tamaanify_create_Q(res=res)

    #*** fixed loadings in tam.mml.2pl (B.fixed)
    res <- tamaanify.proc.loadings.B.fixed(res=res)

    #*** model constraints loadings
    res <- tamaanify.modelconstraints.loadings(res=res)

    #*** variance fixings
    res <- tamaanify_variance_fixed(res=res)

    #*** define design matrices for tam.mml.3pl method
    res <- tamaanify_tam_mml_3pl_designMatrices(res=res)

    #*** delta design matrix
    res <- tamaanify.tam.mml.3pl.deltadesign(res=res)

    #**** model prior
    res <- tamaanify_modelprior(res=res)

    #*** define method
    res <- tamaanify.define.method(res=res, tam.method=tam.method)

    #--- OUTPUT
    return(res)
}

