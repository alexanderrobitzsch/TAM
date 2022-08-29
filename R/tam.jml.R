## File Name: tam.jml.R
## File Version: 9.367


tam.jml <- function( resp, group=NULL, adj=.3, disattenuate=FALSE,
            bias=TRUE, xsi.fixed=NULL,  xsi.inits=NULL,  theta.fixed=NULL,
            A=NULL, B=NULL, Q=NULL, ndim=1,
            pweights=NULL, constraint="cases",
            verbose=TRUE, control=list(), version=3 )
{
    CALL <- match.call()
    #**** handle verbose argument
    args_CALL <- as.list( sys.call() )
    control$progress <- tam_args_CALL_search( args_CALL=args_CALL, variable="verbose",
                                default_value=TRUE )
    #*******
    if ( ! is.null(theta.fixed) ){
        version <- 1
    }
    if (! is.null(B)){
        if (dim(B)[3]>1){
            version <- 1
        }
    }

    #**** version=1
    if (version==1){
        constraint <- "cases"
        res <- tam_jml_version1( resp=resp, group=group, adj=adj,
                    disattenuate=disattenuate, bias=bias, xsi.fixed=xsi.fixed,
                    xsi.inits=xsi.inits, A=A, B=B, Q=Q, ndim=ndim,
                    theta.fixed=theta.fixed, pweights=pweights, control=control )
    }
    #**** version=2
    if (version>=2){
        res <- tam_jml_version2( resp=resp, group=group, adj=adj,
                    disattenuate=disattenuate, bias=bias, xsi.fixed=xsi.fixed,
                    xsi.inits=xsi.inits, A=A, B=B, Q=Q, ndim=ndim,
                    pweights=pweights, control=control, constraint=constraint,
                    version=version)
    }

    #- process item parameters
    res$AXsi <- tam_jml_compute_Axsi(A=res$A, xsi=res$xsi, resp=resp)
    #- item parameter table
    res$item1 <- res$item
    res$item <- tam_jml_itempartable( resp=resp, maxK=res$maxK, AXsi=res$AXsi,
                    B=res$B, resp.ind=res$resp.ind)
    #- theta summary
    res$theta_summary <- tam_jml_proc_abilities(theta=res$theta,
                                pweights=res$pweights, B=res$B)

    #- output
    res$CALL <- CALL
    res$resp <- resp
    res$constraint <- constraint
    res$bias <- bias
    return(res)
}
