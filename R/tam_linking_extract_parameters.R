## File Name: tam_linking_extract_parameters.R
## File Version: 0.08

tam_linking_extract_parameters <- function( tamobj )
{

    #--- item parameters
    A <- tamobj$A
    xsi <- tamobj$xsi$xsi
    guess <- tamobj$guess
    if (is.null(guess)){
        guess <- rep(0, dim(A)[1] )
    }
    AXsi <- tamobj$AXsi
    B <- tamobj$B
    names(guess) <- dimnames(A)[[1]] <- rownames(AXsi) <- dimnames(B)[[1]]
    ndim <- dim(B)[3]
    items <- colnames(tamobj$resp)
    #--- distribution parameters
    res <- tam_linking_extract_parameters_trait_distribution(tamobj=tamobj)
    M <- res$M
    SD <- res$SD
    G <- res$G
    class_tamobj <- res$class_tamobj
    #--- OUTPUT
    res <- list(A=A, xsi=xsi, guess=guess, AXsi=AXsi, B=B, ndim=ndim, items=items, M=M, SD=SD ,
                    class_tamobj=class_tamobj, G=G)
    return(res)
}
