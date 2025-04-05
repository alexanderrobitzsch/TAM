## File Name: tam_linking_extract_parameters.R
## File Version: 0.152

tam_linking_extract_parameters <- function( tamobj, elim_items=NULL )
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
    if (is.null(items)){
        items <- dimnames(B)[[1]]
    }
    #--- distribution parameters
    res <- tam_linking_extract_parameters_trait_distribution(tamobj=tamobj)
    M <- res$M
    SD <- res$SD
    G <- res$G
    class_tamobj <- res$class_tamobj
    #--- linking items
    linking_items <- setdiff(items, elim_items)
    #--- OUTPUT
    res <- list(A=A, xsi=xsi, guess=guess, AXsi=AXsi, B=B, ndim=ndim, items=items,
                    elim_items=elim_items, linking_items=linking_items,
                    M=M, SD=SD, class_tamobj=class_tamobj, G=G)
    return(res)
}
