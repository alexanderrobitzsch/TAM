## File Name: tam_mml_mfr_proc_multiple_person_ids.R
## File Version: 0.12


tam_mml_mfr_proc_multiple_person_ids <- function(pid,tp, gresp, gresp.noStep,
        progress )
{
    persons <- sort( unique( pid ) )
    NP <- length( persons )
    person.ids <- sapply( persons, FUN=function( pp){ which( pid==pp ) },
                            simplify=FALSE)
    PP <- matrix( NA, nrow=NP, ncol=tp)
    for (pos in 1:tp){
        PP[,pos] <- unlist( lapply( person.ids, FUN=function(vv){ vv[pos] } ) )
    }

    gresp0 <- matrix( NA, nrow=NP, ncol=ncol(gresp) )
    colnames(gresp0) <- colnames(gresp)
    gresp0.noStep <- matrix( NA, nrow=NP, ncol=ncol(gresp.noStep) )
    colnames(gresp0.noStep) <- colnames(gresp.noStep)
    grespNA <- ( ! is.na( gresp ) )
    grespnoStepNA <- ( ! is.na( gresp.noStep ) )

    #-- check multiple rows
    m1 <- rowsum( 1-is.na(gresp.noStep), pid )
    h1 <- sum(m1>1)
    if (h1>0){
        cat("* Combinations of person identifiers and facets are not unique.\n")
        cat("* Use an extended 'formulaA' to include all \n")
        cat("  relevant facets and the argument 'xsi.setnull'.\n")
        cat("  See the help page of 'tam.mml' (?tam.mml) Example 10a.\n")
        stop()
    }
    for (pos in 1:tp){
        ind.pos <- which( ! is.na( PP[,pos]  ) )
        PP.pos <- PP[ind.pos,pos]
        g1 <- gresp[ PP.pos, ]
        g0 <- gresp0[ ind.pos, ]
        ig1 <- grespNA[ PP.pos, ]
        # * this check is time-consuming! release it to rcpp
        g0[ ig1 ] <- g1[ ig1 ]
        gresp0[ ind.pos, ] <- g0
        g1 <- gresp.noStep[ PP.pos, ]
        g0 <- gresp0.noStep[ ind.pos, ]
        ig1 <- grespnoStepNA[ PP.pos, ]
        g0[ ig1 ] <- g1[ ig1 ]
        gresp0.noStep[ ind.pos, ] <- g0
    }

    gresp0 -> gresp
    gresp0.noStep -> gresp.noStep
    pid <- persons
    if (progress){
        cat("    * Arranged Response Data with Multiple Person Rows   (",
                paste(Sys.time()), ")\n")
        utils::flush.console()
    }
    #--- OUTPUT
    res <- list(pid=pid, gresp=gresp, gresp.noStep=gresp.noStep)
    return(res)
}


#     cat("*** multiple persons lapply function" ) ; a1 <- Sys.time() ; print(a1-a0) ; a0 <- a1
