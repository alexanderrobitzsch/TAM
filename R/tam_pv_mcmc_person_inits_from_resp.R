## File Name: tam_pv_mcmc_person_inits_from_resp.R
## File Version: 0.04

tam_pv_mcmc_person_inits_from_resp <- function( resp, D, adj = .02 , sd_noise = .5)
{
    score <- rowSums( resp , na.rm=TRUE )
    max_ki <- apply( resp , 2 , max, na.rm=TRUE )
    nstud <- nrow(resp)
    nitems <- ncol(resp)
    maxscore <- rowSums( ( 1-is.na(resp) ) * tam_matrix2( max_ki , nrow=nstud, ncol=nitems) , na.rm=TRUE )
    propscore <- score / maxscore
    propscore <- ( propscore + adj / 2) / ( 1 + adj )
    logit_score <- stats::qlogis( propscore )
    person <- matrix( logit_score , nrow=nstud , ncol=D )
    colnames(person) <- paste0("EAP.Dim" , 1:D )
    person <- person + matrix( stats::rnorm( nstud * D , sd = sd_noise ) , ncol=D)
    return(person)
}
