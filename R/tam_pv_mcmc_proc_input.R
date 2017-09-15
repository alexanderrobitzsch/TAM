## File Name: tam_pv_mcmc_proc_input.R
## File Version: 0.09
## File Last Change: 2017-08-17 17:23:17

tam_pv_mcmc_proc_input <- function( tamobj, group, Y )
{

	tam_class <- class(tamobj)
	# available classes
	tam_classes_available <- c( "tam.mml" , "tam.mml.3pl")	
	is_tam_class <- tam_class %in% tam_classes_available
	
	#** extract some elements
	person <- tamobj$person
	AXsi <- tamobj$AXsi
	AXsi[ is.na(AXsi ) ] <- -99	
	B <- tamobj$B
	guess <- tamobj$guess
	resp <- tamobj$resp
	resp.ind <- tamobj$resp.ind	
	pweights <- tamobj$pweights
	pid <- tamobj$pid
	D <- dim(B)[3]
	nstud <- nrow(resp)
	nitems <- nrow(AXsi)	
	#** is_tam_class==FALSE
	if ( ! is_tam_class ){
		#.. create person inits
		person <- tam_pv_mcmc_person_inits_from_resp( resp=resp, D=D )
		#.. process resp
		resp.ind <- 1 - is.na(resp)
		resp[ is.na(resp) ] <- 0
	}	
	
	#** more values used in tam.pv.mcmc function
	resp1 <- resp + 1	
	resp_ind_bool <- resp.ind == 1	
	maxK <- ncol(AXsi)
	if ( is.null(guess) ){
		guess <- rep(0,nitems)
	}
	if ( is.null(pweights) ){
		pweights <- rep(1,nstud)
	}	
	if ( is.null(pid) ){
		pid <- 1:nstud
	}

	#--- grouping variable
	if (is.null(group)){
		group <- rep(1,nstud)
	}
	groups <- sort(unique(group))
	G <- length(groups)
	group <- match( group, groups )	
	group_index <- list()
	N_groups <- rep(0,G)
	for (gg in 1:G){
		group_index[[gg]] <- which( group == gg )
		attr(group_index,"N_groups")[gg] <- v1 <- length(group_index[[gg]])		
	}	
	c2 <- cumsum( attr(group_index,"N_groups") )
	dfr <- data.frame("start"=c(1 , c2[-G] + 1 ), "end" = c2 )
	attr(group_index,"N_groups_cumsum") <- dfr

	if ( is.null(Y) ){
		Y <- matrix(1 , nrow=nstud, ncol=1)
	}	
	Y <- as.matrix(Y)
	
	#--- OUTPUT
	res <- list(person=person, is_tam_class=is_tam_class, resp=as.matrix(resp),
				AXsi=AXsi, B=B, guess=guess, resp.ind= as.matrix(resp.ind), 
				pweights=pweights, nitems=nitems, nstud=nstud,
				maxK=maxK, D=D, pid=pid, group=group, G=G, groups=groups,
				group_index=group_index, Y=Y)
	return(res)
}
