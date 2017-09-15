## File Name: tam_mml_wle_proc_input_data.R
## File Version: 0.02
## File Last Change: 2017-06-01 13:22:34

tam_mml_wle_proc_input_data <- function(tamobj, score.resp)
{	
    B <- tamobj$B
    A <- tamobj$A
    nitems <- tamobj$nitems
    xsi <- tamobj$xsi
	if ( ! is.null(xsi) ){
		xsi <- xsi[,1]
	}
    AXsi <- tamobj$AXsi
    resp <- tamobj$resp
	resp.ind <- tamobj$resp.ind
	pweights <- tamobj$pweights
	pid <- tamobj$pid		
	
	#--- input via score.resp
	new_input <- FALSE
    if ( ! is.null( score.resp) ){
		resp <- score.resp
		new_input <- TRUE
    }
	#--- input via resp with missings
	if ( is.null(resp.ind) | new_input ){
		resp.ind <- 1 - is.na(resp)
	}
	resp[ is.na(resp) ] <- 0  
		
	#--- more values
	maxK <- ncol(AXsi)
	nitems <- nrow(AXsi)
	ndim <- dim(B)[3]
	nstud <- nrow(resp)
	if ( is.null(pid) ){
		pid <- 1:nstud		
	}
	if ( is.null(pweights) ){
		pweights <- rep(1,nstud)
	}	
	
	#--- OUTPUT
	res <- list(AXsi=AXsi, B=B, A=A, resp=resp, resp.ind=resp.ind,
				nitems=nitems, xsi=xsi, nstud=nstud, ndim=ndim, maxK=maxK,
				pweights=pweights, pid=pid)
	return(res)	
}
