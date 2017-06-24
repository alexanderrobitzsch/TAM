

###########################################################
tam_calc_exp <- function( rprobs , A , np , est.xsi.index , itemwt ,
	indexIP.no , indexIP.list2 , Avector )
{
	CC <- dim(rprobs)[2]
	TP <- dim(rprobs)[3]
	NXSI <- dim(A)[3]
	NI <- dim(A)[1]
	rprobsL1 <- as.vector( rprobs )
    AL1 <- Avector
	rprobsL1 <- redefine_vector_na( rprobsL1, 0 )
	res <- TAM_CALCEXP2( np , rprobsL1 , AL1 ,	indexIP.no , 
			indexIP.list2 , est.xsi.index , CC , itemwt , NI*CC , TP  )
	return(res)
}
		
calc_exp_TK3 <- tam_calc_exp

