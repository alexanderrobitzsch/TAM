## File Name: tam_mml_3pl_calc_total_ll.R
## File Version: 0.05

tam_mml_3pl_calc_total_ll <- function( iIndex , A , B , xsi , theta ,
			nnodes , guess , n.ik , eps )
{		
	AXsi <- tam_mml_compute_AXsi(A=A, xsi=xsi)
	maxK <- dim(A)[2]
    probs0 <- tam_mml_3pl_calc_prob( iIndex= iIndex , A=A , AXsi=AXsi , B=B , 
                        xsi=xsi , theta=theta , nnodes=nnodes, maxK=maxK , 
						guess=guess )$rprobs				
	n.ik <- n.ik[ iIndex ,, , drop=FALSE ]	
	ll0 <- tam_mml_3pl_calc_ll( n.ik=n.ik , probs = probs0, eps=eps )		
	return(ll0)
}
		
