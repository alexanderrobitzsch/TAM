## File Name: tamaan_3pl_mixture_individual_class_probabilities.R
## File Version: 0.01
## File Last Change: 2017-10-23 10:54:21

tamaan_3pl_mixture_individual_class_probabilities <- function(hwt, NCLASSES)
{
	TP <- ncol(hwt) / NCLASSES
	ind_classprobs <- matrix( NA, nrow=nrow(hwt), ncol=NCLASSES )
	for (cl in 1:NCLASSES){
		ind_classprobs[,cl] <- rowSums( hwt[ , (cl-1)*TP + (1:TP)] )
	}
	return(ind_classprobs)
}
