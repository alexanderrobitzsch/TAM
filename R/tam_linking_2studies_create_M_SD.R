## File Name: tam_linking_2studies_create_M_SD.R
## File Version: 0.05
## File Last Change: 2017-06-22 16:06:30

tam_linking_2studies_create_M_SD <- function(M1, SD1, M2, SD2, trafo_persons)
{
	G1 <- length(M1)
	G2 <- length(M2)
	GT <- G1+G2	
	M_SD <- matrix( 0 , nrow=GT , ncol=2 )
	colnames(M_SD) <- c("M", "SD")
	# M_SD <- as.data.frame(M_SD)
	rownames(M_SD) <- tam_linking_2studies_create_M_SD_rownames(G1=G1, G2=G2)
	ind1 <- seq(1,G1)
	M_SD[ ind1 , "M"] <- M1
	M_SD[ ind1 , "SD"] <- SD1
	ind2 <- G1 + seq(1,G2)
	M_SD[ ind2 , "M"] <- M2
	M_SD[ ind2 , "SD"] <- SD2
	#-- transformations
	M_SD[ind2, "SD"] <- M_SD[ind2, "SD"] * trafo_persons["a"]
	M_SD[ind2, "M"] <- M_SD[ind2, "M"] * trafo_persons["a"] + trafo_persons["b"]	
	attr( M_SD, "N_groups") <- c(G1, G2)
	#--- output
	return(M_SD)
}
