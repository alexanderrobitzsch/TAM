
tam_linking_output_summary <- function( parameters_list, linking_list)
{
	NM <- length(parameters_list)
	#--- means and SDs
	M_SD <- NULL
	for (mm in 1:NM){
		M <- parameters_list[[mm]]$M
		SD <- parameters_list[[mm]]$SD
		dfr <- data.frame( M=M, SD=SD)
		G <- length(M)
		if (G==1){
			row_names <- paste0("study",mm)
		} else {
			row_names <- paste0("study",mm, "-group" , 1:G)
		}
		rownames(dfr) <- row_names
		M_SD <- rbind(M_SD, dfr)		
	}	
	M_SD$d <- M_SD$M / mean(M_SD$SD)	
	#--- transformation item parameters
	trafo_items <- matrix( 0 , nrow=NM , ncol=2)
	rownames(trafo_items) <- paste0( "study",1:NM)
	colnames(trafo_items) <- c("a","b")
	trafo_items <- as.data.frame(trafo_items)
	trafo_items$a <- 1	
	for (mm in 1:(NM-1) ){
		trafo_mm <- linking_list[[mm]]$linking_results$trafo_items
		trafo_items[mm+1,] <- trafo_mm
	}	
	#--- transformation person parameters
	trafo_persons <- trafo_items
	for (mm in 1:(NM-1) ){
		trafo_persons[mm+1,] <- linking_list[[mm]]$linking_results$trafo_persons
	}	
	#--- number of linking items
	N_common <- matrix(0 , nrow=NM-1, ncol=1)
	rownames(N_common) <- paste0("Linking Study " , 1:(NM-1) , " -> Study " , 2:NM )
	colnames(N_common) <- "N_Items"
	for (mm in 1:(NM-1) ){
		N_common[mm,1] <- length( linking_list[[mm]]$common_items )		
	}			
	#--- OUTPUT
	res <- list(M_SD=M_SD, trafo_items=trafo_items, trafo_persons=trafo_persons,
					N_common=N_common)
	return(res)
}