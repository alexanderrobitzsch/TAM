## File Name: tam.linking.R
## File Version: 0.19
## File Last Change: 2017-06-23 12:13:16

tam.linking <- function( tamobj_list , type = "SL", theta=NULL, wgt=NULL, fix.slope=FALSE,
	verbose=TRUE)
{
	CALL <- match.call()
	NM <- length(tamobj_list)	
	#** theta specifications
	if (is.null(theta)){
		theta <- seq(-6,6,len=101)
	}
	if (is.null(wgt)){
		wgt <- tam_normalize_vector( stats::dnorm(theta, sd = 2 ) )
	}
	theta <- matrix( theta, ncol=1)		
	#--- extract parameters
	parameters_list <- list()
	for (mm in 1:NM){
		parameters_list[[mm]] <- tam_linking_extract_parameters( tamobj=tamobj_list[[mm]] )
	}
	#**** LINKING
	linking_list <- list()
	linking_args <- list( theta=theta , wgt=wgt , type=type, fix.slope=fix.slope)	
	for (mm in 1:(NM-1) ){	
		if (verbose){
			cat( paste0("Linking Study ", mm , " -> Study " , mm+1 ) , "\n")
			utils::flush.console()
		}
		entries <- c("items", "B" , "A" , "AXsi" , "guess", "M", "SD")
		#--- extract first study
		out1 <- tam_linking_extract_list( input =  parameters_list[[mm]], entries=entries )
		#--- extract second study
		out2 <- tam_linking_extract_list( input =  parameters_list[[mm+1]], entries=entries )
		#--- common item parameters
		items_sel <- intersect( out1$items , out2$items)
		out1 <- tam_linking_parameters_select_common_items(out=out1, items_sel=items_sel, names_suffix="1")
		out2 <- tam_linking_parameters_select_common_items(out=out2, items_sel=items_sel, names_suffix="2")
		linking_args <- tam_linking_include_list( list1=linking_args, list2=out1 )
		linking_args <- tam_linking_include_list( list1=linking_args, list2=out2 )
		#-- call linking function
		link_mm <- do.call( "tam_linking_2studies" , linking_args)
		linking_list_mm <- list()
		linking_list_mm$common_items <- items_sel
		linking_list_mm$linking_results <- link_mm
		linking_list[[mm]] <- linking_list_mm
		M_SD <- link_mm$M_SD
		N_groups <- attr(M_SD , "N_groups")
		ind <- N_groups[1] + seq(1,N_groups[2])
		M_SD <- M_SD[ ind , , drop=FALSE ]
		rownames(M_SD) <- paste0("group", 1:N_groups[2])
		parameters_list[[mm+1]][["M"]] <- M_SD[ ,"M"]
		parameters_list[[mm+1]][["SD"]] <- M_SD[ ,"SD"]	
		parm_mm <- parameters_list[[mm+1]]
		res <- tam_linking_transform_item_parameters( B = parm_mm$B , AXsi = parm_mm$AXsi , A = parm_mm$A , 
						trafo_items=link_mm$trafo_items )
		parm_mm <- tam_linking_include_list( list1=parm_mm, list2=res )					
		parameters_list[[mm+1]] <- parm_mm	
	}
	#--- organize output
	res <- tam_linking_output_summary( parameters_list=parameters_list, linking_list=linking_list )
	M_SD <- res$M_SD
	trafo_persons <- res$trafo_persons
	trafo_items <- res$trafo_items
	N_common <- res$N_common
	#--- OUTPUT
	res <- list(parameters_list=parameters_list, linking_list=linking_list, M_SD=M_SD, trafo_persons=trafo_persons,
					trafo_items=trafo_items, N_common=N_common, theta=theta, wgt=wgt, NS=NM, type=type, CALL = CALL)
	class(res) <- "tam.linking"
	return(res)
}
