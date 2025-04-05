## File Name: tam_linking_chain.R
## File Version: 0.121

tam_linking_chain <- function(NM, parameters_list, entries, verbose,
    linking_args, linking_list)
{
    for (mm in 1L:(NM-1) ){
        if (verbose){
            cat( paste0('Linking Study ', mm, ' -> Study ', mm+1 ), '\n')
            utils::flush.console()
        }
        #--- extract first study
        out1 <- tam_linking_extract_list( input=parameters_list[[mm]], entries=entries )
        #--- extract second study
        out2 <- tam_linking_extract_list( input=parameters_list[[mm+1]], entries=entries )
        #--- common item parameters
        items_sel <- intersect( out1$linking_items, out2$linking_items)
        out1 <- tam_linking_parameters_select_common_items(out=out1,
                            items_sel=items_sel, names_suffix='1')
        out2 <- tam_linking_parameters_select_common_items(out=out2,
                            items_sel=items_sel, names_suffix='2')
        linking_args <- tam_linking_include_list( list1=linking_args, list2=out1 )
        linking_args <- tam_linking_include_list( list1=linking_args, list2=out2 )
        #-- call linking function
        linking_args$par_init <- NULL
        link_mm <- do.call( what=tam_linking_2studies, args=linking_args)
        linking_list_mm <- list()
        linking_list_mm$common_items <- items_sel
        linking_list_mm$linking_results <- link_mm
        linking_list[[mm]] <- linking_list_mm
        M_SD <- link_mm$M_SD
        N_groups <- attr(M_SD, 'N_groups')
        ind <- N_groups[1] + seq(1,N_groups[2])
        M_SD <- M_SD[ ind,, drop=FALSE ]
        rownames(M_SD) <- paste0('group', 1L:N_groups[2])
        parameters_list[[mm+1]][['M']] <- M_SD[,'M']
        parameters_list[[mm+1]][['SD']] <- M_SD[,'SD']
        parm_mm <- parameters_list[[mm+1]]
        res <- tam_linking_transform_item_parameters( B=parm_mm$B, AXsi=parm_mm$AXsi, 
                            A=parm_mm$A, trafo_items=link_mm$trafo_items )
        parm_mm <- tam_linking_include_list( list1=parm_mm, list2=res )
        parameters_list[[mm+1]] <- parm_mm
    }
    #--- organize output
    res <- tam_linking_output_summary( parameters_list=parameters_list, 
                    linking_list=linking_list )
    M_SD <- res$M_SD
    trafo_persons <- res$trafo_persons
    trafo_items <- res$trafo_items
    N_common <- res$N_common

    #--- output
    res <- list(M_SD=M_SD, N_common=N_common, trafo_persons=trafo_persons,
                trafo_items=trafo_items, linking_list=linking_list,
                parameters_list=parameters_list)
    return(res)
}
