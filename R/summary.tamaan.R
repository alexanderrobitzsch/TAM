## File Name: summary.tamaan.R
## File Version: 9.15

###############################################
# summary tamaan
summary.tamaan <- function( object, file=NULL, ... )
{

    tam_osink( file=file)

    #**********************
    # general tamaan syntax
    # cat("------------------------------------------------------------\n")
    cat("!:!:!:!:!:!:!:!:!:!:!:!:!:!:!:!:!:!:!:!:!:!:!:!:!:!:!:!:!:!:\n")
    cat(paste0("tamaan function using '",
        object$tamaanify$method, "' method\n\n") )
    cat( paste(object$tamaanify$tammodel) )
    cat("\n\n")

    #**********************
    # tam.mml
    if ( object$tamaan.method=="tam.mml" ){
        summary.tam.mml( object, file=NULL,... )
    }
    #**********************
    # tam.mml.2pl
    if ( object$tamaan.method=="tam.mml.2pl" ){
        summary.tam.mml( object, file=NULL, ... )
    }
    #**********************
    # tam.mml.3pl
    if ( object$tamaan.method=="tam.mml.3pl" ){
        #--- overview for all parameters
        summary_tamaan_3pl_intro(object)

        #--- distribution discrete skill space
        if ( !( object$tamaanify$ANALYSIS.list$type %in% c( "MIXTURE" ) )){
            if ( object$skillspace=="discrete" ){
                summary_tamaan_3pl_discrete_distribution(object)
            }
        }
        #--- distribution normal skill space
        if (object$skillspace=="normal"){
            summary_tamaan_normal_skillspace(object)
        }

        #--- cluster locations
        if ( object$tamaanify$ANALYSIS.list$type %in% c( "LOCLCA" ) ){
            summary_tamaan_3pl_loclca(object)
        }

        #--- distribution mixture
        if ( object$tamaanify$ANALYSIS.list$type %in% c( "MIXTURE" ) ){
            summary_tamaan_3pl_distr_mixture(object)
        }

        #--- Item parameters
        print_ipars <- FALSE
        if (object$skillspace=="normal"){
            print_ipars <- TRUE
        }
        if ( object$tamaanify$ANALYSIS.list$type %in% c( "LOCLCA" ) ){
            print_ipars <- TRUE
        }
        if ( print_ipars ){
            summary_tamaan_item_parameters(object)
        }

        #--- Latent class probabilities
        if ( object$tamaanify$ANALYSIS.list$type %in% c( "LCA", "OLCA" ) ){
            summary_tamaan_3pl_lcaprobs(object)
        }

        #--- Mixture distribution
        if ( object$tamaanify$ANALYSIS.list$type %in% c( "MIXTURE" ) ){
            summary_tamaan_item_parameters_mixture( object )
        }

        #--- Item means
        if ( object$tamaanify$ANALYSIS.list$type %in% c( "LCA", "OLCA") ){
            summary_tamaan_3pl_class_item_average(object=object)
        }

    }
    #******
    tam_csink(file=file)
}
################################################
