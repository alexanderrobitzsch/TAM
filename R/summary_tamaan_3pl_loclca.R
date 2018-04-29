## File Name: summary_tamaan_3pl_loclca.R
## File Version: 0.06


##############################################res#################
# cluster locations
summary_tamaan_3pl_loclca <- function(object)
{
    #-- print cluster locations
    cat("*******************************\n")
    cat("Cluster locations\n")
    tam_round_data_frame_print(obji=object$locs, digits=3)
    #-- print item response probabilities
    summary_tamaan_3pl_lcaprobs(object=object)
    #-- class-specific item averages
    summary_tamaan_3pl_class_item_average(object=object)
}
######################################################################
