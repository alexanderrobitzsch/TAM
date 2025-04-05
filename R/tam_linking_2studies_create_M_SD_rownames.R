## File Name: tam_linking_2studies_create_M_SD_rownames.R
## File Version: 0.043

tam_linking_2studies_create_M_SD_rownames <- function(G1, G2,
            study1="study1", study2="study2")
{
    row_names <- NULL
    if (G1==1){
        v1 <- study1
    } else {
        v1 <- paste0('study1-group',1L:G1)
    }
    row_names <- c( row_names, v1 )
    if (G2==1){
        v1 <- study2
    } else {
        v1 <- paste0('study2-group',1L:G2)
    }
    row_names <- c( row_names, v1 )
    return(row_names)
}
