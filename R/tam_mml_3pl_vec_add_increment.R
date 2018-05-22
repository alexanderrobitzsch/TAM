## File Name: tam_mml_3pl_vec_add_increment.R
## File Version: 0.04

tam_mml_3pl_vec_add_increment <- function( vec, h, index ){
    vec[index] <- vec[index] + h
    return(vec)
}
