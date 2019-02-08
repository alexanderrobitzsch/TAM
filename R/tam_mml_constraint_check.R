## File Name: tam_mml_constraint_check.R
## File Version: 0.03

tam_mml_constraint_check <- function(constraint)
{
    if (constraint=="item"){
        constraint <- "items"
    }
    if (constraint=="case"){
        constraint <- "cases"
    }
    return(constraint)
}
