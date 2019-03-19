## File Name: tam_mml_constraint_check.R
## File Version: 0.06

tam_mml_constraint_check <- function(constraint)
{
    if (constraint=="item"){
        constraint <- "items"
    }
    if (constraint=="case"){
        constraint <- "cases"
    }
    elig_constraints <- c("items","cases")
    if (! ( constraint %in% elig_constraints) ){
        stop( paste0("Please choose one of the constraints: '",
                paste0( elig_constraints, collapse="' or '"), "'\n") )
    }
    return(constraint)
}
