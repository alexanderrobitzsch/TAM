## File Name: tam_rowCumsums.R
## File Version: 0.02

tam_rowCumsums <- function(matr){ 
	tam_rcpp_rowCumsums(input=matr)
}

rowCumsums.TAM <- tam_rowCumsums
