## File Name: tam_round_data_frame_print.R
## File Version: 0.08
## File Last Change: 2017-09-16 14:07:43

tam_round_data_frame_print <- function(obji, from=1, to=ncol(obji), digits=3, 
		rownames_null=FALSE)
{
	obji <- tam_round_data_frame(obji=obji, from=from, to=to, digits=digits,
				rownames_null=rownames_null)
	print(obji)
	invisible(obji)
}
