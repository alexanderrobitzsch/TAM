## File Name: tam_round_data_frame_print.R
## File Version: 0.01
## File Last Change: 2017-09-15 09:58:38


tam_round_data_frame_print <- function(obji, from=1, to=ncol(obji), digits=3)
{
	obji <- tam_round_data_frame(obji=obji, from=from, to=to, digits=digits)
	print(obji)
}
