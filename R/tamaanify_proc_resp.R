## File Name: tamaanify_proc_resp.R
## File Version: 0.01


tamaanify_proc_resp <- function(res, resp)
{
    cols <- paste(res$items$item)
    resp <- resp[, cols]
    res$resp <- resp
    return(res)
}
