
require_namespace_msg <- function(pkg)
{
    # if ( ! requireNamespace( pkg , quietly = TRUE) ){
    #    stop( paste0("Package '" , pkg , "' needed fo this function 
    #         to work. Please install it." ), call. = FALSE)
    # }
	#--- use function from CDM package
	CDM::CDM_require_namespace(pkg=pkg)
}