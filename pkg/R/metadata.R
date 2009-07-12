################################################################################
# R PACKAGE:   mframe
# FILE:        R/metadata.R
# DESCRIPTION: Default metadata-related methods. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  26/05/2009
# LICENSE:     GPL-2
################################################################################

#' If \code{x} is a character vector, returns the metadata corresponding to 
#' the class named by \code{x}, provided a \code{metadata} method is 
#' implemented for that class, or \code{NULL} if it is not implemented.
#' For other objects, returns the "metadata" attribute of the object, 
#' or \code{NULL} if this attribute does not exist. 
#'
#' @nord
#' @S3method metadata default
#' @seealso  \code{\link{metadata}}.

metadata.default <- function(x) {
	if (is.character(x)) {
		if (length(x) == 0L)
			stop(gettextf("%s must be a class name", sqMsg("x")));
		x <- x[1L];
		fun <- paste("metadata", x, sep=".");
		if (exists(fun) && is.function(get(fun)))
	    	match.fun(fun)()
		else NULL;
	} else attr(x, "metadata"); 	
} 
	
# Infers a \code{dfMetadata} object from a \code{data.frame}.
#metadata.data.frame <- function(x) {	
#}

#' @nord
#' @S3method key default
#' @seealso  \code{\link{key}}.

key.default <- function(x) 
	attr(x, "key");

#' @name keyReplace.default
#' @nord
#' @seealso \code{\link{key}}.
#' @S3method `key<-` default

`key<-.default` <- function(x, value) {
	attr(x, "key") <- if (!is.null(dim(x)) && length(dim(x)) > 1L) 
		valIndex(value, x, dim=2, .name="value")
	else if (length(value))
		valIndex(value, x, .name="value")
	else  
		NULL;
	x;
}

#' @nord
#' @seealso \code{\link{varType}}.
#' @S3method varType default

varType.default <- function(x) lapply(x, class);


