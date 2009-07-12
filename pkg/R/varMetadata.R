################################################################################
# R PACKAGE:   mframe 
# FILE:	       R/varMetadata.R
# DESCRIPTION: S3 class to represent a data frame single variable metadata  
# AUTHORS: 	   Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  2009 May 25
# LICENSE:     GPL-2
################################################################################
# TODO: Test defining multiple classes in varMetadata.
# TODO: valIndex with retclass="character" should return NA for out-of-range
#		integer indexes: i <- valIndex(i, x, retclass="character", handler=NULL)
#		now returns the number coerced to character.

#' @nord
#  NOT EXPORTED

.VM_RESERVED_NAMES <- c("type", "coerce", "na.ok", "default", 
	"varType", "varCoerce", "naOk", "varDefault");

#' Creates objects of the S3 class \code{varMetadata} that represent metadata 
#' of variables. A "variable" is normally a vector stored as a column of a 
#' data frame. 
#' 
#' Variable's metadata consists of specifications for:
#' \enumerate{
#' 	\item the type of the variable, which can be just the class or include
#' 		optional validation tests;
#'  \item how to coerce values to the target type of the variable;
#'  \item whether missing data (\code{NA}s) are accepted;
#'  \item the default value to assign new data is created without an
#' 		explicit value; and
#'  \item any user-defined information, such as descriptive labels, units
#' 		of measure, etc.
#' }
#' 
#' Normally, \code{varMetadata} is not used in isolation, but as a part of
#' data frame specifications of class \code{\link{dfMetadata}}. 
#'
#' \section{Methods}{ 
#' \link[base:UseMethod]{Methods} to \link[base:InternalMethods]{standard 
#' generics} for "\code{varMetadata}" objects currently include: 
#' \code{\link[coerce.varMetadata]{coercion}} to \code{character} 
#' and \code{list}, \code{\link[print.varMetadata]{print}}, 
#' the \code{$} \code{\link[Extract.varMetadata]{extract operator}},
#' and \code{\link[c.varMetadata]{c}}.
#'  
#' \code{as.varMetadata} is a generic function with a default implementation. 
#' \link[base:UseMethod]{Methods} can be added for new classes.
#' 
#' Several generic functions and methods are provided to set and replace each 
#' of the standard components of variable's metadata: 
#' \code{\link{varType}}, \code{\link{varCoerce}},
#' \code{\link{naOk}} and \code{\link{varDefault}}. The \code{$} operator
#' with the same names is a wrapper for each of the methods, whereas with
#' any other name it provides access to the user-defined metadata.
#' The other subset operators (\code{[} and \code{[[}) only give access to the
#' list of user-defined metadata.
#' 
#' Method \code{\link{enforce}} is provided to ensure compliance to the
#' metadata constraints: it generates or validates data conforming to the 
#' specification.     
#' }
#' 
#' @title Variables' Metadata
#' @aliases varMetadata is.varMetadata as.varMetadata
#' @usage varMetadata(type, coerce = NULL, na.ok, default, \dots) 
#' is.varMetadata(x) 
#' as.varMetadata(x, \dots)
#' 
#' @param type specification of the type of the variable, in any of the forms 
#'  accepted by method \code{\link[varType]{varType<-}}.
#' @param coerce specification of the coercion behaviour, in any of 
#'  the forms accepted by method \code{\link[varCoerce]{varCoerce<-}}.
#' @param na.ok specification of whether to accept or reject missing
#'  (\code{NA}) values, in any of the forms accepted by method
#'  \code{\link[naOk]{naOk<-}}.
#' @param default specification of the default value to use when new data is
#' 	created without an explicit value, in any of the forms accepted by 
#'  method \code{\link[varDefault]{varDefault<-}}.
#' @param \dots extra metadata in the form \code{tag = value}. This
#'  user-defined data can then be retrieved with the usual 
#'  \link[varMetadata:Extract]{extraction operators}.
#' @param x an \R object, normally of class \code{varMetadata} or that 
#'  can be coerced to one.
#' 
#' @return For \code{varMetadata} and \code{as.varMetadata}, an object of 
#'  class \code{varMetadata}.
#' 
#'  For \code{is.varMetadata}, \code{TRUE} or \code{FALSE}.
#' 
#' @seealso Classes \code{\link{dfMetadata}} and \code{\link{mframe}}.
#' @export

varMetadata <- function(type, coerce=NULL, na.ok, default, ...) {
	result <- structure(list(...), 
			type = NA_character_, 
			coerce = NULL,
			na.ok = TRUE,
			default = NULL, 
			class="varMetadata");
	result <- suppressWarnings(`varType<-.varMetadata`(result, type));
	result <- `varCoerce<-.varMetadata`(result, coerce);
	result <- `naOk<-.varMetadata`(result, na.ok);
	result <- `varDefault<-.varMetadata`(result, default);	
	result;
}

#' @nord
#' @export

is.varMetadata <- function(x)
	inherits(x, "varMetadata");

#' @nord
#' @S3method as.varMetadata default
#' @note If \code{x} is a list and contains quoted expressions, it
#' 	should be created with \code{alist} instead of \code{list} to prevent
#' 	wrong evaluation of the \code{coerce} argument in \code{varMetadata}.

as.varMetadata.default <- function(x, ...) {
	if (is.varMetadata(x))
		return(x)
	else if (is.list(x))
		do.call(varMetadata, x, quote=TRUE)
	else varMetadata(x, ...);
}

#' Methods for coercing "\code{\link{varMetadata}}" objects to other classes. 
#' @title Coercion from Variable's Metadata
#' @name coerce.varMetadata
#' @aliases as.character.varMetadata as.list.varMetadata
#' @usage
#' 	\method{as.character}{varMetadata}(x, \dots)
#' 	\method{as.list}{varMetadata}(x, \dots)
#' 
#' @param x an object of class \code{varMetadata}.
#' @param \dots further arguments passed to or from other methods.
#' @return For \code{as.list}, a named list whose first four elements are each
#'  of the standard components "\code{type}", "\code{coerce}", "\code{na.ok}",
#'  and "\code{default}", followed by all user-defined elements.
#' 
#'  For \code{as.character}, a character vector with the same elements as
#'  \code{as.list}, each of them coerced to character, in general using
#'  \link[base:deparse]{deparse}. This produces a very
#'  compact representation which is also used to print \code{varMetadata}
#'  objects.
#' @seealso \code{\link{varMetadata}}.
#' @S3method as.character varMetadata

as.character.varMetadata <- function(x, ...) {
#	compact0 <- function(x) deparse(x, nlines=1); 
#		if (is.language(x) || is.function(x) || !length(x) || length(x) > 1L)
#			deparse(x)
#		else if (all(is.na(x)) || is.factor(x))
#			deparse(x)
#		else as.character(x);
    
	sapply(x, deparse, nlines=1);
}

#' @nord
#' @S3method as.list varMetadata

as.list.varMetadata <- function(x, ...) { 
	result <- list(type=varType(x), coerce=varCoerce(x), na.ok=naOk(x), 
		default=varDefault(x));
	if (length(x) > 0L) c(result, unclass(x)) else result;
}


#' \code{\link[base:print]{Prints}} objects of class \code{\link{varMetadata}}.
#' 
#' @title Print Variables' Metadata
#' @param x an object of class \code{varMetadata}.
#' @param style string specifying the printing style, which can be "compact" 
#' 	(the default) or "list". The "compact" style uses less space using
#'  \code{\link[coerce.varMetadata]{as.character}}, while the "list" style
#'  prints the metadata as a list.  
#' @param title logical flag. Whether to print a first line with the
#'  object class.
#' @param \dots further arguments passed to \code{print} when 
#' 	\code{style = "list"}.
#' 
#' @return \code{x}, invisibly. Invoked for its side effect of printing
#'  a character representation of \code{x} to the console.
#' @seealso \code{\link{varMetadata}}.
#' @S3method print varMetadata

print.varMetadata <- function(x, style="compact", title=TRUE, ...) {
	style <- match.arg(style, c("compact", "list"));
	if (title) cat("[Variable Metadata]\n");
	if (style == "list") {
		print(as.list(x), ...);
		return(invisible(x));
	}	

	x <- as.character(x);
	maxNchar <- max(nchar(names(x)), na.rm=TRUE) + 1;
	trailing <- sapply(
			mapply(rep, times=maxNchar-nchar(names(x)), MoreArgs=list(x=" ")), 
		paste, collapse="");	
	
	for (i in seq_along(x))
		cat(" ", names(x)[i], trailing[i], "= ", x[i], "\n", sep="");
		
	invisible(x);
}

#' @nord
#' @export

varType.varMetadata <- function(x) { 
	result <- attr(x, "type");
	if (length(result)) result else NA_character_;
}

#' @name varTypeReplace.varMetadata
#' @nord
#' @export `varType<-.varMetadata`

`varType<-.varMetadata` <- function(x, value) {	
	if (missing(value)) 
		if (length(attr(x, "type"))) 
			return(x) 
		else value <- NA_character_
	else if (!length(value) || (is.atomic(value) && any(is.na(value))))
		value <- NA_character_
    else if (is.character(value) || is.evaluable(value)) {
		if (is.character(value)) 
			value <- unique(value)
		test <- tryCatch(checkType(logical(0), value),
			warning = function(w) NULL,
			error = function(e) stop(paste("type test is not valid:",
					paste(e, sep=" ", collapse=" ")), call.=FALSE));
		if (!identical(TRUE, test) && !identical(FALSE, test))
			stop("variable type test must return a logical of length 1");    	    	
	} else 
		stop(gettextf("type of %s not recognized", sqMsg("varType")))

	if (!is.character(value) && is.null(varCoerce(x))) 
		warning("varType of class 'call' or 'function' requires varCoerce to be defined");		
	
	attr(x, "type") <- value;
	
	if (!is.null(attr(x, "default")))
		attr(x, "default") <- enforce(x, attr(x, "default"));
	x;	
}

#' @nord
#' @export

varCoerce.varMetadata <- function(x)  
	attr(x, "coerce");

#' @name varCoerceReplace.varMetadata
#' @nord
#' @export `varCoerce<-.varMetadata`

`varCoerce<-.varMetadata` <- function(x, value) {
	if (missing(value)) 
		return(x)
    else if (!length(value)) {
		if (!is.character(varType(x)))
			stop("undefined coercion only works with varType of class character");		
		value <- NULL;		
	} else if (is.evaluable(value))  
		tryCatch(doVarCoercion(logical(0), value, varType(x)),
			error = function(e) stop(paste("coercion is not valid:",
					paste(e, sep=" ", collapse=" ")), call.=FALSE))    	
	else
		stop("coercion must be NULL, a quoted expression, a function or a function name")
	
	attr(x, "coerce") <-  value;
	
	if (!is.null(attr(x, "default")))
		attr(x, "default") <- enforce(x, attr(x, "default"));	
	x;	
}

#' @nord
#' @export

naOk.varMetadata <- function(x) {
	result <- attr(x, "na.ok");
	if (is.null(result)) TRUE else result;	
}

#' @name naOkReplace.varMetadata
#' @nord
#' @export `naOk<-.varMetadata`

`naOk<-.varMetadata` <- function(x, value) {
	if (missing(value))
		return(x)
	else if (invalid(value))    	 
		value <- TRUE
	else if (!is.logical(value))
		stop("na.ok? spec must be logical")
    else if (is.na(value[[1L]]))
		value <- TRUE
	else value <- value[[1L]];

	attr(x, "na.ok") <- value;
	x;
}

#' @nord
#' @export

varDefault.varMetadata <- function(x, eval=FALSE) {
	result <- attr(x, "default");
	if (is.null(result)) 
		enforce(x, NA, verify.na.ok=FALSE) 
	else if (eval) {
		if (is.function(result) || is.language(result))
			enforce(x, doEval(result), verify.na.ok=FALSE)
		else result
	} else result;
}

#' @name varDefaultReplace.varMetadata
#' @nord
#' @export `varDefault<-.varMetadata`

`varDefault<-.varMetadata` <- function(x, value) {	
	if (missing(value))
    	return(x)	
	if (length(value) == 0L) { 
    	value <- evalValue <- NA;
		isEvaluable <- FALSE;
	} else {		
		# We don't use is.evaluable() as strings must not evaluated here  		
		isEvaluable <- (is.function(value) || is.language(value));
		if (isEvaluable) 
			evalValue <- doEval(value)
		else {
			if (length(value) > 1L) {
				warning("only first element of default value is retained");
				value <- value[[1L]];
			}
			evalValue <- value;			
		}		
	}
	
	# Verify that the default is of (or can be coerced to) the target type.
	# NOTE: If we allow expressions depending on the mframe data at some
	#	point, we will not be able to be perform this test
	tryCatch(evalValue <- enforce(x, evalValue, verify.na.ok=FALSE),
		error = function(e) 
			stop(gettextf("default value cannot be coerced to target type: "), 
				paste(e, collapse=" "), call.=FALSE) 
	)
	if (!isEvaluable) value <- evalValue; 	

	attr(x, "default") <- value;
	x;	
}

#' @nord
#' @S3method varMeta varMetadata

varMeta.varMetadata <- function(x, i) {
	result <- x;
	customAttributes(result) <- NULL;
	if (missing(i)) result else result[i]	
}

#' Extract or replace parts of objects of class \code{\link{varMetadata}}   
#' 
#' The \code{[} and \code{[[} operators with \code{varMetadata} are used
#' to extract or replace only the user-defined list components of the metadata.
#'  
#' Access to standard components "\code{type}", "\code{coerce}", 
#' "\code{na.ok}" and "\code{default}" is usually done using the explicit 
#' methods \code{\link{varType}}, \code{\link{varCoerce}}, \code{\link{naOk}},
#' and \code{\link{varDefault}}, respectively, and their replacement versions.
#'    
#' The \code{$} operator is overloaded to provide access to all the 
#' components, so that \code{x$varType} is equivalent to \code{varType(x)},
#' \code{x$varType <- value} is equivalent to \code{varType(x) <- value},
#' and so on.
#' 
#' @title Extract or Replace Parts of Variable's Metadata
#' @name Extract.varMetadata
#' @aliases $.varMetadata $<-.varMetadata
#' @usage
#' 	\method{$}{varMetadata}(x, i) 
#' 	\method{$}{varMetadata}(x, i) <- value
#' 
#' @param x an object of class \code{varMetadata}.
#' @param i string specifying the element to extract or replace. Follows the
#' 	sames rules as \code{$} for lists when accessing user-defined metadata, 
#'  and provides a wrapper to the standard \code{varMetadata} components 
#'  methods when \code{i} is any of "\code{varType}", "\code{varCoerce}", 
#'  "\code{naOk}", or "\code{varDefault}".
#' @param value a suitable replacement value. 
#'  If \code{NULL} and \code{i} indexes a user-defined element, the element
#'  is removed. If \code{i} indexes a standard element, it is cleared, 
#'  setting it to its default value.  
#'  
#' @seealso \code{\link{varType}}, \code{\link{varCoerce}}, 
#'  \code{\link{naOk}} and \code{\link{varDefault}}.  
#' @S3method `$` varMetadata

`$.varMetadata` <- function(x, i) {
	i <- as.character(substitute(i));
	switch(i, 
		type=, varType = varType(x),
		coerce=, varCoerce = varCoerce(x),
		na.ok=, naOk = naOk(x),
		default=, varDefault = varDefault(x),
		x[[i]]);
}

#' @name Replace3.varMetadata
#' @nord
#' @S3method `$<-` varMetadata

`$<-.varMetadata` <- function(x, i, value) {
	switch(as.character(substitute(i)), 
		type=, varType = varType(x) <- value,
		coerce=, varCoerce = varCoerce(x) <- value,
		na.ok=, naOk = naOk(x) <- value,
		default=, varDefault = varDefault(x) <- value,
		x[[i]] <- value);
	x;
}

#' This is the \code{\link{c}} method for variable's metadata. It concatenates
#' all the supplied variable metadata and lists into a single 
#' \code{varMetadata}. This can be used for adding user-defined elements
#' to a variable's metadata, but not for setting any of the reserved
#' components ("\code{varType}", "\code{varCoerce}", "\code{naOk}", 
#' or "\code{varDefault}", which are always taken from the first 
#' \code{varMetadata} object.
#'  
#' Unlike the default implementation of \code{c}, no attributes are dropped 
#' (i.e. \code{c(x)} equals \code{x}).
#' 
#' @title Combine Variable's Metadata
#' @usage \method{c}{varMetadata}(\dots)
#' @param \dots objects to be concatenated, normally of class 
#'  "\code{varMetadata}" or lists.
#' 
#' @return An object of class \code{varMetadata}. 
#' @seealso \code{\link[base]{c}}.
#' @S3method c varMetadata
# TODO: Should we use some attribute from non-first item?

c.varMetadata <- function(...) {	
	first <- list(...)[[1L]];
	as.varMetadata(c(list(type=varType(first), coerce=varCoerce(first),
		na.ok=naOk(first), default=varDefault(first)), NextMethod()));
}

#' @nord
#  NOT EXPORTED

checkType <- function(x, type) {
	if (is.character(type)) {
		if (any(is.na(type)))  
			TRUE		
		if (length(type) == 1L)
			is(x, type)
		else 
			any(mapply(is, class2=type, MoreArgs=list(object=x), SIMPLIFY=TRUE, 
				USE.NAMES=FALSE))	
	} else if (is.evaluable(type)) { 
		result <- doEval(type, x=x);
#		result <- if (is.call(type)) 
#			eval(type, list(x=x)) 		
#		else if (is.function(type))
#			type(x)	
#		else 
#			stop(gettextf("type of %s not recognized", sqMsg("varType")));	

		if (!(identical(result, TRUE) || identical(result, FALSE))) {
			warning("type checking does not return TRUE or FALSE")
			result <- FALSE;
		}
		result			
	} else
		stop(gettextf("type of %s not recognized", sqMsg("varType"))); 
}

#' @nord
#  NOT EXPORTED

doVarCoercion <- function(x, coercion, type) {	
    if (!length(coercion)) {
		if (!is.character(type))
			stop("undefined coercion only works with varType of class character");
		type <- type[[1L]];
		if (is.na(type))
			x
		else if (canCoerce(x, type)) 
			as(x, type)
		else
			match.fun(paste("as", type, sep="."))(x)
	} else doEval(coercion, x=x) 
#		if (is.call(coercion))
#    	eval(coercion, list(x=x))
#	else if (is.function(coercion)) 
# 		coercion(x)
#	else if (is.character(coercion) && length(coercion) == 1L)
#		match.fun(coercion)(x)
#	else 
#		stop(gettextf("type of %s not recognized", sqMsg("varCoerce")));	
}


#' Ensures compliance to metadata constraints, generating or validating data 
#' that conforms to the metadata specification.     
#' 
#' @title Apply Metadata Specifications to Data
#' @param x a metadata object. 
#' @param data data on which metadata should be enforced. 
#' @param verify.na.ok logical flag. Whether the \code{na.ok} specification
#' 	should be verified (and enforced).
#' 
#' @return \code{data}, possibly transformed to ensure that it complies
#' 	with all constraints defined in the metadata. An error should be
#'  issued if some constraint cannot be enforced.
#' @seealso \code{\link{varMetadata}}.
#' @export
 
enforce <- function(x, data=logical(0), verify.na.ok=TRUE) {
	type <- varType(x);
	result <- if (checkType(data, type)) 
		data
	else 		
		doVarCoercion(data, varCoerce(x), type);
	
	if (verify.na.ok && !naOk(x) && length(result) && any(is.na(result)))
		stop("missing (NA) values are not allowed");	
	result;
}

#' @nord
#  Not exported.

customAttrNames.varMetadata <- function(x, class=TRUE) {
	result <- c("type", "coerce", "na.ok", "default");
	if (class) c("class", result) else result; 
}
