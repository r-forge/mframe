################################################################################
# R PACKAGE:   mframe 
# FILE:	       R/dfMetadata.R
# DESCRIPTION: S3 class to represent data frame variables' metadata  
# AUTHORS: 	   Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  2009 May 25
# LICENSE:     GPL-2
################################################################################
# TODO: Add examples to all the documentation
# TODO: Add validation / hooks infrastructure (?)
# TODO: Explore usage of TypeInfo S4 package/class to model the metadata (?)

#' Creates objects of the S3 class \code{dfMetadata} that represent data frame 
#' metadata. This metadata consists of a specification for each variable of the 
#' data frame (using class \code{\link{varMetadata}}), plus constraints like 
#' a primary key (row uniqueness), or whether additional variables are allowed.   
#' 
#' Data frame metadata can be used in several ways:
#' \enumerate{
#' 	\item to create new data frames according to a specification with minimal 
#' 		coding.
#' 	\item to verify that a given data frame fulfills a specification. 
#' 	\item together with the \code{\link{mframe}} class, to create specialized 
#' 		data frames on which a specification is enforced. 		
#' }    
#'  
#' \section{Methods}{
#' \link[base:UseMethod]{Methods} to \link[base:InternalMethods]{standard 
#' generics} for "\code{dfMetadata}" objects currently include: 
#' \code{\link[coerce.dfMetadata]{coercion}} to \code{list} 
#' and \code{data.frame}, \code{\link[print.dfMetadata]{print}}, 
#' \code{\link[Extract.dfMetadata]{extract and replace operators}} (\code{[},
#' \code{[[} and \code{\{}), \code{\link[c.dfMetadata]{c}} and 
#' \code{\link[namesReplace.dfMetadata]{names<-}}.
#'  
#' \code{as.dfMetadata} is a generic function, with a default implementation
#' and methods for \code{list}s and \code{data.frame}s. 
#' \link[base:UseMethod]{Methods} can be added for new classes.
#'  
#' While the subset operators \code{[}, \code{[[} and \code{\{} can be used
#' to directly modify each variable's metadata, methods are also provided
#' to get and replace individual components of several variables' metadata
#' at once (in a "cross-sectional" way): 
#' \code{\link{varType}}, \code{\link{varCoerce}},
#' \code{\link{naOk}} and \code{\link{varDefault}}. 
#' 
#' In addition, generic functions and methods are provided to set and
#' replace the \code{\link{key}} and other options (XXX).
#' }
#' 
#' @title Data Frame Metadata
#' @aliases dfMetadata is.dfMetadata as.dfMetadata
#' @param \dots variables variables' specifications for the data frame, in the  
#'  form \code{tag = value}, where \code{tag} is the variable name, and  
#'  \code{value} is a \code{\link{varMetadata}} or any object that can be  
#'  coerced to one (such as a character vector of class names, or a list
#'  with elements like "type", "coerce", "na.ok", etc.)
#'     
#' 	If only one \emph{unnamed} list is specified, its contents are assumed to 
#'  contain the variables specification. This can be used to avoid name 
#'  collisions between the \code{dfMetadata} function argument names 
#'  \code{na.ok} or \code{default} and variables having the same name.
#'
#' @param type specification of the variables types, in any of the forms 
#'  accepted by method \code{\link[varType]{varType<-}}.
#'  If specified, this value takes precedence over the \code{type} 
#'  component of the variable specification in \dots.
#'
#' @param coerce specification of the variables coercion behaviour, in any of 
#'  the forms accepted by method \code{\link[varCoerce]{varCoerce<-}}.
#'  If specified, this value takes precedence over the \code{coerce} 
#'  component of the variable specification in \dots.
#'    
#' @param na.ok specification of which variables accept or reject missing
#'  (\code{NA}) values, in any of the forms accepted by method
#'  \code{\link[naOk]{naOk<-}}.
#'  If specified, this value takes precedence over the \code{na.ok} 
#'  component of the variable specification in \dots.
#' 
#' @param default specification of default values for the variables, in any of 
#'  the forms accepted by method \code{\link[varDefault]{varDefault<-}}.
#'  If specified, this value takes precedence over the \code{default} 
#'  component of the variable specification in \dots.
#'   
#' @param key specification of the variables that compose the 
#' 	primary key of the table, if there is one, in any of the forms accepted by  
#'  method \code{\link[key]{key<-}}.
#'  If a key is provided and this \code{dfMetadata} is associated with a 
#'  \code{mframe}, an error will be raised whenever there 
#'  is more than one row in the \code{mframe} with the same value of all 
#'  variables that compose the primary key (coerced to character and 
#'  concatenated, see \code{\link{keys}}).  
#' 
#' @param allow.new.columns logical flag. Are additional variables not included 
#'  in the specification allowed?  
#' @return An object of class \code{dfMetadata}.
#' @note The class implementation can change in future versions. Instead of  
#' 	directly manipulating the data structure, always use the following methods 
#' 	to retrieve and modify the object components: 
#' 	\code{\link{[}}, \code{\link{varType}}, \code{\link{varCoerce}}, 
#' 	\code{\link{key}}, etc. 
#' 
#' @seealso Classes \code{\link{varMetadata}} and \code{\link{mframe}}.
#' @export

dfMetadata <- function(..., type, coerce, na.ok, default, key=NULL, 
		allow.new.columns=TRUE) {
	colsSpec <- list(...);
	if (length(colsSpec) == 0L) 
		return(structure(list(), class="dfMetadata", allow.new.columns=TRUE))
	else if (length(colsSpec) == 1L && is.null(names(colsSpec)) && 
			is.list(colsSpec[[1L]]) && !is.varMetadata(colsSpec[[1L]])) 
		colsSpec <- colsSpec[[1]];
	
    result <- structure(list(), class="dfMetadata");	
	result[] <- colsSpec;	

	if (!missing(type)) varType(result) <- type;	
	if (!missing(coerce)) varCoerce(result) <- coerce;
	if (!missing(na.ok)) naOk(result) <- na.ok;
	if (!missing(default)) varDefault(result) <- default;
	
	allowNewColumns(result) <- allow.new.columns;	
	key(result) <- key;
	
	result;		
}

#' @nord
#' @export 

is.dfMetadata <- function(x)
	inherits(x, "dfMetadata");

#' @nord
#' @S3method as.dfMetadata default

as.dfMetadata.default <- function(x) 
	dfMetadata(x);

#' @nord
#' @S3method as.dfMetadata dfMetadata

as.dfMetadata.dfMetadata <- function(x) x;
    
#' @nord
#' @S3method as.dfMetadata list

as.dfMetadata.list <- function(x) 
	do.call("dfMetadata", varType(x));	

#' @nord
#' @S3method as.dfMetadata data.frame

as.dfMetadata.data.frame <- function(x) 
	do.call("dfMetadata", varType(x));	

#' @nord
#' @S3method as.data.frame dfMetadata

as.data.frame.dfMetadata <- function(x) {
	if (length(x) == 0L) 
		data.frame(
			var = character(0),
			type = factor(character(0)),
			# coerce = character(0),
			na.ok = logical(0),
	    	default = character(0),
			key = factor(character(0), levels=c("*", "")),
			row.names = NULL,
			stringsAsFactors = FALSE
		)		
	else { 
		# Putting defs outside the data.frame() call produces better
    	# error message when varType is not character and varCoerce is
		# undefined...
		defs <- sapply(varDefault(x), deparse, nlines=1);
		result <- result <- data.frame(
			var = names(x),
			type = sapply(varType(x), deparse, nlines=1),
			coerce = sapply(varCoerce(x), deparse, nlines=1),
			na.ok = naOk(x),
	    	default = defs,
			key = factor(ifelse(names(x) %in% key(x), "*", "")),
			row.names = NULL,
			stringsAsFactors = FALSE
		)		
		userMetaNames <- unique(unlist(sapply(x, names)));
		for (i in userMetaNames) 
			result[[i]] <- sapply(x, function(y, j) 
					if (is.null(y[[j]])) "" else deparse(y[[j]], nlines=1),
				j=i)
		result;		
	}
} 

#' @nord
#' @S3method as.list dfMetadata

as.list.dfMetadata <- function(x) {
	customAttributes(x) <- NULL;
	x;
} 
	
#' \code{\link[base:print]{Prints}} objects of class \code{\link{dfMetadata}}.
#' 
#' @title Print Data Frame Metadata
#' @param x an object of class \code{dfMetadata}.
#' @param style string specifying the printing style, which can be "compact" 
#' 	(the default) or "list". The "compact" style uses less space using
#'  \code{\link[coerce.dfMetadata]{as.data.frame}}, while the "list" style
#'  prints the metadata as a list.  
#' @param title logical flag. Whether to print a first line with the
#'  object class.
#' @param \dots further arguments passed to the \code{print} method for 
#' 	\code{list}s of \code{data.frame}s.
#' 
#' @return \code{x}, invisibly. Invoked for its side effect of printing
#'  a character representation of \code{x} to the console.
#' @seealso \code{\link{dfMetadata}}.
#' @S3method print dfMetadata

print.dfMetadata <- function(x, style="compact", title=TRUE, ...) {
	style <- match.arg(style, c("compact", "list"));
	if (title)
		cat("[Data Frame Metadata]\n");
	if (style == "list") {
		print(unclass(x), ...);
		return(invisible(x));
	}
	
	print(as.data.frame(x), ...);
	cat("\nOptions: Allow New Columns = ", allowNewColumns(x), "\n", sep="");
	
	invisible(x);
}

#' @nord
#' @S3method varType dfMetadata

varType.dfMetadata <- function(x, vars) {
	result <- lapply(x, varType);
	if (invalid(vars)) return(result);
	
	result <- result[valIndex(vars, x)];	
	if (length(vars) == 1L) result[[1L]] else result;
}

#' @name varTypeReplace.dfMetadata
#' @nord
#' @S3method `varType<-` dfMetadata

`varType<-.dfMetadata` <- function(x, vars, value) {
	varNames <- if (missing(vars))
		names(x)
	else
		valIndex(vars, x, retclass="character");	
	if (is.null(value)) {
    	value <- rep(list(NA_character_), length(varNames));
		names(value) <- varNames;
	} else {
		if (!is.list(value))
			value <- if (is.function(value) || is.language(value))
				list(value)
			else as.list(value)
		value <- valParamList(value, varNames, n=NULL, recycle=TRUE, 
			all.nams=FALSE);
		vars <- varNames <- names(value);
	}		

	if (missing(vars))
		result <- mapply(`varType<-`, x=x, value=value, SIMPLIFY=FALSE)
	else {
		result <- unclass(x);
		result[varNames] <- 
			mapply(`varType<-`, x=x[varNames], value=value, SIMPLIFY=FALSE);
	}	
	attributes(result) <- attributes(x);
	result;
}

#' @nord
#' @S3method varCoerce dfMetadata

varCoerce.dfMetadata <- function(x, vars) {
	result <- lapply(x, varCoerce);
	if (invalid(vars)) return(result);
	
	result <- result[valIndex(vars, x)];	
	if (length(vars) == 1L) result[[1L]] else result;
}

#' @name varCoerceReplace.dfMetadata
#' @nord
#' @S3method `varCoerce<-` dfMetadata

`varCoerce<-.dfMetadata` <- function(x, vars, value) {
	varNames <- if (missing(vars))
		names(x)
	else
		valIndex(vars, x, retclass="character");
	
	if (is.null(value)) {
    	value <- vector(mode="list", length=length(varNames));
		names(value) <- varNames;
	} else {
		if (!is.list(value))
			value <- if (is.function(value) || is.language(value))
				list(value)
			else as.list(value);
		value <- valParamList(value, varNames, n=NULL, recycle=TRUE, 
			all.nams=FALSE);
		vars <- varNames <- names(value);
	}		

	if (missing(vars))
		result <- mapply(`varCoerce<-`, x=x, value=value, SIMPLIFY=FALSE)
	else {
		result <- unclass(x);
		result[varNames] <- 
			mapply(`varCoerce<-`, x=x[varNames], value=value, SIMPLIFY=FALSE);
	}	
	attributes(result) <- attributes(x);
	result;
}

#' @nord
#' @S3method naOk dfMetadata

naOk.dfMetadata <- function(x, vars) { 
	result <- sapply(x, naOk);
	if (invalid(vars)) result else result[valIndex(vars, x)];
}

#' @name naOkReplace.dfMetadata 
#' @nord
#' @S3method `naOk<-` dfMetadata

`naOk<-.dfMetadata` <- function(x, vars, value) {
	varNames <- if (missing(vars))
		names(x)
	else
		valIndex(vars, x, retclass="character");
	
	value <- if (is.null(value)) 
		rep(NA, length.out=length(varNames))
	else if (is.logical(value)) 
		rep(value, length.out=length(varNames))				
	else if (is.character(value)) {
		if (any(isInvalid <- !value %in% varNames))
			stop(sprintf(ngettext(sum(isInvalid),
				"na.ok? variable %s not found in the specification",
				"na.ok? variables %s not found in the specification"),
				dqMsg(vars[isInvalid])));
		varNames %in% value;    		
	} else if (is.list(value)) {
		value <- valParamList(value, varNames, recycle=TRUE, all.nams=FALSE);
		vars <- varNames <- names(value);
		value
	} else
		stop(gettext("na.ok? specification is not valid"));
 
	if (missing(vars))
		result <- mapply(`naOk<-`, x=x, value=value, SIMPLIFY=FALSE)
	else {
		result <- unclass(x);
		result[varNames] <- 
			mapply(`naOk<-`, x=x[varNames], value=value, SIMPLIFY=FALSE);
	}	
	attributes(result) <- attributes(x);
	result;
}

#' @nord
#' @S3method varDefault dfMetadata
# NOTE: There's a 'defaults' function in the 'plyr' package which sets
# list defaults (similar to 'modifyList' but not recursive). It can be
# compatible with this 'defaults' by making it generic and 
# defaults.list <- plyr:::defauls.

varDefault.dfMetadata <- function(x, vars, eval=FALSE) {
	result <- lapply(x, varDefault, eval=eval);
	if (invalid(vars)) return(result);
	
	result <- result[valIndex(vars, x)];	
	if (length(vars) == 1L) result[[1L]] else result;
}

#' @name varDefaultReplace.dfMetadata
#' @nord
#' @S3method `varDefault<-` dfMetadata

`varDefault<-.dfMetadata` <- function(x, vars, value) {
	varNames <- if (missing(vars))
		names(x)
	else
		valIndex(vars, x, retclass="character");
	
	if (is.null(value))
    	value <- structure(vector(mode="list", length(varNames)), 
			names=varNames)
	else {
		if (!is.list(value))
			value <- if (is.function(value) || is.language(value))
				list(value)
			else as.list(value)
		value <- valParamList(value, varNames, n=NULL, recycle=TRUE, 
			all.nams=FALSE);
		vars <- varNames <- names(value);
	}		

	if (missing(vars))
		result <- mapply(`varDefault<-`, x=x, value=value, SIMPLIFY=FALSE)
	else {
		result <- unclass(x);
		result[varNames] <- 
			mapply(`varDefault<-`, x=x[varNames], value=value, SIMPLIFY=FALSE);
	}	
	attributes(result) <- attributes(x);
	result;
}

#' @nord
#' @S3method varMeta dfMetadata

varMeta.dfMetadata <- function(x, vars, i) {
	if (!missing(vars)) x <- x[vars];
	if (missing(i))
		lapply(x, varMeta)
	else 
		lapply(x[vars], varMeta, i=i)
}

#' @name keyReplace.dfMetadata
#' @nord
#' @seealso \code{\link{key}}.
#' @S3method `key<-` dfMetadata

`key<-.dfMetadata` <- function(x, value) {
	x <- NextMethod();
	xKey <- attr(x, "key");
	if (!is.null(xKey) && !is.character(xKey))
		attr(x, "key") <- names(x)[xKey];
	x;
}

#' Extract or replace \link[varMetadata]{variable metadata} elements from 
#' objects of class \code{\link{dfMetadata}}.   
#' 
#' These operators behave mostly as their \link[base:Extract]{counterparts}
#' for lists, with the following differences:
#' \enumerate{
#' 	\item Each element of the right-hand-side in replacement expressions 
#'  	(\code{value}) is always coerced to class \code{\link{varMetadata}}, 
#' 		stopping if the coercion is not succesful.
#' 		For replacing multiple elements, the \code{value} must be a \emph{list}
#' 		of \code{varMetadata} objects, or something that can be coerced to it,
#' 		and is recycled as necessary.
#'  \item Out-of-range elements cannot be selected. If an index points to 
#' 		some element outside the list, an error is raised (lists do return
#' 		\code{NULL} instead, which is often a source of hard-to-find bugs).
#'  \item When out-of-range elements are replaced (the equivalent of
#' 		adding new elements), unnamed new elements get their name either:
#'  	1) from the index \code{i} if it is a character vector; 2) from the  
#'  	name of the matching item in the \code{value}; or 3) are automatically 
#'  	assigned as "\code{X5}", "\code{X6}", etc., where the numbers 
#' 		correspond to their variable position. 
#' 		Duplicated names are not allowed.    
#' 	\item If the metadata has a \link{key} specification, it is removed 
#'  	whenever the subset does not contain some of the variables that compose 
#' 		the key, and kept if the subset contains all the variables in the key.
#'  \item Metadata attributes like "allow.new.columns" are not dropped
#' 		when subsetting.
#'  \item Recursive indexing using a vector as argument for \code{[[} is not 
#' 		supported.  
#' }
#' 
#' @title Extract or Replace Parts of a Data Frame's Metadata
#' @name Extract.dfMetadata
#' @aliases [.dfMetadata [<-.dfMetadata [[.dfMetadata [[<-.dfMetadata 
#' 	$.dfMetadata $<-.dfMetadata
#' @usage 
#' \method{[}{dfMetadata}(x, i)
#' 
#' \method{[}{dfMetadata}(x, i) <- value
#' \method{[[}{dfMetadata}(x, i, exact) <- value
#' 
#' @param x an object of class \code{dfMetadata}.
#' @param i index specifying elements to extract or replace. Follows the
#' 	sames rules as for lists.
#' @param exact logical flag; controls possible partial matching of \code{[[} 
#'  when extracting by a character vector. The default is no partial matching. 
#'  Value \code{NA} allows partial matching but issues a warning when it occurs. 
#'  Value \code{FALSE} allows partial matching without any warning.
#' @param value a suitable replacement value. This must be coercible to a
#' 	\code{varMetadata} when \code{i} indexes a single element, or to a list
#'  of \code{varMetadata} objects when \code{i} indexes more than one 
#'  element.
#'  It will be repeated ("recycled") as necessary. A warning is issued if
#'  repetition is not done a whole number of times.
#'  If \code{NULL}, deletes all variables selected in the index.  
#' 
#' @return A (possibly subsetted) object of class \code{dfMetadata}. 
#' @seealso \code{\link{varMetadata}} and 
#' 	\code{\link[Extract.varMetadata]{[.varMetadata}}. 
#' @S3method `[` dfMetadata

`[.dfMetadata` <- function(x, i) {	
	if (missing(i))  
		return(x);
		
	xAttr <- customAttributes(x);	
	x <- .subset(x, valIndex(i, x));
	attributes(x)[names(xAttr)] <- xAttr;
	
	xKey <- key(x);
	if (!is.null(xKey) && any(invalidCol <- !xKey %in% names(x))) 	
    	key(x) <- NULL;
	
	x;	
}

#' @name Replace.dfMetadata
#' @nord
#' @S3method `[<-` dfMetadata

`[<-.dfMetadata` <- function(x, i, value) {
	if (missing(i)) {
		varNames <- names(value);
		if (any(unnamed <- !hasNames(value)))
    		varNames[unnamed] <- paste("X", which(unnamed), sep=""); 	

		xAttr <- customAttributes(x);
		customAttributes(x) <- NULL;		
		x <- lapply(value, as.varMetadata);
		names(x) <- varNames;		
		attributes(x)[names(xAttr)] <- xAttr;
	} else {
		ilen <- length(i);		
		if (ilen == 0L) return(x);
		if (!is.null(value)) {
			if (ilen == 1L && (length(value) > 0L || is.list(value))) 
				value <- list(value)
			else if (!is.list(value)) 
				value <- if (is.function(value) || is.language(value))				
					list(value)
				else as.list(value);
			if (ilen > 1L)
				value <- valLength(value, ilen, recycle=TRUE);
			varNames <- if (is.character(i)) i else names(x)[i];		
			if (any(unnamed <- is.na(varNames) | varNames == "") && 
					!is.null(names(value))) 			
				varNames[unnamed] <- names(value)[unnamed];			
		}
				
		xAttr <- customAttributes(x);
		customAttributes(x) <- NULL;
		if (is.null(value)) 
			x[i] <- NULL 
		else {
			x[i] <- lapply(value, as.varMetadata);
			if (!is.character(i))
				names(x)[i] <- varNames;
	    	x[sapply(x, is.null)] <- NULL;	# Removes non-contiguous items when
											# adding elements by numeric index		
			if (any(unnamed <- !hasNames(x)))
	    		names(x)[unnamed] <- paste("X", which(unnamed), sep="");					
 		}		
		attributes(x)[names(xAttr)] <- xAttr;
	}

	if (any(duplicated(names(x))))
		stop("all variables must have a unique name");
	
	xKey <- key(x);
	if (!is.null(xKey) && any(invalidCol <- !xKey %in% names(x))) 	
    	key(x) <- NULL;	
	x;
}

#' @name Extract2.dfMetadata
#' @nord
#' @S3method `[[<-` dfMetadata

`[[<-.dfMetadata` <- function(x, i, exact=FALSE, value) {
	i <- valIndex(i, x, several.ok=FALSE, handler=NULL);
	x[i] <- value;
	x;
}

#' @nord
#' @title Allow New Columns
#' @export allowNewColumns

allowNewColumns <- function(x)  
	attr(x, "allow.new.columns");

#' @nord
#' @title Set Allow New Columns
#' @name allowNewColumnsReplace.dfMetadata
#' @export `allowNewColumns<-`
#
# Set the component \code{AllowNewColumns}. If this is an invalid value
# (\code{NA} or a zero-length object), the current component value is not
# touched, unless it is undefined, in which case it is set to the default
# value \code{TRUE}.

`allowNewColumns<-` <- function(x, value) {
	value <- if (length(value)) as.logical(value[1]) else NA;
	
	if (is.na(value)) {
		if (invalid(attr(x, "allow.new.columns")))
    		attr(x, "allow.new.columns") <- TRUE
	} else attr(x, "allow.new.columns") <- value;
	
	x;
}
	
#' @nord
#  Not exported.

customAttrNames.dfMetadata <- function(x, class=TRUE) {
	result <- c("key", "allow.new.columns");
	if (class) c("class", result) else result; 
}

#' Set the variables names of objects of classs \code{\link{dfMetadata}}.
#'  
#' This is the method for the generic \code{\link[base:names]{names<-}}
#' applied to data frame metadata objects of class \code{dfMetadata}.
#' 
#' This works mostly like for data frames, except that:
#' \enumerate{
#' 	\item All names must be defined: \code{NA}s and empty strings "" are not
#' 		allowed.
#' 	\item No duplicated names are allowed.
#' 	\item If a variable name is a part of the \code{\link{key}}, the variable
#' 		name is also modified in the key specification.
#' }
#'  
#' @title Set Data Frame's Metadata Variables Names
#' @name namesReplace.dfMetadata
#' @aliases names<-.dfMetadata
#' @usage 
#' \method{names}{dfMetadata}(x) <- value
#' 
#' @param x an object of class \code{dfMetadata}.
#' @param value a character vector of up to the same length as \code{x}.
#'   
#' @seealso \code{\link[base:names]{names}}.
#' @S3method `names<-` dfMetadata

`names<-.dfMetadata` <- function(x, value) {
	oldNames <- names(x);	
    x <- NextMethod();
	
	value <- names(x);	
	if (is.null(value) && length(x))
    	stop("all variable names must be defined")
	else if (!is.null(value) && 
			any(is.na(value) | value == "" | duplicated(value)))
		stop("all variable names must be defined and unique");		

	if (!is.null(key(x)) && !is.null(oldNames))
		key(x) <- value[match(key(x), oldNames)];	
	x;
}

#' This is the \code{\link{c}} method for data frame metadata. It concatenates
#' all the supplied variable metadata into a single \code{dfMetadata}.
#' An error is raised if some column name is duplicated.
#' No attributes are dropped (i.e. \code{c(x)} equals \code{x}).
#' 
#' Both the resulting key and the "allow new columns" spec are always taken  
#' from the first \code{dfMetadata} object being combined.  
#'  
#' @title Combine Data Frame Metadata
#' @usage \method{c}{dfMetadata}(\dots)
#' @param \dots objects to be concatenated. They are all coerced to 
#'  \code{dfMetadata}.
#' 
#' @return An object of class \code{dfMetadata}. 
#' @seealso \code{\link[base]{c}}.
#' @S3method c dfMetadata

`c.dfMetadata` <- function(...) {
	dots <- list(...);
	result <- do.call("c", lapply(dots, function(x) unclass(as.dfMetadata(x))));
	dfMetadata(result, key=key(dots[[1L]]), 
		allow.new.columns=allowNewColumns(dots[[1L]]));
}