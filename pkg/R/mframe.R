################################################################################
# R PACKAGE:   mframe
# FILE:        R/mframe.R
# DESCRIPTION: S3 class 'mframe' 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  25/05/2009
# LICENSE:     GPL-2
################################################################################
# TODO: mframe() function is too long, refactorize it
# TODO: Improve documentation of how to extend mframe. 
# TODO: Allow 'default' expressions to reference other mframe variables
# TODO: Warn if new data needs to be re-coerced to the variable type.
#		Or maybe only if NAs arise from the operation?
# TODO: Implement an easier way to get & set user-defined varMetadata elements
#		from an mframe. Maybe meta(x)[1, "label", simplify=TRUE]

#' Function \code{mframe} creates objects of the S3 class \code{"mframe"} that
#' represent data frames with associated metadata. 
#' 
#' \code{mframe}s are standard \code{\link{data.frame}}s extended to include
#' accompanying metadata. Metadata is represented with an object of class 
#' \code{\link{dfMetadata}} and can be used to store arbitrary information 
#' about each variable in the data frame, but also to set some useful   
#' constraints on the data frame behaviour, such as:
#' \enumerate{
#' 	\item (pre)fix the class of the variables (columns), so that new or   
#' 		modified values are automatically coerced to the target class
#' 		even with operators like \code{$}.
#' 	\item ensure constraints per-variable ("only allow positive numeric values 
#' 		for variable X") 
# 	 or intra-variable ("variable \code{Start} must be earlier than variable 
#    \code{End}"). 
#'  \item allow or disallow missing (\code{NA}) values on a per-column basis.
#'  \item set default values for each variable, which can be arbitrary
#' 		expressions or functions evaluated on addition (e.g. timestamps.)
#  \item enforce the set of variable names and its order on the data frame (?). 
#'  \item prevent adding new variables that are not defined on the metadata 
#' 		specification. 
#'  \item establish row uniqueness constraints ("primary keys").
#' }
#' 
#' So, an \code{mframe} consists of two objects: a standard data frame and
#' its metadata (a "\code{\link{dfMetadata}}"). 
#' The main component of the metadata is a specification of each
#' variable in the data frame (of class \code{\link{varMetadata}}), but it 
#' also includes options that apply to the whole data frame --e.g. the key.
#'
#' Variables' specification in the metadata are always synchronized 
#' with the data frame: subsetting the data frame also subsets the metadata,
#' so that the number of columns in the data frame always equals the number
#' of variables in the metadata. All standard methods are overriden to 
#' guarantee this behaviour. It is also guaranteed that the data in the
#' data frame complies with all constraints specified in the metadata. 
#' 
#' A most common usage of \code{mframe}s arises when a data frame needs
#' to be specialized to a specific meaning, so that partially constraining the 
#' flexibility provided by data frames is desired, but the constraints or 
#' coding requirements of a formal S4 class are not handy. 
#' \code{mframe}s are also useful in many situations to simplify the code 
#' for many common tasks done with data frames, or to prevent some common
#' bugs.
#' 
#' \section{Methods}{
#' \link[base:UseMethod]{Methods} to \link[base:InternalMethods]{standard 
#' generics} for "\code{mframe}" objects currently include: 
#' \code{\link[coerce.mframe]{coercion}} to \code{data.frame}s 
#' and \code{dfMetadata}, \code{\link[print.mframe]{print}}, 
#' \code{\link[Extract.mframe]{extract and replace operators}} (\code{[},
#' \code{[[} and \code{\{}), \code{\link[cbind.mframe]{cbind}} and 
#' \code{rbind}, and \code{\link[namesReplace.mframe]{names<-}}.
#'
#' \code{as.mframe} is a generic function with a default implementation. 
#' \link[base:UseMethod]{Methods} can be added for new classes.
#'  
#' Methods that apply to \code{\link{dfMetadata}} can also be applied directly 
#' to \code{mframe}s, and they all ensure that the changes to the metadata are 
#' applied to the data, enforcing any new constraints or raising an error if 
#' they cannot be enforced: see \code{\link{varType}}, \code{\link{varCoerce}},
#' \code{\link{naOk}}, \code{\link{varDefault}}, and \code{\link{key}}. 
#' }
#' 
# \section{Subclasses (Extending \code{mframe})}{
# By default, metadata is kept together with the data. 
# But it can also be
# set in a generic way by subclassing \code{mframe}. Subclassing is simple:
# just override the \code{metadata} method for the new class returning
# a \code{\link{dfMetadata}} object that defines the \code{mframe}, but
# prevents modifying the metadata on the fly.
# }
#' @title Data Frames with Metadata
#' @aliases mframe is.mframe as.mframe as.mframe.default
#' @usage mframe(\dots, row.names = NULL, check.rows = FALSE, check.names = TRUE, 
#' 		stringsAsFactors=default.stringsAsFactors(), nrow = NULL, 
#'		class = "mframe", metadata = NULL)
#' 
#' 	\method{as.mframe}{default}(x, class = "mframe", metadata)
#'  is.mframe(x, strict = FALSE)
#' 
#' @param \dots each of the variables (columns), either in \code{value} or in 
#' 	\code{name = value} form. 
#'  Component names are matched against the metadata variables names, first by 
#'  matching the \code{name}, then for missing tags by order. Unmatched names
#'   are considered extra columns, which may generate an error if they're 
#'  disallowed by the metadata.
#'  If a single list or data frame is specified, its components are used
#'  one by one. 
#' @param row.names \code{NULL} or a single integer or character string 
#' 	specifying a column to be used as row names, or a character or integer 
#'  vector giving the row names for the data frame. 
#'  See \code{\link{data.frame}}.
#' @param check.rows logical flag. If \code{TRUE} then the rows are checked 
#' 	for consistency of length and names. 
#'  See \code{\link{data.frame}}. 
#' @param check.names logical flag. If \code{TRUE} then the names of the 
#'  variables in the data frame are checked to ensure that they are 
#'  syntactically valid variable names and are not duplicated.
#'  See \code{\link{data.frame}}. 
#' @param stringsAsFactors logical flag: should character vectors be 
#' 	converted to factors?
#'  See \code{\link{data.frame}}. 
#' @param nrow integer. Number of rows of the resulting frame. This is useful
#'  to create empty frames (with zero rows, or with rows where all values
#'  have been filled by the default value), or to recycle the provided 
#'  values. 
#'  \code{nrow} is ignored if some component has a higher length.  
#' @param class string. Subclass of \code{mframe} to create. A 
#'  \code{metadata} method must exist for that class. If a class is not
#'  specified, or this is just \code{mframe}, the metadata is stored together
#'  with the data.
#' @param metadata an object of class \code{dfMetadata}, or that can be
#'  coerced to one. This should be specified when \code{class} is not set.
#'  If neither \code{class} nor \code{metadata} is provided, the metadata
#'  is inferred from the data themselves.
#' @param strict logical flag. If \code{TRUE}, tests that \code{x} is an
#' 	\code{mframe} and not of any class that extends \code{mframe}. If 
#'  \code{FALSE}, any subclass of \code{mframe} makes \code{is.mframe} 
#'  return \code{TRUE}.
#' @param x an \R object.     
#'   
#' @return For \code{mframe} and \code{as.mframe}, an object of class 
#'  \code{mframe}, which is a data frame with accompanying metadata.
#'  
#'  For \code{is.mframe}, a logical of length one telling whether
#'  \code{x} is exactly an \code{"mframe"} (if \code{strict = TRUE}) or
#'  any subclass of it (if \code{strict = FALSE}). 
#'  
#' @seealso \code{\link{data.frame}}. 
#'  Class \code{\link[Biobase]{AnnotatedDataFrame}} in package \pkg{Biobase}
#'  is an S4 implementation of similar ideas, and allows to add descriptions 
#'  of each variable to a data frame 
#'  
#'  Package \pkg{\link[data.table]{data.table}} provides an alternative
#'  to data frames optimized for speed of access with subsetting operations, 
#'  and also includes the notion of a (primary) key.
#' 
#' @export

mframe <- function(..., row.names=NULL, check.rows=FALSE, 
		check.names=TRUE, stringsAsFactors=default.stringsAsFactors(), 
		nrow=NULL, class="mframe", metadata=NULL) {
	subclass <- class[!class %in% c("mframe", "data.frame")];
	class <- c(subclass, c("mframe", "data.frame"));
	
	x <- list(...);
	# Allow to pass a single argument that is a list or data.frame, instead
	# of each column individually	
	if (length(x) == 1L && is.list(x[[1L]]))  
		x <- x[[1L]];
	xLen <- length(x);
	xNames <- names(x);
		
	if (length(subclass) > 0L) {
		classMetadata <- metadata(subclass[1]);
		if (!invalid(classMetadata)) {
			if (!invalid(metadata))
				warning(gettextf(
					"supplied metadata ignored for class %s", 
					dqMsg(subclass[1])));
			metadata <- classMetadata;
 		}
	} 
	
	if (missing(metadata) || is.null(metadata)) 
		metadata <- as.dfMetadata(x)
	else if (!is.dfMetadata(metadata))
		metadata <- as.dfMetadata(metadata);
	metaVarNames <- names(metadata);
	
	# Determine number of rows
	xRows <- if (xLen == 0L) 0L else max(sapply(x, length, USE.NAMES=FALSE));	
	if (length(nrow) > 0L) {
		nrow <- as.integer(nrow)[1L];
		if (xRows > nrow) {
			warning(gettextf("%s smaller than number of rows, it is ignored", 
				sqMsg("nrow")));
			nrow <- xRows;
		}
	} else nrow <- xRows;

	if (nrow == 0L) {
		x <- rep(list(logical(0)), length.out=length(metadata));
		xLen <- length(x);
		xNames <- NULL;
	} else if (xRows == 0L) {
		x <- varDefault(metadata, eval=TRUE);
		xLen <- length(x);
		xNames <- names(x);
	}
	
	# If elements are not named, use the metadata column names
	if (xLen > 0L && any(!hasNames(x))) 
		names(x) <- xNames <- emptyNamesFrom(x, metaVarNames)

	# Remove columns not in the metadata, if they're not allowed
	isInMeta <- xNames %in% metaVarNames;
	allVars <- if (allowNewColumns(metadata))  
		c(metaVarNames, xNames[!isInMeta])
	else metaVarNames;
		
	if (!allowNewColumns(metadata) && any(!isInMeta)) {
		warning(sprintf(ngettext(sum(!isInMeta),
			"variable %s has been removed from %s",
			"variables %s have been removed from %s"),
			dqMsg(names(x)[!isInMeta]), sqMsg("x")));
		x <- x[isInMeta];
		xNames <- names(x);
	}
	
	# Ensure, for each column, that:
	# 	1) it's assigned its default value if missing from the input
	# 	2) its values are recycled as necessary
	#	3) matches the metadata column class & constraints, or coerce
	#	   it as necessary. 	
	isInMeta <- allVars %in% metaVarNames;
	isInX <- allVars %in% xNames;
	
	for (i in seq_along(allVars)) {
		varName <- allVars[i];
		if (isInMeta[i]) {
    		metaVar <- metadata[[varName]];
			col <- varDefault(metaVar, eval=TRUE);
		} else col <- NA;
    	
		if (isInX[i]) 
			col <- x[[varName]] 
    	
		if (isInMeta[i]) {
			col <- enforce(metaVar, col);
			if (!naOk(metaVar) && any(is.na(col)))
				stop(gettextf(
					"NA values snot accepted for variable %s",
					dqMsg(varName)));
		}
		
		# Recycle data if required. This is done here as the call to
		# data.frame below doesn't recycle some atomic classes such as
		# Date or POSIXct/lt.
		if (length(col) < nrow)
			col <- rep(col, length.out=nrow);
		
		x[[varName]] <- col;		
	}
	
	x <- x[allVars];

	x <- data.frame(x, row.names=row.names, check.rows=check.rows,
		check.names=check.names, stringsAsFactors=FALSE);
	class(x) <- class;
	if (length(subclass) == 0L)
		attr(x, "metadata") <- metadata;	
	
	# Verify primary key constraints
	if (!is.null(metadata)  && !is.null(metaPk <- key(metadata))) {
		xPk <- keys(x, vars=metaPk); 
		if (any(isDup <- duplicated(xPk))) 
			stop(sprintf(ngettext(sum(isDup),
				"duplicated row %s breaks primary key constraint: %s",
				"duplicated rows %s break primary key constraint: %s"), 
				nqMsg(which(isDup)), dqMsg(xPk[isDup])));
	}
		
	x;
}

#' @nord
#' @export

is.mframe <- function(x, strict=FALSE) {
    if (strict) 
		inherits(x, "mframe", which=TRUE) == 1L
	else inherits(x, "mframe");
}

#' @nord
#' @S3method as.mframe default

as.mframe.default <- function(x, class="mframe", metadata) 
	mframe(x, class=class, metadata=metadata);

#' @nord
#' @S3method as.data.frame mframe

as.data.frame.mframe <- function(x) {
	customAttributes(x, class=FALSE) <- NULL;
	class(x) <- "data.frame";
	x;
}

#' @nord
#' @S3method customAttrNames mframe

customAttrNames.mframe <- function(x, class=TRUE) 
	if (class) c("class", "metadata") else "metadata";

#' \code{\link[base:print]{Prints}} objects of class \code{\link{mframe}}.
#'
#' \code{mframe}s are printed like data frames, with two extra rows at
#' the beginning that show information from the metadata: the variable type 
#' and wheter \code{NA}s are allowed. Other metadata information is not
#' shown and can be printed using \code{metadata(x)}.
#'   
#' @title Print Specialized Data Frames
#' @param x an object of class \code{mframe}.
#' @param style string specifying the printing style, which can be "compact" 
#' 	(the default) or "list". The "compact" style uses less space using
#'  \code{\link[coerce.dfMetadata]{as.data.frame}}, while the "list" style
#'  prints the metadata as a list.  
#' @param \dots further arguments passed to \code{\link{print}}.
#' 
#' @return \code{x}, invisibly. Invoked for its side effect of printing
#'  a character representation of \code{x} to the console.
#' @seealso \code{\link{mframe}}.
#' 
#' @title Print Specialized Data Frames
#' @S3method print mframe

# NOTE: Code copied from base::print.data.frame, adding the class & required 
# 		rows at the head of the printed result

print.mframe <- function(x, ..., digits=NULL, quote=FALSE, right=TRUE, 
    	row.names=TRUE) {
	n <- length(row.names(x))
	if (length(x) == 0L) {
		cat(gettextf("Specialized data frame with 0 columns and %d rows\n", n))
	} else if (n == 0L) {
		print.default(names(x), quote=FALSE)
		cat(gettext("Specialized data frame <0 rows> (or 0-length row.names)\n"))
	} else {
		m <- as.matrix(format.data.frame(x, digits=digits, na.encode=FALSE))
		if (!isTRUE(row.names)) 
			dimnames(m)[[1]] <- if (identical(row.names, FALSE)) 
				rep.int("", n)
			else row.names
		
		if (!is.null(metadata(x))) 
			m <- rbind(sapply(varType(x), .subset, 1), 
				ifelse(naOk(x), "na.ok", "no NA"), m)	
		print(m, ..., quote = quote, right = right)
	}
	invisible(x)
}

#' @nord
#' @S3method metadata mframe

metadata.mframe <- function(x, vars) {
    result <- attr(x, "metadata");
	if (!missing(vars)) 
		if (length(vars) == 1L)
			result[[vars]]
		else result[vars]
	else result
}

#' @name metadataReplace.mframe
#' @nord
#' @S3method `metadata<-` mframe
# TODO: Implement 'vars'

`metadata<-.mframe` <- function(x, vars, value) {
	if (missing(vars) && is.null(value)) 
		return(as.data.frame(x));
	if (is.null(meta <- attr(x, "metadata"))) 
		stop(gettextf("metadata of %s is hardcoded: cannot be modified", 
			sqMsg("x")))
	
	if (missing(vars)) meta <- value else meta[vars] <- value;
	mframe(unclass(x), class=class(x), metadata=meta);
} 

#' @nord
#' @S3method varType mframe

varType.mframe <- function(x, vars) 
	varType(metadata(x), vars);

#' @name varTypeReplace.mframe
#' @nord
#' @S3method `varType<-` mframe

`varType<-.mframe` <- function(x, vars, value) {
	varType(metadata(x), vars) <- value;
	x;	
}

#' @nord
#' @S3method varCoerce mframe

varCoerce.mframe <- function(x, vars) 
	varCoerce(metadata(x), vars);


#' @name varCoerceReplace.mframe
#' @nord
#' @S3method `varCoerce<-` mframe

`varCoerce<-.mframe` <- function(x, vars, value) {
	varCoerce(metadata(x), vars) <- value;
	x;	
}

#' @nord
#' @S3method naOk mframe

naOk.mframe <- function(x, vars) 
	naOk(metadata(x), vars);


#' @name naOkReplace.mframe
#' @nord
#' @S3method `naOk<-` mframe

`naOk<-.mframe` <- function(x, vars, value) { 
	naOk(metadata(x), vars) <- value;
	x;
}

#' @nord
#' @S3method varDefault mframe

varDefault.mframe <- function(x, vars, eval=FALSE) 
	varDefault(metadata(x), vars, eval=eval);

#' @name varDefaultReplace.mframe
#' @nord
#' @S3method `varDefault<-` mframe

`varDefault<-.mframe` <- function(x, vars, value) { 
	varDefault(metadata(x), vars) <- value;
	x;
}

#' @nord
#' @S3method varMeta mframe

varMeta.mframe <- function(x, vars, i) 
	varMeta(metadata(x), vars, i);


#' @nord
#' @S3method key mframe

key.mframe <- function(x)
	key(metadata(x));

#' @name keyReplace.mframe
#' @nord
#' @S3method `key<-` mframe

`key<-.mframe` <- function(x, value) {
	key(metadata(x)) <- value;
	x;
}

#' @return A character vector whose length equals the number of rows in 
#'	\code{x}, with the (primary) key for each row.  
#' @nord
#' @S3method keys mframe

keys.mframe <- function(x, vars) {
	if (missing(vars)) 
		vars <- key(metadata(x));
	
	if (length(vars) == 0L)
		character(0)
	else if (length(vars) == 1L)
		as.character(x[[vars]])
	else
		apply(as.matrix(x[vars]), 1, paste, collapse=":");
}
