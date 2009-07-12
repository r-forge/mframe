################################################################################
# R PACKAGE:   mframe
# FILE:        temp/tmp.R
# DESCRIPTION: Scratchpad file for writting & testing code. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  28/05/2009
################################################################################
stop("not to be directly sourced!"); 

library(DevTools);
useDevLib();
devPath(file.path("C:", "ebe", "Work", "Projects", "PaRiS_R", "mframe"));

library(piUtils)

s("generics.R")
s("metadata.R")
s("varMetadata.R")
s("dfMetadata.R")
s("mframe.R")
s("mframe_subset.R")

# For testing namespace, etc. without loading DevTools
.libPaths(c("C:\\PROGRA~1\\R\\R-28~1.1\\dev-library", .libPaths()))

# Runs all unit tests in 'inst/unitTests'
ut();

# Test code that will be in PaRiS:

# data.frame metadata for class \code{relPdts} (move to PaRiS!)
metadata.relPdts <- function(x) {
	dfMetadata(Pdt = "character",
		Type = list(
			Class = "factor",
			levels = c("benchmark", "tbenchmark", "extend_series"),
    		Default = "benchmark"
		),
		RelPdt = "character",
		RelName = "character",
		Start = "Date",
		End = "Date",
		
		required = c("Pdt", "Type", "RelPdt"),
		key = c("Pdt", "Type", "RelPdt", "Start", "End"),		
		allow.new.columns = FALSE		
	);
}

relPdts <- function(..., row.names=NULL, check.rows=FALSE, check.names=TRUE,
		stringsAsFactors=default.stringsAsFactors()) 
	mframe(..., row.names=row.names, check.rows=check.rows, 
		check.names=check.names, stringsAsFactors=stringsAsFactors, 
		class="relPdts");






# Old params code (to be moved to some other package...)

#' Functions to get and set the parameters of an object.  
#' 
#' \code{params} is an \link[base:UseMethod]{S3 generic} accessor 
#' function, and \code{params<-} is an S3 generic replacement function. 
#' The default \code{\link[base:UseMethod]{methods}} get and set the 
#' "params" attribute of the object. 
#'  
#' "Parameters" is a broad concept to represent lists of values that affect
#' the behaviour of an object, and are normally attached to models, 
#' specifications, or metadata objects.   
#'    
#' @title Parameters of an Object
#' @aliases params `params<-` 
#' @usage params(x, ...)
#'  params(x, ...) <- value
#' 
#' @param x an \R object.
#' @param col subset of elements for which to return the parameters.
#' @param all logical flag; for \code{mframe}s, \code{TRUE} means to return
#'  an element for each column of \code{x}, even if the column is not in the
#'  metadata, while \code{FALSE} would return only the metadata information.
#' @param value a list of parameters in the form \code{tag = value}.
#' @param ... placeholder in the generic function to accomodate new arguments
#'  in methods.
#' 
#' @return For \code{params}, a named list of parameters. 
#'  For \code{mframe}s, when \code{all = TRUE}, returns an empty list for 
#'  columns that are not in the metadata specification.  
#' 
#'  \code{params<-} allows to modify all or some of the parameters. 
#' 
#' @seealso \code{\link{metadata}}.
# @export
# NOTE: 'param' is a common generic function in several R packages to extract
# parameters from models: \code{\link[nlreg]{param.nlreg} in \pkg{nlreg},
# or \code{\link[distr]{param-methods} in \pkg{distr}.
# Although we don't refer to a model here, the meaning "extract parameters
# from an object that has parameters" is similar enough to share the same
# generic, I think...

#params <- function(x, ...) UseMethod("params");

# @name paramsReplace
# @nord
# @export `params<-`

#`params<-` <- function(x, ..., value) UseMethod("params<-");


`[.testObj` <- function(x, i, j, ..., drop) {
	#newFun(x, i, j, ..., drop=drop);
	indexArgs(x, i, j, ..., drop=drop);
#	cat("Narg =", Narg, "; missing(i) =", missing(i), 
#		"; missing(j) =", missing(j), "\n")	
}
newFun <- function(x, i, j, ..., drop) {
    mdrop <- missing(drop);
    Narg <- nargs() - (!mdrop);
	list(nargs(), missing(i), missing(j), missing(drop));	
}

dfIndexes <- function(x, ...) {
	callArgs <- as.list(sys.call(-1))[-c(1,2)];
	if ((posDrop <- match("drop", names(callArgs), nomatch=0L)) > 0)
		callArgs <- callArgs[-posDrop];
	
	nArgs <- length(callArgs)
	if (nArgs > length(dim(x)))
		stop("incorrect number of dimensions");
	
	if (nArgs == 0L) 
		list(rolw=NULL, cols=NULL)
	else {
		isMissing <- sapply(callArgs, function(x) 
				is.name(x) && identical(as.character(x), ""));
		if (nArgs == 1L) {
			if (isMissing[1L])
				list(rows=NULL, cols=NULL)
			else list(rows=NULL, cols=i)
		} else if (nArgs == 2L){			
			if (isMissing[1L])
				list(rows=NULL, cols=j)
			else
				list(rows=i, cols=j)				
		} else {
browser()			
		}
	}
}	
	
#	stop();
#print(callArgs);	
#	Nargs <- eval(quote(nargs()), parent.frame(1));
#	mi <- missing(i);
#	mj <- missing(j);
#	md <- missing(drop);
#	list(Nargs, mi, mj, md)
#}

extractDF <- function() {
	x <- data.frame(Code=character(0), nargs=integer(0), `miss_i`=logical(0), 
		`miss_j`=logical(0), `miss_drop`=logical(0), stringsAsFactors=FALSE);
	y <- structure(c(1,2),  class="testObj");
	
	x[nrow(x)+1, ] <- c(list("x[]"), y[]);
	x[nrow(x)+1, ] <- c(list("x[d]"), y[drop=TRUE]);
    x[nrow(x)+1, ] <- c(list("x[1]"), y[1]);
	x[nrow(x)+1, ] <- c(list("x[1,]"), y[1,]);
	x[nrow(x)+1, ] <- c(list("x[,1]"), y[,1]);
	x[nrow(x)+1, ] <- c(list("x[1,1]"), y[1,1]);
 	x[nrow(x)+1, ] <- c(list("x[1,d]"), y[1,drop=TRUE]);    
 	x[nrow(x)+1, ] <- c(list("x[1,,d]"), y[1,,drop=TRUE]);
	x[nrow(x)+1, ] <- c(list("x[,1,d]"), y[,1,drop=TRUE]);
	x[nrow(x)+1, ] <- c(list("x[1,1,d]"), y[1,1,drop=TRUE]);
	
	x;
}
z2 <- extractDF();	
	


test_indexArgs <- svUnit(function(){
	`[.testClass` <- function(x, ..., drop) {
		dfIndexes(x, ...);
	}
	d <- data.frame(X=1:2, Y=3:4, Z=5:6);
	dc <- structure(d, class="testClass");
	
	checkEquals(list(rows=NULL, cols=NULL), dc[]);
	checkEquals(list(rows=NULL, cols=NULL), dc[drop=TRUE]);
	checkEquals(list(rows=NULL, cols=NULL), dc[]);
	
	
})