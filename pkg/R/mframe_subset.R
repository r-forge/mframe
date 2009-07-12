################################################################################
# R PACKAGE:   mframe
# FILE:        R/mframe_subset.R
# DESCRIPTION: Methods to extract and replace parts of 'mframe' objects.  
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  10/06/2009
# LICENSE:     GPL-2
################################################################################
# TODO: - Review & test what happens when indexing by a matrix.
#		- Test behaviour of $ operator

# NOTE: The code for these methods has been copied from the equivalent
#  	methods for data frames from package base v2.8.1, then minor changes
# 	done to ensure compliance to the metadata specifications.

#' Extract or replace subsets of specialized data frames (\code{mframe}s).
#' 
#' These methods behave as their \code{data.frame}  
#' \link[base:Extract.data.frame]{counterparts}, with the following 
#' differences:
#' \itemize{
#' 	\item for extraction, whenever possible, the \code{mframe} class 
#'  	(or subclass) and its associated metadata are preserved, subsetting
#' 		also the metadata as necessary. If some component of the key is 
#' 		dropped, the whole key is removed from the metadata.
#' 		Subsets extracted with \code{[[} and \code{$}, or with \code{[} if 
#' 		some dimension is dropped, always lose the metadata. 	 
#' 	\item for replacement, the metadata constraints are enforced: 
#' 	\enumerate{
#' 		\item data is coerced to the target column class automatically
#' 		\item \code{NA}s in columns with \code{na.ok = FALSE} raise an
#' 			error. Beware that non-intended \code{NA}s may appear in the
#' 			automatic coercion process.   			
#' 		\item new data is filled with the default value when a specific
#' 			value is not given.
#' 		\item duplicated key values in several rows raise an error.
#' 	}
#' }
#' 
#' @title Extract or Replace Parts of a Specialized Data Frame
#' @name Extract.mframe
#' @aliases [.mframe [<-.mframe [[.mframe [[<-.mframe
#' @usage 
#' 	\method{[}{mframe}(x, ..., drop)
#'  \method{[}{mframe}(x, ...) <- value
#' 	\method{[[}{mframe}(x, ...) <- value
#'  
#' @param x an \code{\link{mframe}}.
#' @param {i,j} elements to extract or replace. For \code{[} and \code{[[}, 
#' 	these are \code{numeric} or \code{character} or, for \code{[} only, 
#'  \code{logical} or \code{empty}. Numeric values are coerced to 
#'  \code{integer}. 
#  For replacement by [, a logical matrix is allowed. For replacement by $, 
#  i is a name or literal character string.
#' @param drop logical. If \code{TRUE} the result is coerced to the lowest 
#'  possible dimension. The default is to drop if only one column is left, 
#'  but not to drop if only one row is left. If some dimension is dropped,
#'  class \code{mframe} and the related metadata are always dropped.
#' @param value a suitable replacement value: it will be repeated a whole 
#'  number of times if necessary and it may be coerced to the column
#'  class in the metadata specification. If \code{NULL}, deletes the column 
#'  if a single column is selected. 
#' 
#' @return For \code{[} a mframe, data frame, list or a single column 
#'  (the latter two only when dimensions have been dropped). 
#' @seealso \link[base:Extract.data.frame]{Extract} for data frames.
#' @S3method `[` mframe

`[.mframe` <- function(x, i, j, drop) {
	if (missing(i) && missing(j)) {
		if (!missing(drop)) warning("drop argument will be ignored")
		return(x);
	}
	
	result <- NextMethod();	
	if (is.data.frame(result)) {		
		ca <- customAttributes(x);
		if (!is.null(meta <- ca$metadata)) {
			nArgs <- nargs() - !missing(drop);
			colSelection <- if (missing(j)) {				
				if (nArgs >= 3L) NULL else i
			} else {
				if (nArgs <= 2L) NULL else j
			}    			
			if (!is.null(colSelection)) {
				meta <- meta[colSelection];
				ca$metadata <- meta;
			}
		}
		customAttributes(result) <- ca;		
	}
	
	result;
}

#' @name mframeReplace
#' @nord
#' @S3method `[<-` mframe
# TODO: Handle the matrix index case!

`[<-.mframe` <- function(x, i, j, value) {
    nA <- nargs()
    if (nA == 4) {
        has.i <- !missing(i)
        has.j <- !missing(j)
    }
    else if (nA == 3) {
        if (is.atomic(value)) 
            names(value) <- NULL
        if (missing(i) && missing(j)) {
            i <- j <- NULL
            has.i <- has.j <- FALSE
            if (is.null(value)) 
                return(x[logical(0L)])
        }
        else {
            if (is.logical(i) && is.matrix(i) && all(dim(i) == dim(x))) {
				stop("matrix indexing currently does not work with mframes")				
                nreplace <- sum(i, na.rm = TRUE)
                if (!nreplace) 
                  return(x)
                N <- length(value)
                if (N > 1L && N < nreplace && (nreplace%%N) == 0L) 
                  value <- rep(value, length.out = nreplace)
                if (N > 1L && (length(value) != nreplace)) 
                  stop("rhs is the wrong length for indexing by a logical matrix")
                n <- 0L
                nv <- nrow(x)
                for (v in seq_len(dim(i)[2L])) {
                  thisvar <- i[, v, drop = TRUE]
                  nv <- sum(thisvar, na.rm = TRUE)
                  if (nv) {
                    if (is.matrix(x[[v]])) 
                      x[[v]][thisvar, ] <- if (N > 1L) 
                        value[n + seq_len(nv)]
                      else value
                    else x[[v]][thisvar] <- if (N > 1L) 
                      value[n + seq_len(nv)]
                    else value
                  }
                  n <- n + nv
                }
                return(x)
            }
            if (is.matrix(i)) 
                stop("only logical matrix subscripts are allowed in replacement")
            j <- i
            i <- NULL
            has.i <- FALSE
            has.j <- TRUE
        }
    } else stop("need 0, 1, or 2 subscripts")
    if (has.j && length(j) == 0L) 
        return(x)
	meta <- metadata(x);			## mframe
	attr(x, "metadata") <- NULL;	## mframe
    cl <- oldClass(x)	
    class(x) <- NULL
    new.cols <- NULL
    nvars <- length(x)
    nrows <- .row_names_info(x, 2L)
    if (has.i) {
        rows <- NULL
        if (any(is.na(i))) 
            stop("missing values are not allowed in subscripted assignments of data frames")
        if (char.i <- is.character(i)) {
            rows <- attr(x, "row.names")
            ii <- match(i, rows)
            nextra <- sum(new.rows <- is.na(ii))
            if (nextra > 0L) {
                ii[new.rows] <- seq.int(from = nrows + 1L, length.out = nextra)
                new.rows <- i[new.rows]
            }
            i <- ii
        }
        if (all(i >= 0L) && (nn <- max(i)) > nrows) {
            if (is.null(rows)) 
                rows <- attr(x, "row.names")
            if (!char.i) {
                nrr <- (nrows + 1L):nn
                if (inherits(value, "data.frame") && (dim(value)[1L]) >= 
                  length(nrr)) {
                  new.rows <- attr(value, "row.names")[seq_along(nrr)]
                  repl <- duplicated(new.rows) | match(new.rows, 
                    rows, 0L)
                  if (any(repl)) 
                    new.rows[repl] <- nrr[repl]
                }
                else new.rows <- nrr
            }
            x <- xpdrows.mframe(x, rows, new.rows, 
				defaults=varDefault(meta, eval=TRUE))
            rows <- attr(x, "row.names")
            nrows <- length(rows)
        }
        iseq <- seq_len(nrows)[i]
        if (any(is.na(iseq))) 
            stop("non-existent rows not allowed")
    }
    else iseq <- NULL
    if (has.j) {
        if (any(is.na(j))) 
            stop("missing values are not allowed in subscripted assignments of data frames")
        if (is.character(j)) {
            if ("" %in% j) 
                stop("column name \"\" cannot match any column")
            jj <- match(j, names(x))
            nnew <- sum(is.na(jj))
            if (nnew > 0L) {
                n <- is.na(jj)
                jj[n] <- nvars + seq_len(nnew)
                new.cols <- j[n]
            }
            jseq <- jj
        }
        else if (is.logical(j) || min(j) < 0L) 
            jseq <- seq_along(x)[j]
        else {
            jseq <- j
            if (max(jseq) > nvars) {
                new.cols <- paste("V", seq.int(from = nvars + 
                  1L, to = max(jseq)), sep = "")
                if (length(new.cols) != sum(jseq > nvars)) 
                  stop("new columns would leave holes after existing columns")
                if (is.list(value) && !is.null(vnm <- names(value))) {
                  p <- length(jseq)
                  if (length(vnm) < p) 
                    vnm <- rep(vnm, length.out = p)
                  new.cols <- vnm[jseq > nvars]
                }
            }
        }
    }
    else jseq <- seq_along(x)
    if (any(duplicated(jseq))) 
        stop("duplicate subscripts for columns")
    n <- length(iseq)
    if (n == 0L) 
        n <- nrows
    p <- length(jseq)
    m <- length(value)
    if (!is.list(value)) {
        if (p == 1L) {
            N <- NROW(value)
            if (N > n) 
                stop(gettextf("replacement has %d rows, data has %d", 
                  N, n), domain = NA)
            if (N < n && N > 0L) 
                if (n%%N == 0L && length(dim(value)) <= 1L) 
                  value <- rep(value, length.out = n)
                else stop(gettextf("replacement has %d rows, data has %d", 
                  N, n), domain = NA)
            names(value) <- NULL
            value <- list(value)
        }
        else {
            if (m < n * p && (m == 0L || (n * p)%%m)) 
                stop(gettextf("replacement has %d items, need %d", 
                  m, n * p), domain = NA)
            value <- matrix(value, n, p)
            value <- split(value, col(value))
        }
        dimv <- c(n, p)
    }
    else {
        value <- unclass(value)
        lens <- sapply(value, NROW)
        for (k in seq_along(lens)) {
            N <- lens[k]
            if (n != N && length(dim(value[[k]])) == 2L) 
                stop(gettextf("replacement element %d is a matrix/data frame of %d rows, need %d", 
                  k, N, n), domain = NA)
            if (N > 0L && N < n && n%%N) 
                stop(gettextf("replacement element %d has %d rows, need %d", 
                  k, N, n), domain = NA)
            if (N > 0L && N < n) 
                value[[k]] <- rep(value[[k]], length.out = n)
            if (N > n) {
                warning(gettextf("replacement element %d has %d rows to replace %d rows", 
                  k, N, n), domain = NA)
                value[[k]] <- value[[k]][seq_len(n)]
            }
        }
        dimv <- c(n, length(value))
    }
    nrowv <- dimv[1L]
    if (nrowv < n && nrowv > 0L) {
        if (n%%nrowv == 0L) 
            value <- value[rep(seq_len(nrowv), length.out = n), 
                , drop = FALSE]
        else stop(gettextf("%d rows in value to replace %d rows", 
            nrowv, n), domain = NA)
    }
    else if (nrowv > n) 
        warning(gettextf("replacement data has %d rows to replace %d rows", 
            nrowv, n), domain = NA)
    ncolv <- dimv[2L]
    jvseq <- seq_len(p)
	
    if (ncolv < p) 
        jvseq <- rep(seq_len(ncolv), length.out = p)
    else if (ncolv > p) {
        warning(gettextf("provided %d variables to replace %d variables", 
            ncolv, p), domain = NA)
        new.cols <- new.cols[seq_len(p)]
    }
    if (length(new.cols)) {		
        nm <- names(x)
        rows <- .row_names_info(x, 0L)
        a <- attributes(x)
        a["names"] <- NULL
        x <- c(x, vector("list", length(new.cols)))
        attributes(x) <- a
        names(x) <- c(nm, new.cols)
        attr(x, "row.names") <- rows
		meta <- c(meta, ## mframe
			setNames(dfMetadata(as.list(rep(NA, length(new.cols)))), new.cols))
    }	   
	
	if (has.i)	
        for (jjj in seq_len(p)) {
            jj <- jseq[jjj]
            vjj <- value[[jvseq[[jjj]]]]
			if  (!is.null(meta) && jj <= length(meta)) {	## mframe
				mj <- meta[[jj]]
				vjj <- enforce(mj, vjj)
				if (!naOk(mj) && any(is.na(vjj)))
					stop(gettextf("NA values not accepted for variable %s",
						dqMsg(names(meta)[jj])))
    		}
#browser()					
            if (jj <= nvars) {
                if (length(dim(x[[jj]])) != 2L) 
                  x[[jj]][iseq] <- vjj
                else x[[jj]][iseq, ] <- vjj
            }
            else {
                x[[jj]] <- vjj[FALSE]
                if (length(dim(vjj)) == 2L) {
                  length(x[[j]]) <- nrows * ncol(vjj)
                  dim(x[[j]]) <- c(nrows, ncol(vjj))
                  x[[jj]][iseq, ] <- vjj
                }
                else {
                  length(x[[j]]) <- nrows
                  x[[jj]][iseq] <- vjj
                }
            }
        }
    else if (p > 0L) {		
        for (jjj in p:1L) {
            jj <- jseq[jjj]			
            v <- value[[jvseq[[jjj]]]]	
			if (!is.null(meta) && !is.null(v)) {	## mframe
				if (jj <= nvars) {
	    			mj <- meta[[jj]]
					v <- enforce(mj, v)
					if (!naOk(mj) && any(is.na(v)))
						stop(gettextf("NA values not accepted for variable %s",
							dqMsg(names(meta)[jj])))					
				} else 
					varType(meta[[jj]]) <- class(v)[1L]
			}

#			if (!is.null(meta) && !is.null(v) && jj <= length(meta)) {	## mframe
#    			mj <- meta[[jj]]
#				v <- enforce(mj, v)
#				if (!naOk(mj) && any(is.na(v)))
#					stop(gettextf("NA values not accepted for variable %s",
#						dqMsg(names(meta)[jj])))
#			}
            x[[jj]] <- v
			if (is.null(v))	## mframe
				meta[[jj]] <- NULL
            if (!is.null(v) && is.atomic(x[[jj]])) 
                names(x[[jj]]) <- NULL
        }
	}
    if (length(new.cols) > 0L) {
		if (!allowNewColumns(meta)) ## mframe 
			stop(sprintf(ngettext(length(new.cols),
				"new column %s is not allowed",		
				"new columns %s are not allowed"), dqMsg(new.cols)))	
        new.cols <- names(x)
        if (any(duplicated(new.cols))) 
            names(x) <- make.unique(new.cols)
    }
    class(x) <- cl
	attr(x, "metadata") <- meta;	## mframe
    x	
}

#' @name mframeReplace2
#' @nord
#' @S3method `[[<-` mframe

`[[<-.mframe` <- function(x, i, j, value) {
	meta <- metadata(x);
    cl <- oldClass(x)
    class(x) <- NULL
    nrows <- .row_names_info(x, 2L)
    if (is.atomic(value)) 
        names(value) <- NULL
	
    if (nargs() < 4L) {
        nc <- length(x)
        if (!is.null(value)) {
            N <- NROW(value)
            if (N > nrows) 
                stop(gettextf("replacement has %d rows, data has %d", 
                  N, nrows), domain = NA)
            if (N < nrows && N > 0L) 
                if (nrows%%N == 0L && length(dim(value)) <= 1L) 
                  value <- rep(value, length.out = nrows)
                else stop(gettextf("replacement has %d rows, data has %d", 
                  N, nrows), domain = NA)
  
			if  (!is.null(meta)) {	## mframe
				# FIXME: Replace this logic by a call to valIndex, but first
				#	ensure that valIndex with handler=NULL return NAs or zero-length
				#	when the index does not validate
				im <- valIndex(i, x, retclass="integer", mark.new=TRUE, 
					handler=NULL)
				isInMeta <- !attr(im, "is.new")
#				if (is.numeric(i)) {
#	    			im <- as.integer(i)
#					im <= length(meta)
#				} else if (is.character(i)) {
#					im <- match(i, names(meta), nomatch=0L)
#					im[1L] > 0L
#				} else if (isTRUE(i)) {
#					im <- 1L
#					length(meta) > 0L
#				} else FALSE
				if (isInMeta) {
					mi <- meta[[im]]
					value <- enforce(mi, value)
					if (!naOk(mi) && any(is.na(value)))
						stop(gettextf(
							"NA values not accepted for variable %s",
							dqMsg(names(meta)[[im]])))
				} else if (!allowNewColumns(meta))
					stop("new columns are not allowed");
			}  
        }		
        x[[i]] <- value		
        if (length(x) > nc) {	# New column added
			if (!is.null(meta) && !isInMeta && !allowNewColumns(meta))
				stop(gettextf("new column %s is not allowed"))	## mframe				
            nc <- length(x)
            if (names(x)[nc] == "") 
                names(x)[nc] <- paste("V", nc, sep = "")
            names(x) <- make.unique(names(x))
        } 	
        class(x) <- cl
		if (length(x) < nc)  ## mframe, column removed
			metadata(x)[[i]] <- NULL		
        return(x)
    }
    if (missing(i) || missing(j)) 
        stop("only valid calls are x[[j]] <- value or x[[i,j]] <- value")
    rows <- attr(x, "row.names")
    nvars <- length(x)
    if (n <- is.character(i)) {
        ii <- match(i, rows)
        n <- sum(new.rows <- is.na(ii))
        if (n > 0L) {
            ii[new.rows] <- seq.int(from = nrows + 1L, length.out = n)
            new.rows <- i[new.rows]
        }
        i <- ii
    }
    if (all(i >= 0L) && (nn <- max(i)) > nrows) {
        if (n == 0L) {
            nrr <- (nrows + 1L):nn
            if (inherits(value, "data.frame") && (dim(value)[1L]) >= 
                	length(nrr)) {
                new.rows <- attr(value, "row.names")[seq_len(nrr)]
                repl <- duplicated(new.rows) | match(new.rows, 
                  rows, 0L)
                if (any(repl)) 
                  new.rows[repl] <- nrr[repl]
            }
            else new.rows <- nrr
        }		
        x <- xpdrows.mframe(x, rows, new.rows, 
			defaults=varDefault(meta, eval=TRUE))
        rows <- attr(x, "row.names")
        nrows <- length(rows)
    }
    iseq <- seq_len(nrows)[i]
    if (any(is.na(iseq))) 
        stop("non-existent rows not allowed")
    if (is.character(j)) {
        if ("" %in% j) 
            stop("column name \"\" cannot match any column")
        jseq <- match(j, names(x))
        if (any(is.na(jseq))) 
            stop("replacing element in non-existent column: ", 
                j[is.na(jseq)])
    }
    else if (is.logical(j) || min(j) < 0L) 
        jseq <- seq_along(x)[j]
    else {
        jseq <- j
        if (max(jseq) > nvars) 
            stop("replacing element in non-existent column: ", 
                jseq[jseq > nvars])
    }
    if (length(iseq) > 1L || length(jseq) > 1L) 
        stop("only a single element should be replaced")	
	if  (!is.null(meta) && jseq <= length(meta)) {	## mframe
		mj <- meta[[jseq]]
		value <- enforce(mj, value)
		if (!naOk(mj) && any(is.na(value)))
			stop(gettextf("NA values not accepted for variable %s",
				dqMsg(names(meta)[[jseq]])))
	}	
    x[[jseq]][[iseq]] <- value
    class(x) <- cl
    x
}

#' @nord
#' @seealso mframeHelpers
#  NOT EXPORTED

xpdrows.mframe <- function(x, old.rows, new.rows, defaults) {
    nc <- length(x)
    nro <- length(old.rows)
    nrn <- length(new.rows)
    nr <- nro + nrn
    for (i in seq_len(nc)) {
        y <- x[[i]]
        dy <- dim(y)
        cy <- oldClass(y)
        class(y) <- NULL
        if (length(dy) == 2L) {
browser()						
            dny <- dimnames(y)
            if (length(dny[[1L]]) > 0L) 
                dny[[1L]] <- c(dny[[1L]], new.rows)
            z <- array(y[1L], dim = c(nr, nc), dimnames = dny)
            z[seq_len(nro), ] <- y
            class(z) <- cy
            x[[i]] <- z
        } else {
            ay <- attributes(y)
            if (length(names(y)) > 0L) 
                ay$names <- c(ay$names, new.rows)
            length(y) <- nr
			if (!is.null(defaults[[i]]))		## mframe
				y[(nro+1):nr] <- defaults[[i]]	
            attributes(y) <- ay
            class(y) <- cy
            x[[i]] <- y
        }
    }
    attr(x, "row.names") <- c(old.rows, new.rows)
    x
}
