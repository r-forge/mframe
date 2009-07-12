################################################################################
# R PACKAGE:   mframe
# FILE:        R/runit_mframe.R
# DESCRIPTION: Test suite for the 'mframe' S3 class. To be run with svUnit. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  25/05/2009
# LICENSE:     GPL-2
################################################################################

# Plain DF Metadata
mdp <- dfMetadata(C="character", L="logical", I="integer", N="numeric", 
	CX="complex", D="Date", Pct="POSIXct", F="factor", 
	R=list("raw", default=0));
ncolMdp <- 9;

test_mframe_coerceColsOnConstruction <- svTest(function() {		
	x <- mframe(1, 1, 1, 1, 1, "2008-01-01", "2008-01-01 10:00:00 CET", 1, 1, 
		metadata=mdp)
	
	checkEquals(c(1, ncolMdp), dim(x), "verify dimensions");
	checkEquals(list("1", c("C", "L", "I", "N", "CX", "D", "Pct", "F", "R")), 
		dimnames(x), "verify dimnames");
	checkTrue(is.data.frame(x), "an mframe is also a data.frame");
	checkTrue(is.mframe(x), "an mframe is an mframe");
	checkTrue(is.mframe(x, strict=TRUE), "an mframe is strictly an mframe");
	
	# Test coercion mechanisms
	checkEquals("1", x[1, "C"], "coerce 1 to character");
	checkEquals(as.Date("2008-01-01"), x[1, "D"], "coerce character to Date");
	checkEquals(TRUE, x[1, 2], "coerce 1 to logical");
	checkEquals(1L, x[1, "I"], "coerce 1 to integer");
	checkEquals(1L, x[1, "N"], "coerce 1 to numeric");
	checkEquals(1+0i, x[1, "CX"], "coerce 1 to complex");
	checkEquals(as.POSIXct("2008-01-01 10:00:00 CET"), x[1, "Pct"], 
		"coerce character to POSIXct");
	checkEquals(factor(1), x[1, "F"], "coerce 1 to factor");
	checkEquals(as.raw(1), x[1, "R"], "coerce 1 to raw");
})

test_mframe_metadataDefaults <- svTest(function() { 
	x <- mframe(metadata=mdp, nrow=2);
	
	checkEquals(mdp, metadata(x), "retrieve an mframe metadata");
	
	checkEquals(c(2, ncolMdp), dim(x), "verify dimensions");
	checkTrue(all(is.na(x[,-match("R", names(x))])), 
		"non-specified values without defaults produce NAs");
		
	md <- dfMetadata(C=list("character", default="C"),
    	D=list("Date", default="2008-01-01"), 
		L=list("logical", default=TRUE),
		I=list(Class="integer", default=100),  
		N=list("numeric", default=-5),
		CX=list("complex", default=10+3i),
		Pct=list("POSIXct", default="2008-01-01 10:00:00 CET"),
		F=list("factor", default="abc"),
		R=list("raw", default=7));
	x <- mframe(metadata=md, nrow=3);

	checkEquals("C", x[3, "C"], "default character value");
	checkEquals(rep(as.Date("2008-01-01"), 3), x$D, "default Date value");
	checkEquals(rep(TRUE, 3), x[["L"]], "default logical value");
	checkEquals(100L, x[2, "I"], "default integer value");
	checkEquals(-5.0, x[1, "N"], "default numeric value");
	checkEquals(10+3i, x[1, "CX"], "default complex value");
	checkEquals(as.POSIXct("2008-01-01 10:00:00 CET"), x[1, "Pct"], 
		"default POSIXct value");
	checkEquals(factor("abc"), x[1, "F"], "default factor value");	
	checkEquals(as.raw(7), x[2, "R"], "default raw value");
	
	# Default using expressions
	md <- dfMetadata(D =
			varMetadata("Date", default=quote(as.Date("2008-01-01"))));
	x <- mframe(nrow=1, metadata=md);
	checkEquals(as.Date("2008-01-01"), x[1, "D"]);
})	

test_mframe_nrows <- svTest(function() {
	x <- mframe();
	checkEquals(c(0, 0), dim(x), "empty mframe with no metadata");
	
	checkEquals(c(0, ncolMdp), dim(mframe(metadata=mdp)), 
		"zero-rows mframe by missing columns and nrow");

	checkEquals(c(0, ncolMdp), dim(mframe(metadata=mdp, nrow=0)), 
		"zero-rows mframe setting nrow=0");
	
	checkEquals(c(0, ncolMdp), dim(mframe(I=integer(0), metadata=mdp)), 
		"zero-rows mframe setting a zero-length column");
	
	x <- suppressWarnings(mframe(C="abc", metadata=mdp, nrow=0));
	checkEquals(c(1, ncolMdp), dim(x), 
		"nrow=0 ignored if there's some non-zero length column");

	x <- suppressWarnings(mframe(C=c("abc", "cde"), metadata=mdp, nrow=1));
	checkEquals(c(2, ncolMdp), dim(x), 
		"nrow ignored if some column has length > nrow");
	checkEquals(c("abc", "cde"), x[,1], "");
	checkEquals(as.logical(c(NA, NA)), x[,"L"], "recycle logical");
	
	x <- mframe(C=c("abc", "cde"), metadata=mdp, nrow=3);
	checkEquals(c(3, ncolMdp), dim(x), 
		"nrow produces column recycling when nrow > length(cols)");	
})

test_mframe_naOk <- svTest(function() { 
	mdr <- mdp;
	naOk(mdr) <- list(L=FALSE, D=FALSE, TRUE);
	
	checkEquals(c(0, ncolMdp), dim(mframe(metadata=mdr)), 
		"no error when there are required columns with zero-rows mframes");
	
	checkEquals(TRUE, mframe(L=TRUE, D="2009-01-15", metadata=mdr)$L,
		"no error if required columns are provided non-NA values")

	checkException(mframe(I=1, metadata=mdr),
		"throws exception if non-specified required values have NA defauls");
	checkException(mframe(L=NA, D="2009-01-15", metadata=mdr),
		"throws exception if a required values is assigned an NA value");
	
	varDefault(mdr) <- list(L=FALSE, D="2009-01-15");
	checkEquals(as.Date("2009-01-15"), mframe(I=1, metadata=mdr)$D,
		"no error if non-specified columns have non-NA defaults");
})

test_mframe_allowNewColumns <- svTest(function() { 
	checkTrue("New" %in% names(mframe(New=1, metadata=mdp)), 
		"new columns allowed on creation if allow.new.columns = TRUE");

	checkEquals(c("I", "X", "J"), 
		names(mframe(X=3, 1:2, J="a", metadata=mdp["I"])),
		"unnames columns are assigned names from the unused metadata names");
	
	mda <- mdp;
	allowNewColumns(mda) <- FALSE;
	checkTrue(!"New" %in% suppressWarnings(names(mframe(New=1, metadata=mda))), 
		"new columns removed on creation when allow.new.columns = FALSE");
	 	
})

test_mframe_keys <- svTest(function() {
	x <- mframe(I=1:3, metadata=mdp)
	checkEquals(NULL, key(x), "undefined key is NULL");
	checkEquals(character(0), keys(x), 
		"keys when key is undefined are an empty character vector");
	
	mdk <- mdp;
	key(mdk) <- "I";
	
	x <- mframe(I=1:3, metadata=mdk)
	checkEquals("I", key(x), "single-column key");
    checkEquals(c("1", "2", "3"), keys(x), "values of single-column key");
	
	checkException(mframe(I=c(1, 1, 2), metadata=mdk),
		"duplicated primary key should raise an error")
})

test_mframe_coercion <- svTest(function() { 
	x <- mframe(I=1:2, D="2008-01-01", L=TRUE, metadata=mdp);
	
	checkEquals("data.frame", class(as.data.frame(x)),
		"as.data.frame.mframe must return a plain data frame");
	
	checkEquals(x, as.mframe(as.data.frame(x), metadata=mdp),
		"coerce an mframe to a data.frame, then recoerce to mframe");
	
	checkEquals(x, 
		as.mframe(data.frame(I=1:2, D="2008-01-01", L=TRUE), metadata=mdp),
		"input as a list works with as.mframe")		
});

test_mframe_metadataReplace <- svTest(function() {
	x <- mframe(I=1:2, metadata=mdp["I"]);	
	checkEquals(c(2, 1), dim(x), "just to compare with the next test");
	
	metadata(x) <- mdp[c("I", "L")]
	checkEquals(c(2, 2), dim(x), "replacing metadata changes dimension of x");
	checkEquals(c("I", "L"), names(x), "replacing metadata adds new column");
	checkEquals(c(1L, 2L), x$I, "replacing metadata respects old column");
	checkEquals(c(NA, NA), x$L, "replacing metadata adds new column values");
	
	metadata(x) <- NULL;
	checkEquals(NULL, metadata(x), 
		"setting metadata to NULL removes metadata attribute");	
	checkEquals("data.frame", class(x), 
		"setting metadata to NULL coerces mframe to data.frame");
	
	# Metadata for hardcoded mframe subclasses cannot be modified!
	metadata.childSframe <- function(x) return(mdp["I"]);
	x <- mframe(I=1:2, class="childSframe");
	checkException(metadata(x) <- mdp[c("I", "L")], 
		"hardcoded metadata via subclassing cannot be modified");	
	
	x <- mframe(I=c(NA, 2), metadata=mdp["I"]);
	checkException(required(metadata(x)) <- "I",
		"changing the required field of a column that has NA raises error");
	
	x[1, 1] <- 5;
	naOk(metadata(x)) <- FALSE;
	checkEquals(c(5L, 2L), x[,1],
		"change the required field of a column without NAs");	
})

test_mframe_extract <- svTest(function() {
	x <- mframe(1:2, c(TRUE, FALSE), metadata=mdp[c("I", "L")]);
	
	checkEquals(x, x[], "x equals x[]");
	checkEquals(x, x[,], "x equals x[,]");
	checkEquals(x, suppressWarnings(x[,,drop=TRUE]), "x equals x[,]");
	# Using a single index but named "j" returns the frame without changes
	# for data.frame (don't understand why!!), so the behaviour is replicated
	# for mframes
	checkEquals(x, x[j=2], "x equals x[j=2] (!!!)");

	checkEquals(mframe(1, metadata=mdp["I"]), x[1, 1, drop=FALSE],
		"row/col subsetting without dropping keeps mframe");

	checkEquals(c(TRUE, FALSE), x[, "L"],
		"row/col subsetting with drop dimension removes mframe and metadata");

	checkEquals(mframe(c(TRUE, FALSE), metadata=mdp["L"]), x["L"], 
		"single index keeps mframe");

	# Resort variables should also resort dfMetadata
	xt <- mframe(c(TRUE, FALSE), 1:2, metadata=mdp[c("L", "I")]);
	checkEquals(xt, x[c(2, 1)], "resort variables with x[i] syntax");
	checkEquals(xt, x[,c(2, 1)], "resort variables with x[,j] syntax");
	checkEquals(xt, x[c(2, 1), drop=TRUE], 
		"resort variables with x[i,drop] syntax");
	checkEquals(xt, x[,c(2, 1),drop=TRUE], 
		"resort variables with x[,j,drop] syntax");
	
	# Empty extract
	y <- x[0];
	checkEquals(c(2, 0), dim(y));
	checkEquals(0, length(metadata(y)));
	
	y <- x[0,0];
	checkEquals(c(0, 0), dim(y));
	checkEquals(0, length(metadata(y)));
	
#	y <- x[0,];
#	checkEquals(c(0, 2), dim(y));
#	checkEquals(2, length(metadata(y)));	
})

test_mframe_replace <- svTest(function() {
	x <- mframe(1:2, c(TRUE, FALSE), metadata=mdp[c("I", "L")]);
	
	y <- x; y[,2] <- c(FALSE, TRUE);
	checkEquals(mframe(1:2, c(FALSE, TRUE), metadata=mdp[c("I", "L")]),
		y, "replace using [,j]");
	
	y[,"L"] <- c(15, NA);
	checkEquals(mframe(1:2, c(TRUE, NA), metadata=mdp[c("I", "L")]),
		y, "replace using [,j] with implicit coercion");
	
	y[1, 1] <- "15";
	checkEquals(mframe(c(15, 2), c(TRUE, NA), metadata=mdp[c("I", "L")]),
		y, "replace using [i,j] with implicit coercion");
	
	y[2,] <- 0;
	checkEquals(mframe(c(15, 0), c(TRUE, FALSE), metadata=mdp[c("I", "L")]),
		y, "replace using [i,] with implicit coercion");

	y[2,] <- list(-12.3, TRUE);
	checkEquals(mframe(c(15, -12), c(TRUE, TRUE), metadata=mdp[c("I", "L")]),
		y, "replace using [i,] and value is a list");
	
	y[1] <- 25.5
	checkEquals(mframe(c(25L, 25L), c(TRUE, TRUE), metadata=mdp[c("I", "L")]),
		y, "replace using [i] with implicit coercion");		
})

test_mframe_replaceNaOk <- svTest(function() { 
	x <- mframe(1:2, c(TRUE, FALSE), metadata=mdp[c("I", "L")]);
	naOk(x) <- FALSE;		

	checkException(x[1, 1] <- NA, 
		"replacing to NA using [i,j] when required raises error");
	checkException(x[, "I"] <- NA,
		"replacing to NA using [,j] when required raises error");
	checkException(x[2, ] <- NA,
		"replacing to NA using [i,] when required raises error");
	
	x[1,1] <- 2;	# Ensure a non-NA value can be set	
})
	
test_mframe_replaceAddCol <- svTest(function() { 
	x <- mframe(c("a", "b"), c(-2, 3), metadata=mdp[c("C", "N")]);
	
	y <- x; y[, "L"] <- TRUE;
	checkEquals(mframe(c("a", "b"), c(-2, 3), TRUE, 
		metadata=mdp[c("C", "N", "L")]), y,
		"add variable on replace also adds it to the metadata");

	y <- x; y["L"] <- TRUE;
	checkEquals(mframe(c("a", "b"), c(-2, 3), TRUE, 
		metadata=mdp[c("C", "N", "L")]), y,
		"add variable on replace also adds it to the metadata");
	
	y <- x;
	allowNewColumns(metadata(y)) <- FALSE;
	checkEquals(c("C", "N"), names(y),
		"extra columns are removed if allow.new.columns is changed to FALSE");
    
	checkException(y[, "J"] <- TRUE,
		"add column on replace when new columns not allowed raises error");
})

test_mframe_replaceRemoveCol <- svTest(function() {
	x <- mframe(c("a", "b"), c(-2, 3), metadata=mdp[c("C", "N")]);	
	
	y <- x; 
	key(y) <- c("C", "N");
	checkEquals(c("C", "N"), key(y));
	y["C"] <- NULL;
	checkEquals(mframe(c(-2, 3), metadata=mdp["N"]), y);
	checkEquals(mdp["N"], metadata(y));
	checkEquals(NULL, key(y));
})

test_mframe_replaceAddRow <- svTest(function() {
	x <- mframe(c("a", "b"), c(-2, 3), metadata=mdp[c("C", "N")]);
	
	y <- x; y[4, "N"] <- 5;
	checkEquals(mframe(c("a", "b", NA, NA), c(-2, 3, NA, 5), 
		metadata=mdp[c("C", "N")]), y,
		"[ adds new rows without explicit defaults defined"); 
	
	varDefault(metadata(y)) <- list(C="def");
	y[6, "N"] <- 3.1;
	checkEquals(as.data.frame(mframe(c("a", "b", NA, NA, "def", "def"), 
		c(-2, 3, NA, 5, NA, 3.1), metadata=mdp[c("C", "N")])), 
		as.data.frame(y), "[ adds new rows with explicit default"); 
	
	y <- x; y[[4, "N"]] <- 5;
	checkEquals(mframe(c("a", "b", NA, NA), c(-2, 3, NA, 5), 
		metadata=mdp[c("C", "N")]), y,
		"[[ adds new rows without explicit defaults defined");
    
	varDefault(metadata(y)) <- list(C="def");
	y[[6, "N"]] <- 3.1;
	checkEquals(as.data.frame(mframe(c("a", "b", NA, NA, "def", "def"), 
		c(-2, 3, NA, 5, NA, 3.1), metadata=mdp[c("C", "N")])), 
		as.data.frame(y), "[ adds new rows with explicit default"); 
	
})

test_mframe_replaceRemoveRow <- svTest(function() { DEACTIVATED();
		
})

test_mframe_replace2 <- svTest(function() { 
	x <- mframe(1:2, c(TRUE, FALSE), metadata=mdp[c("I", "L")]);
	
	x[[2]] <- c(0, -5);
	checkEquals(mframe(1:2, c(FALSE, TRUE), metadata=mdp[c("I", "L")]),
		x, "replace using [[int]] automatically coerces");
	
	x[["I"]] <- 5.75;
	checkEquals(mframe(c(5, 5), c(FALSE, TRUE), metadata=mdp[c("I", "L")]),
		x, "replace using [[char]] automatically coerces and recycles");
	
	x[[2, 1]] <- NA;
	checkEquals(mframe(c(5, NA), c(FALSE, TRUE), metadata=mdp[c("I", "L")]),
		x, "replace using [[i,j]]");
	checkEquals(NA_integer_, x[[2, 1]], "replace using [[i,j]] coerce");	
})

test_mframe_replace2NaOk <- svTest(function() { 
	x <- mframe(1:2, c(TRUE, FALSE), metadata=mdp[c("I", "L")]);
	naOk(x) <- FALSE;		

	checkException(x[[1, 1]] <- NA, 
		"replacing to NA using [i,j] when required raises error");
	checkException(x[["I"]] <- NA,
		"replacing to NA using [,j] when required raises error");
	
	x[[1,1]] <- 5.2;	# Ensure a non-NA value can be set	
})

test_mframe_replace2AddCol <- svTest(function() {
	x <- mframe(c("a", "b"), c(-2, 3), metadata=mdp[c("C", "N")]);
	
	y <- x; y[["J"]] <- TRUE;
	checkEquals(c("C", "N", "J"), names(y),
		"add column on [[ replace when new columns allowed");

	y <- x;   
    allowNewColumns(metadata(y)) <- FALSE;
	checkException(y[["W"]] <- TRUE,
		"add column on [[ replace when new columns not allowed raises error");
})

test_mframe_replace2RemoveCol <- svTest(function() {
	x <- mframe(c("a", "b"), c(-2, 3), metadata=mdp[c("C", "N")]);
	
	y <- x;
    key(y) <- c("C", "N");
	checkEquals(c("C", "N"), key(y));
	y[["C"]] <- NULL;
	checkEquals(mframe(c(-2, 3), metadata=mdp["N"]), y);
	checkEquals(mdp["N"], metadata(y));
	checkEquals(NULL, key(y));
})

