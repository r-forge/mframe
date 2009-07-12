################################################################################
# R PACKAGE:   mframe
# FILE:        inst/unitTests/runit_dfMetadata.R
# DESCRIPTION: Test suite for the dfMetadata S3 class. To be run with svUnit.
# AUTHORS: 	   Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  2009 May 11
# LICENSE:     GPL-2
################################################################################

## Create a few objects we need for tests


# Executed before each test function
.setUp <- function () {
    ## Specific actions for svUnit: prepare context
    if ("package:svUnit" %in% search()) {
        .Log <- Log() ## Make sure .Log is created
        .Log$..Unit <- "C:/DOCUME~1/m824704/LOCALS~1/Temp/RtmpnJ8PsS/runitlt1.R"
        .Log$..File <- ""
        .Log$..Obj <- ""
        .Log$..Tag <- ""
        .Log$..Msg <- ""
        rm(..Test, envir = .Log)
    }
	
	#assign("firstLetters", LETTERS[4:1], envir=.GlobalEnv);	
}

# Executed after each test function
.tearDown <- function () {
    ## Specific actions for svUnit: clean up context
    if ("package:svUnit" %in% search()) {
        .Log$..Unit <- ""
        .Log$..File <- ""
        .Log$..Obj <- ""
        .Log$..Tag <- ""
        .Log$..Msg <- ""
        rm(..Test, envir = .Log)
    }
	
	#rm("firstLetters", envir=.GlobalEnv);
}

# Tests that 'current' contains ALL elements of 'target' with the same values,
# but disregards the order of elements as well as any additional elements in
# 'current' that are not in 'target'. Matching of elements is done by their
# names.
allCommonEqual <- function(target, current, check.attributes=TRUE, ...) {
	current <- current[names(target)];
	all.equal.list(target, current, check.attributes=check.attributes, ...);
} 

test_dfMetadata_empty <- svTest(function() {
	x <- dfMetadata();
	
	checkEquals(0L, length(x), "empty dfMetadata");
	checkEquals(NULL, key(x), "empty dfMetadata has NULL key");
	checkEquals(TRUE, allowNewColumns(x), 
		"empty dfMetadata has allow.new.columns=TRUE");
})

# Test setting the variables component in construction of 'dfMetadata' objects
test_dfMetadata_vars <- svTest(function() {
	checkEquals(varMetadata("logical"),
		dfMetadata("logical")$X1, "auto-column names");
		
    checkEquals(varMetadata("character"), dfMetadata(Pdt="character")$Pdt, 
		"expand type name to column list");

    checkEquals(varMetadata(NA), dfMetadata(Pdt=NA)$Pdt, "NA type");
    checkEquals(varMetadata(NA), dfMetadata(Pdt=list(NULL))$Pdt, "NULL type");
    
    checkEquals(varMetadata("integer", na.ok=FALSE, default=2.0),
    	dfMetadata(Pdt="integer", na.ok=FALSE, default=2.0)$Pdt, 
		"set 'na.ok' and 'default'");

    checkEquals(varMetadata(type="double", na.ok=FALSE, default=5.0),
    	dfMetadata(Pdt=list(type="double", na.ok=FALSE, default=5L))$Pdt, 
		"specify all column data as a named list");
	
    checkEquals(varMetadata(type="factor", coerce="as.factor", na.ok=FALSE, 
			default=factor("empty")),
    	dfMetadata(Pdt=list("factor", "as.factor" , FALSE, "empty"))$Pdt, 
		"specify all column data as an unnamed list");

	st <- Sys.time();
    checkEquals(varMetadata(type="POSIXct", default=st, na.ok=FALSE),
    	dfMetadata(Pdt=list("POSIXct", NULL , default=st, FALSE))$Pdt, 
		"specify all column data as a partially unnamed list");
	
	x <- dfMetadata(Pdt=list("Date", NULL, FALSE, "2008-01-01"), 
		na.ok=TRUE, default="2008-02-02");
    checkEquals(
		varMetadata(type="Date", na.ok=TRUE, default=as.Date("2008-02-02")), 
		x$Pdt, 
		"'required' and 'default' arguments have precedence over column list settings");

    checkEquals(varMetadata(type="character", na.ok=TRUE),
    	dfMetadata(Pdt=list("character", na.ok=FALSE), na.ok=NA)$Pdt, 
		"required=NA respects column list Required setting");

	x <- dfMetadata(list(na.ok="integer", default="factor"), 
		na.ok="default", default=list(1, "a"));
    checkEquals(varMetadata(type="integer", na.ok=FALSE, default=1L), x$na.ok, 
		"use a single unnamed list first argument to set a 'na.ok' column");
    checkEquals(varMetadata(type="factor", na.ok=TRUE, default=factor("a")),
    	x$default, 
		"use a single unnamed list first argument to set a 'default' column");
	
	x <- dfMetadata(Pdt=list("character", default=1), default=list(Pdt=NULL));
    checkEquals(NA_character_, varDefault(x$Pdt), 
		"default=NULL resets current default setting");

	x <- dfMetadata(Pdt=list("character", default=character(0)));
    checkEquals(NA_character_, varDefault(x$Pdt),
		"zero-length default value is transformed to NA");

	x <- suppressWarnings(dfMetadata(Pdt=list("integer", default=c(1,2))));
	checkEquals(1L, varDefault(x$Pdt), 
		"only first element of default values of length > 1 are retained");	
	
	x <- dfMetadata(D =
			varMetadata("Date", default=quote(as.Date("2008-01-01"))));
	checkEquals(1L, length(x), 
		"this failed when varMetadata was changed to store common specs in attributes")
	
	x <- dfMetadata(Pdt="character", 
		Ccy=list(coerce=quote(factor(x, levels=c("EUR", "USD"), ordered=TRUE)),
			type="factor", na.ok=FALSE, default="EUR"),
		At=varMetadata("Date"),
    	default=list(At=quote(Sys.Date())),
		na.ok=list(At=FALSE, TRUE));
	checkEquals(varMetadata(type="character", na.ok=TRUE), 
		x$Pdt, 
		"use list for 'na.ok' and 'default', element 'Pdt'");
	checkEquals(varMetadata(
			coerce=quote(factor(x, levels=c("EUR", "USD"), ordered=TRUE)), 
			type="factor", na.ok=TRUE,  
			default=factor("EUR", ordered=TRUE, levels=c("EUR", "USD"))),
		x$Ccy, 
		"use list for 'required' and 'defaults', element 'Ccy'");
	checkEquals(varMetadata(type="Date", na.ok=FALSE, default=quote(Sys.Date())), 
		x$At, 
		"use list for 'na.ok' and 'default', element 'At'");	
})

# Tests errors that may arise when constructing the columns of a 'dfMetadata'
# object
test_dfMetadata_columnErrors <- svTest(function() { 	
	checkException(dfMetadata(A="logical", na.ok="B"), 
		"'required' names must match column names");
	checkException(dfMetadata(A="logical", na.ok=list(B=TRUE)), 
		"'required' elements names when it is a list must match column names");

	checkException(dfMetadata(A="integer", default=list(B=1, A=3)), 
		"'defaults' elements names must match column names");
	
	checkException(dfMetadata(A="integer", A="logical"), 
		"column names cannot be duplicated");		
})

# Test setting of Primary Key component in construction of 'dfMetadata' objects
test_dfMetadata_keys <- svTest(function() { 
	x <- dfMetadata(AB="integer");
	checkEquals(NULL, key(x), "primary key not set returns NULL");
		
	x <- dfMetadata(A="integer", B="logical", key=c("B", "A"));
	checkEquals(c("B", "A"), key(x), "simple primary key");
	
	x <- dfMetadata(AB="integer", BA="logical", key=c("B", "A"));
	checkEquals(c("BA", "AB"), key(x), "primary key with names partial matching");

	x <- dfMetadata(AB="integer", BA="logical", key=2);
	checkEquals("BA", key(x), "primary key using an integer index");
	
	x <- dfMetadata(AB="integer", BA="logical", key=c(TRUE, FALSE));
	checkEquals("AB", key(x), "primary key using a logical index");
		
	checkException(dfMetadata(A="integer", key="C"), 
		"raise error when using a key that does not match a column name");
	checkException(dfMetadata(A="integer", key=2), 
		"raise error when using a key with an out-of-range index");
	
});

# Test setting of AllowNewColumns component in construction of 'dfMetadata' 
# objects
test_dfMetadata_allowNewColumns <- svTest(function() { 
	x <- dfMetadata(AB="integer");
	checkEquals(TRUE, allowNewColumns(x), "AllowNewColumns not set returns TRUE");
		
	x <- dfMetadata(AB="integer", allow.new.columns=FALSE);
	checkEquals(FALSE, allowNewColumns(x), "simple AllowNewColumns");
	
	x <- dfMetadata(AB="integer", allow.new.columns=NULL);
	checkEquals(TRUE, allowNewColumns(x), "NULL AllowNewColumns returns TRUE");
	
	x <- dfMetadata(AB="integer", allow.new.columns=c(NA, FALSE));
	checkEquals(TRUE, allowNewColumns(x), "NA AllowNewColumns returns TRUE");	
})

# Test subsetting method [ of dfMetadata 
test_dfMetadata_subset <- svTest(function() {  
	x <- dfMetadata(Pdt="character", 
		Ccy=list(type="factor", na.ok=TRUE, default="EUR", 
			coerce=quote(factor(x, levels=c("EUR", "USD"), ordered=TRUE))),
		At=list("Date"), 
		default=list(At=quote(Sys.Date())), 
		na.ok=list(Pdt=TRUE, FALSE));	
	y <- dfMetadata(Ccy=list(type="factor", na.ok=FALSE, default="EUR", 
			coerce=quote(factor(x, levels=c("EUR", "USD"), ordered=TRUE))));
	y2 <- dfMetadata(Ccy=list(type="factor", na.ok=FALSE, default="EUR", 
			coerce=quote(factor(x, levels=c("EUR", "USD"), ordered=TRUE))),
		At=list(type="Date", na.ok=FALSE, default=quote(Sys.Date())));

	checkEquals(y, x["Ccy"]);	
	checkEquals(y, x[2]);
	checkEquals(y, x[c(FALSE, TRUE, FALSE)]);
	checkEquals(y, x[-c(1,3)]);

	checkEquals(y2, x[c("Ccy", "At")]);
	checkEquals(y2, x[c(2, 3)]);
	checkEquals(y2, x[-1]);
	checkEquals(y2, x[c(FALSE, TRUE, TRUE)]);
	
	checkEquals(0, length(x[0]));
	checkEquals(0, length(x[NULL]));
	checkTrue(is.dfMetadata(x[NULL]));
	checkEquals(0, length(x[integer(0)]));
	checkTrue(is.dfMetadata(x[character(0)]));
	
	checkException(x[4]);
	checkException(x["InexistingName"]);
	checkException(x[c(TRUE, TRUE, TRUE, FALSE)]);	
})

# Test subset replacement method `[<-` of dfMetadata 
test_dfMetadata_subsetReplace <- svTest(function() { 
	x <- dfMetadata(AB="integer", CD="logical");
	
	y <- x; y[c("CD", "AB")] <- c("numeric", "character");	
	checkEquals(list(AB="character", CD="numeric"), varType(y),
		"replace column spec using 'i' and setting classes as character vector");
	
	y <- x; y[c("AB", "CD")] <- list(c("Date", "POSIXct"), "character");
	checkEquals(list(AB=c("Date", "POSIXct"), CD="character"), varType(y));
		
	y <- x; 
	y[c(2, 1)] <- list(
		XJ=list("character", quote(as.character(x)), FALSE, "a"), 
		KW="logical");
	checkEquals(varMetadata("character", quote(as.character(x)), FALSE, "a"), 
		y$CD,
		"replace column spec using integer 'i' and setting only class names");

	y <- x; 
	y[c(FALSE, TRUE)] <- "factor";
	checkEquals(varMetadata("factor"), y$CD,
		"replace column spec using logical index");
	
	y <- x;	y[NULL] <- "character";
	checkEquals(x, y);

	y[logical(0)] <- "character";
	checkEquals(x, y);

	y <- x; y[] <- NULL;
	checkEquals(0L, length(y));
	checkTrue(is.dfMetadata(y));
	
	y <- x; y["CD"] <- NULL;
	checkEquals("AB", names(y), "remove one column setting it to NULL");
		
	y <- x;
	y[c("CD", "AB")] <- NULL;
	checkEquals(character(0), names(y), 
		"remove several columns setting them to NULL");
	
	y <- x;	y[c(TRUE, FALSE)] <- NULL;
	checkEquals("CD", names(y), 
		"remove one column setting it to NULL, indexing with logical");
	
	# Add new elements
	y <- x; y[c(3, 4)] <- "factor"
	checkEquals(list(AB="integer", CD="logical", X3="factor", X4="factor"), 
		varType(y));
	
	y <- x; y["X"] <- list(na.ok=FALSE);
	checkEquals(structure(c(TRUE, TRUE, FALSE), names=c("AB", "CD", "X")),
    	naOk(y));
	
	y <- x; y[c(3,4)] <- list(Y=varMetadata("character"), J="logical");
	checkEquals(c("AB", "CD", "Y", "J"), names(y));
	
	# Test adding at non-contiguous positions!
	y <- x; y[4] <- "character";
    checkEquals(3L, length(y), 
		"Adding at non-contiguous positions removes elements in the middle");
})

test_dfMetadata_varTypeReplace <- svTest(function() {
	x <- dfMetadata(X="integer", Y="logical", Z=NA);
	checkEquals(list(X="integer", Y="logical", Z=NA_character_), varType(x));
	
	y <- x; suppressWarnings(varType(y) <- c("factor", "character"));	
	checkEquals(list(X="factor", Y="character", Z="factor"), varType(y));
	
	y <- x; varType(y) <- list(Z=c("integer", "numeric"), NA);	
	checkEquals(list(X=NA_character_, Y=NA_character_, Z=c("integer", "numeric")), 
		varType(y));

	y <- x; varType(y) <- list(Y=NULL, X=quote(is.character(x)));	
	checkEquals(list(X=quote(is.character(x)), Y=NA_character_, Z=NA_character_), 
		varType(y));
	
	y <- x; varType(y) <- NULL;	
	checkEquals(list(X=NA_character_, Y=NA_character_, Z=NA_character_), 
		varType(y));

	# Errors
	checkException(varType(y["X"]) <- list(Z="logical"));
	checkException(varType(y) <- list(J="character"));
	checkException(varType(y[4]) <- NA);
	checkException(varType(y["J"]) <- NA);
	
	checkException(varType(y, "X") <- quote(x / 2));
	checkException(varType(y) <- 1);
})

# Compares the 2 ways or replacing subsets of varType with [: 
# 	1) varType(x, i) <- v
#   2) varType(x)[i] <- v
#   3) varType(x[i]) <- v
test_dfMetadata_varTypeReplaceSubset<- svTest(function() {
	x <- dfMetadata(X="integer", Y="logical", Z=NA);
	varCoerce(x) <- list("as.integer", "as.logical", "as.numeric");
	checkEquals(list(X="integer", Y="logical", Z=NA_character_), varType(x));
		
	y <- x; varType(y, "Z") <- is.numeric;
	checkEquals(list(X="integer", Y="logical", Z=is.numeric), varType(y));	
	
	# Beware: Next line fails if not wrapping the value into a list
	y <- x; varType(y)["Z"] <- list(is.numeric);
	checkEquals(list(X="integer", Y="logical", Z=is.numeric), varType(y));	
	
	# Replacing using varType(y[i]) syntax fails misserably: apparently nothing 
	# is changed in y, but an spurious user-defined element named "Z" is added 
	# to Z
	y <- x; varType(y["Z"]) <- list(is.numeric);
	checkEquals(varType(x), varType(y));	
	#checkEquals(x, y);	# This fails!
	
	checkException(varType(y, "Z") <- 1);
	checkException(varType(y)["Z"] <- 1);
	checkException(varType(y["Z"]) <- 1);

	checkException(varType(y, 4) <- is.numeric);
	# BEWARE: Next case doesn't produce an error, but instead of modifying
	# varType it creates an spurious element in z$Z
	y <- z <- x; varType(z)[4] <- list(is.numeric);
	checkEquals(y, z);
	checkException(varType(y[4]) <- is.numeric);	
})

test_dfMetadata_varTypeReplaceSubsetWithNamedList <- svTest(function() {
	x <- dfMetadata(X="integer", Y="logical", Z=NA);
	varCoerce(x) <- list("as.integer", "as.logical", "as.numeric");
		
	y <- x; varType(y[c(1, 3)]) <- quote(is.integer(x));
    checkEquals(list(X=quote(is.integer(x)), Y="logical", Z=quote(is.integer(x))), 
		varType(y));
	
	y <- x; 
	varType(y)[c("Z", "X")] <- list(quote(is.integer(x)), "character");
    checkEquals(list(X="character", Y="logical", Z=quote(is.integer(x))), 
		varType(y));	
	
	y <- x; varType(y[c("Z", "X")]) <- list(X="factor", NA);
    checkEquals(list(X="factor", Y="logical", Z=NA_character_), 
		varType(y), "match RHS by names (uses valParamList)");

	# BEWARE: With this syntax varType(y)[c("Z", "X")], the matching in
	# 	the RHS is done by order, NOT BY valParamList (i.e. NOT BY NAME)! 
	# TODO: Document varType(y)[c("Z", "X")] in the examples	
	y <- x; varType(y)[c("Z", "X")] <- list(X="factor", NA);
    checkEquals(list(X=NA_character_, Y="logical", Z="factor"), 
		varType(y));	
})

# Compares the 2 ways or replacing subsets of varType with [[: 
# 	1) varType(x, i)   <- v
#   2) varType(x)[[i]] <- v
#   3) varType(x[[i]]) <- v
test_dfMetadata_varTypeReplaceSubset2 <- svTest(function() {
	x <- dfMetadata(X="integer", Y="logical", Z=NA);
	varCoerce(x, "Z") <- as.numeric;
		
	y <- x; varType(y)[["Z"]] <- is.numeric;
	checkEquals(list(X="integer", Y="logical", Z=is.numeric), varType(y));	
	
	y <- x; varType(y[[3]]) <- is.numeric;
	checkEquals(list(X="integer", Y="logical", Z=is.numeric), varType(y));	
	
	checkException(varType(y)[["Z"]] <- 1);
	checkException(varType(y[["Z"]]) <- 1);

	# BEWARE: Next case doesn't produce an error, but does not have any
	# effect either, as varType(y) is a plain list.
	z <- y; varType(z)[[4]] <- is.numeric;
	checkEquals(y, z);
	# This fails, but error is much less informative that for varType(y, 4) <- v
	checkException(varType(y[[4]]) <- is.numeric);	
})

# Compares the 2 ways or replacing subsets of varType with $: 
# 	1) varType(x, i) <- v
#   2) varType(x)$i  <- v
#   3) varType(x$i)  <- v
test_dfMetadata_varTypeReplaceSubset3 <- svTest(function() {
	x <- dfMetadata(X="integer", Y="logical", Z=NA);
	varCoerce(x, "Z") <- as.numeric;
				
	y <- x; varType(y)$Z <- is.numeric;
	checkEquals(list(X="integer", Y="logical", Z=is.numeric), varType(y));	
	
	y <- x; varType(y$Z) <- is.numeric;
	checkEquals(list(X="integer", Y="logical", Z=is.numeric), varType(y));	
	
	checkException(varType(y)$Z <- 1);
	checkException(varType(y$Z) <- 1);

	checkException(varType(y, "W") <- is.numeric);
	checkException(varType(y)$W <- is.numeric);
	# This fails, but because y$W is NULL
	checkException(varType(y$W) <- is.numeric);	
})

test_dfMetadata_varCoerceReplace <- svTest(function() { 
	x <- dfMetadata(X="integer", Y="logical", Z=NA);
	checkEquals(list(X=NULL, Y=NULL, Z=NULL), varCoerce(x));
	
	y <- x;
    qx <- quote(as.numeric(x)); 
	varCoerce(y) <- qx;
	checkEquals(list(X=qx, Y=qx, Z=qx), varCoerce(y));
	
	varCoerce(y) <- list(Z=as.numeric, NULL)
	checkEquals(list(X=NULL, Y=NULL, Z=as.numeric), varCoerce(y));

	varCoerce(y, c(2, 3)) <- "as.integer"
	checkEquals(list(X=NULL, Y="as.integer", Z="as.integer"), varCoerce(y));
	
	# Errors
	checkException(varCoerce(y, "X") <- list(Z="as.logical"));
	checkException(varCoerce(y) <- list(J="as.character"));
	checkException(varCoerce(y, 4) <- NA);
	checkException(varCoerce(y, "J") <- NA);
	
	checkException(varCoerce(y, "X") <- "abcdefg_12345");
	checkException(varCoerce(y) <- 1);
	
	y <- x; 
	varCoerce(y) <- "as.integer";
	varType(y) <- quote(is.integer(x));
	checkException(varCoerce(y) <- NULL);	
})

test_dfMetadata_naOkReplace <- svTest(function() { 
	x <- dfMetadata(X=NA, Y=NA, Z=NA);
	checkEquals(structure(c(TRUE, TRUE, TRUE), names=c("X", "Y", "Z")),
		naOk(x), checkNames=FALSE);
	
	# Full specs
	y <- x; naOk(y) <- c(FALSE, TRUE);
	checkEquals(c(FALSE, TRUE, FALSE), naOk(y), checkNames=FALSE);
	
	y <- x; naOk(y) <- c("Z", "X");
	checkEquals(c(TRUE, FALSE, TRUE), naOk(y), checkNames=FALSE);
		
	y <- x; naOk(y) <- FALSE; naOk(y) <- c();
	checkEquals(c(TRUE, TRUE, TRUE), naOk(y), checkNames=FALSE);

	y <- x; naOk(y) <- FALSE; naOk(y) <- list(Y=FALSE, TRUE);
	checkEquals(c(TRUE, FALSE, TRUE), naOk(y), checkNames=FALSE);

	y <- x; naOk(y) <- FALSE; naOk(y) <- list(Y=NULL);
	checkEquals(c(FALSE, TRUE, FALSE), naOk(y), checkNames=FALSE);
	
	# Partial specs
	y <- x; naOk(y, c(3,1)) <- c(TRUE, FALSE);
 	checkEquals(c(FALSE, TRUE, TRUE), naOk(y), checkNames=FALSE);

	y <- x; naOk(y, TRUE) <- FALSE;
 	checkEquals(c(FALSE, FALSE, FALSE), naOk(y), checkNames=FALSE);

	y <- x; suppressWarnings(naOk(y, c(FALSE, TRUE)) <- c(FALSE, FALSE));
 	checkEquals(c(TRUE, FALSE, TRUE), naOk(y), checkNames=FALSE);
	
	y <- x; naOk(y, "Z") <- NA;
 	checkEquals(c(TRUE, TRUE, TRUE), naOk(y), checkNames=FALSE);
	
	y <- x; naOk(y) <- list(Z=FALSE, X=FALSE);
	checkEquals(c(FALSE, TRUE, FALSE), naOk(y), checkNames=FALSE);

	# This case is very particular: the matching on the RHS items is always
	# done by its **name**. Argument 'vars' is only used here to *restrict*
	# the variables that will have its 'na.ok' replaced.
	y <- x; naOk(y) <- FALSE; naOk(y, c("Z", "X")) <- list(X=FALSE, TRUE);
	checkEquals(c(FALSE, FALSE, TRUE), naOk(y), checkNames=FALSE);
			
	# Errors
	checkException(naOk(y) <- 2);
	checkException(naOk(y) <- list(A=TRUE));
	checkException(naOk(y, c("X", "Y")) <- list(Z=TRUE));
	checkException(naOk(y, 4) <- FALSE);	
});

test_dfMetadata_varDefaultReplace <- svTest(function() {
	x <- dfMetadata(X="integer", Y="logical", Z=NA);
	checkEquals(list(X=NA_integer_, Y=NA, Z=NA), varDefault(x));
	
	y <- x; varDefault(y) <- list(1, TRUE, 0);	
	checkEquals(list(X=1L, Y=TRUE, Z=0), varDefault(y));
	
	varDefault(y) <- list(Z="a");	
	checkEquals(list(X=1L, Y=TRUE, Z="a"), varDefault(y));
	
	varDefault(y) <- list(Y=FALSE, NA);	
	checkEquals(list(X=NA_integer_, Y=FALSE, Z=NA), varDefault(y));
	    	
	varDefault(y, c(1, 3)) <- list(Z=FALSE, 1.2);	
	checkEquals(list(X=1L, Y=FALSE, Z=FALSE), varDefault(y));

	varDefault(y, "Y") <- TRUE;	
	checkEquals(list(X=1L, Y=TRUE, Z=FALSE), varDefault(y));
	
	varDefault(y, -1) <- 0;	
	checkEquals(list(X=1L, Y=FALSE, Z=0), varDefault(y));

	varDefault(y, c(2, 3)) <- NULL;	
	checkEquals(list(X=1L, Y=NA, Z=NA), varDefault(y));
	
	y <- x; varDefault(y, "Y") <- function(x) TRUE;
	checkEquals(list(X=NA_integer_, Y=function(x) TRUE, Z=NA), varDefault(y));
    
	# Errors
	checkException(varDefault(y, "X") <- list(Z=7));
	checkException(varDefault(y) <- list(J=7));
	checkException(varDefault(y, 4) <- NA);
	checkException(varDefault(y, "J") <- NA);
})

test_dfMetadata_names <- svTest(function() {
	x <- dfMetadata(X="integer", Y="logical", Z=NA);

	key(x) <- c(1, 3);
	checkEquals(c("X", "Z"), key(x));

	names(x) <- c("A", "B", "C");	
	checkEquals(c("A", "B", "C"), names(x));
	checkEquals(c("A", "C"), key(x));
	
	checkException(names(x) <- NULL);
	checkException(names(x) <- c("a", "b"));
	checkException(names(x) <- c("a", "b", "a"));
})

test_dfMetadata_c <- svTest(function() {
	x <- dfMetadata(X="integer", Y="logical");
	y <- dfMetadata(Z="complex");
	
	checkEquals(x, c(x));
	checkEquals(dfMetadata(X="integer", Y="logical", Z="complex"), c(x, y));
	checkException(c(x, x));
})