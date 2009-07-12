################################################################################
# R PACKAGE:   mframe
# FILE:        inst/unitTests/runit_varMetadata.R
# DESCRIPTION: Test suite for the varMetadata S3 class. To be run with svUnit.
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  15/06/2009
# LICENSE:     GPL-2
################################################################################


# Tests that 'current' contains ALL elements of 'target' with the same values,
# but disregards the order of elements as well as any additional elements in
# 'current' that are not in 'target'. Matching of elements is done by their
# names.
allCommonEqual <- function(target, current, check.attributes=TRUE, ...) {
	current <- current[names(target)];
	all.equal.list(target, current, check.attributes=check.attributes, ...);
} 

test_varMetadata_empty <- svTest(function() {
	x <- varMetadata();
	
	checkEquals(0L, length(x), "empty varMetadata");
	checkEquals(0L, length(names(x)), "empty varMetadata's names");
	
	checkEquals(NA_character_, varType(x), "empty varMetadata's varType");
	checkEquals(NA_character_, x$type, "empty varMetadata's $type");
	#checkEquals(NA_character_, x$varType, "empty varMetadata's $varType");
	
	checkEquals(NULL, varCoerce(x), "empty varMetadata's varCoerce");
	checkEquals(NULL, x$coerce, "empty varMetadata's $coerce");
	#checkEquals(NULL, x$varCoerce, "empty varMetadata's $varCoerce");
	
	checkEquals(TRUE, naOk(x), "empty varMetadata's naOk");
	checkEquals(TRUE, x$na.ok, "empty varMetadata's $na.ok");
	#checkEquals(TRUE, x$naOk, "empty varMetadata's $naOk");
	
	checkEquals(NA, varDefault(x), "empty varMetadata's varDefault");
	checkEquals(NA, x$default, "empty varMetadata's $default");
	#checkEquals(NA, x$varDefault, "empty varMetadata's $varDefault");
})

test_varMetadata_varTypeOnConstruction <- svTest(function() {
	checkEquals(NA_character_, varType(varMetadata(NA)));
	checkEquals(NA_character_, varType(varMetadata(NULL)));	
	checkEquals("character", varType(varMetadata("character")));
	checkEquals(c("character", "integer"), 
		varType(varMetadata(type=c("character", "integer"))));
	checkEquals(NA_character_, 
		varType(varMetadata(c("character", NA, "integer"))));
	
	checkEquals(quote(is.numeric(x) && all(x > 0)), 
		varType(varMetadata(quote(is.numeric(x) && all(x > 0)), as.numeric)));
	
	checkEquals(is.numeric, varType(varMetadata(is.numeric, as.numeric)));

	checkEquals(is.numeric, varType(varMetadata(is.numeric, as.numeric)));
	
	checkException(varMetadata(1));
	checkException(varMetadata(list("character")));
	# Following test does not pass: in principle any character string
	# can name a valid S3 class. If no default is tried to be assigned,
	# there's no way to know whether it's valid or not... REVIEW	
	#checkException(varMetadata("abcdefg_12231")); 
	checkException(varMetadata(quote(is.numeric)));
	checkException(varMetadata(quote(x + 1)));
	checkException(varMetadata(function(x) x + 1));	
})

test_varMetadata_varCoerceOnConstruction <- svTest(function() {
	checkEquals(NULL, varCoerce(varMetadata("character", NULL)));
	checkEquals("as.character", 
		varCoerce(varMetadata(coerce="as.character")));		
	checkEquals(as.character,  
		varCoerce(varMetadata("character", as.character)));
	checkEquals(quote(x+1), varCoerce(varMetadata(coerce=quote(x+1))));
	checkEquals(quote(as.numeric), 
		varCoerce(varMetadata(coerce=quote(as.numeric))));
	
	checkException(varMetadata(coerce=c("as.integer", "as.logical")));
	checkException(varMetadata(coerce="abcdefg_12231"));	
	checkException(varMetadata(is.numeric), 
		"varCoerce is required when varType is not a character vector");
})

test_varMetadata_naOkOnConstruction <- svTest(function() {
	checkEquals(TRUE, naOk(varMetadata(na.ok=TRUE)));
	checkEquals(FALSE, naOk(varMetadata(na.ok=FALSE)));
	checkEquals(TRUE, naOk(varMetadata(na.ok=NA)));
	checkEquals(TRUE, naOk(varMetadata(na.ok=NULL)));
	
	checkEquals(FALSE, naOk(varMetadata(na.ok=c(FALSE, TRUE, NA))));
	checkEquals(TRUE, naOk(varMetadata(na.ok=c(NA, FALSE))));
	
	checkException(naOk(varMetadata(na.ok=0)));
	checkException(naOk(varMetadata(na.ok="abc")));
})

test_varMetadata_varDefaultOnConstruction <- svTest(function() {
	checkEquals(NA_character_, varDefault(varMetadata("character")));
	checkEquals(NA, varDefault(varMetadata(c("logical", "integer"))));
	checkEquals(NA_integer_, 
		varDefault(varMetadata(type=is.integer, coerce=as.integer)));
	checkEquals(factor("a"), 
		varDefault(varMetadata(type="factor", default="a")));
	
	checkEquals(as.Date("1999-01-18"), 
		varDefault(varMetadata(type="Date", 
			coerce=quote(as.Date(x, format="%d/%m/%Y")), default="18/1/1999")));
	
	checkEquals(quote(Sys.Date()),
		varDefault(varMetadata("Date", default=quote(Sys.Date()))));
	
	checkEquals(12L, varDefault(varMetadata("integer", default=12.3)));
	checkEquals(NA_integer_, varDefault(varMetadata("integer", default=NULL)));	
})

test_as.varMetadata <- svTest(function() {
	x <- varMetadata("character")
	
	checkEquals(x, as.varMetadata(x));	
	checkEquals(x, as.varMetadata("character"));
	checkEquals(x, as.varMetadata(list("character")));
	checkEquals(x, as.varMetadata(list(type="character")));

	checkEquals(varMetadata(coerce=quote(factor(x, levels=c("EUR", "USD")))), 
		as.varMetadata(list(coerce=quote(factor(x, levels=c("EUR", "USD"))))));
})

test_varMetadata_otherElementsOnConstruction <- svTest(function() {
	x <- varMetadata(label="This Var", units=5);
    checkTrue(is.varMetadata(x));
	checkEquals("This Var", x$label);
	checkEquals(5, x[["units"]]);
	
#	checkException(varMetadata(varType=5));
#	checkException(varMetadata(naOk=TRUE));
#	checkException(varMetadata(varCoerce=quote(as.numeric)));
#	checkException(varMetadata(varDefault=NA));
})

test_varMetadata_subset3 <- svTest(function() {
	x <- varMetadata(type="character", na.ok=FALSE, coerce="as.character", 
		label="Test", default="");
	y <- varMetadata();
	
	checkEquals(x$type, varType(x));
	checkEquals(x$varType, varType(x));
	checkEquals(y$type, varType(y));
	
	checkEquals(x$coerce, varCoerce(x));
	checkEquals(x$varCoerce, varCoerce(x));
	checkEquals(y$coerce, varCoerce(y));

	checkEquals(x$na.ok, naOk(x));
	checkEquals(x$naOk, naOk(x));
	checkEquals(y$na.ok, naOk(y));

	checkEquals(x$default, varDefault(x));
	checkEquals(x$varDefault, varDefault(x));
	checkEquals(y$default, varDefault(y));
	
	checkEquals("Test", x$label);
})


test_varMetadata_subsetReplace3 <- svTest(function() {  		
	x <- varMetadata(); 
	x$label <- "A Variable" 	
	checkEquals(varMetadata(label="A Variable"), x);
	
	checkException(x$na.ok <- "a");
    checkException(x$naOk <- "a");
	suppressWarnings(x$na.ok <- c(FALSE, TRUE));
	checkEquals(FALSE, naOk(x));
	suppressWarnings(x$naOk <- c(TRUE, FALSE));
    checkEquals(TRUE, naOk(x));
	
	checkException(x$type <- 1);
    checkException(x$varType <- 1);
	varCoerce(x) <- "as.Date";	# Otherwise a non-fixed varType issues an error
	# If we don't wrap the quote() in list(), the wrong assignment is made
	# both for list() and for varMetadata()
	x$type <- quote(is(x, "Date"));
	checkEquals(quote(is(x, "Date")), varType(x));
	x$varType <- "character";
	checkEquals("character", varType(x));

	checkException(x$coerce <- 1);
    checkException(x$varCoerce <- 1);
	x$coerce <- quote(as.Date(x));
	checkEquals(quote(as.Date(x)), varCoerce(x));
	x$varCoerce <- "as.Date";
	checkEquals("as.Date", varCoerce(x));

})

test_varMetadata_c <- svTest(function() {
	x <- varMetadata("factor");
	
	y <- c(x, label="A factor", unit="eur");
	checkTrue(is.varMetadata(y));
    checkEquals(
		as.list(varMetadata(type="factor", label="A factor", unit="eur")),
		as.list(y));
	
#	checkException(c(x, type="character"));
#	checkException(c(x, more=1, varCoerce="character"));
#	checkException(c(x, na.ok=TRUE));
#	checkException(c(x, varDefault="a"));	
})


test_varMetadata_enforce <- svTest(function() {
	checkEquals(c(0, -1, 2.7, NA),
		enforce(varMetadata(), c(0, -1, 2.7, NA)));
	checkEquals(c(FALSE, TRUE, TRUE, NA),
		enforce(varMetadata("logical"), c(0, -1, 2.7, NA)));
	checkEquals(c(FALSE, TRUE, TRUE, NA),
		enforce(varMetadata("logical"), c(FALSE, TRUE, TRUE, NA)));	
	checkEquals(c(1, 0, 3.7, NA),
		enforce(varMetadata("logical", quote(x+1)), c(0, -1, 2.7, NA)));	
	checkEquals(character(0), enforce(varMetadata("character"), logical(0)));	
	
	checkException(
		enforce(varMetadata("logical", na.ok=FALSE), c(FALSE, TRUE, TRUE, NA)));	
})

test_varMetadata_varTypeReplace <- svTest(function() {
	x <- varMetadata(); 
		
	checkException(varType(x) <- 1);
	varCoerce(x) <- "as.Date";	# Otherwise a non-fixed varType issues an error
	varType(x) <- quote(is(x, "Date"));
	checkEquals(quote(is(x, "Date")), varType(x));
	varType(x) <- "character";
	checkEquals("character", varType(x));
	
	checkEquals(0L, length(x), "no user-def elements have been added")
	
	# Coercion
	x <- varMetadata("numeric", default=5.2);
	checkEquals(5.2, varDefault(x));	
	
	varType(x) <- "integer"
	checkEquals(5L, varDefault(x), "re-coerce default when varType changes");
})

test_varMetadata_varCoerceReplace <- svTest(function() {
	x <- varMetadata();
	
	checkException(varCoerce(x) <- 1);
	varCoerce(x) <- quote(as.Date(x));
	checkEquals(quote(as.Date(x)), varCoerce(x));
	varCoerce(x) <- "as.Date";
	checkEquals("as.Date", varCoerce(x));				
	
	checkEquals(as.Date(NA), varDefault(x));
	varCoerce(x) <- "as.integer";
	checkEquals(NA_integer_, varDefault(x), 
		"re-coerce default when varCoerce changes");
	
	checkEquals(0L, length(x), "no user-def elements have been added")	
})

test_varMetadata_naOkReplace <- svTest(function() {
	x <- varMetadata();
	
	checkException(naOk(x) <- "a");
	suppressWarnings(naOk(x) <- c(FALSE, TRUE));
	checkEquals(FALSE, naOk(x));
	suppressWarnings(naOk(x) <- c(TRUE, FALSE));
    checkEquals(TRUE, naOk(x));			
	
	checkEquals(0L, length(x), "no user-def elements have been added")	
})

test_varMetadata_varDefaultReplace <- svTest(function() {
	x <- varMetadata("integer");
	
	checkEquals(NA_integer_, varDefault(x), 
		"default varDefault for numeric varMetadata");
	suppressWarnings(varDefault(x) <- c(5.2, 6.2))
	checkEquals(5L, varDefault(x), 
		"select first element when default has length > 1, and coerce it");    
})
