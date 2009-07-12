################################################################################
# R PACKAGE:   mframe
# FILE:        R/generics.R
# DESCRIPTION: Generic functions and their default methods. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  26/05/2009
# LICENSE:     GPL-2
################################################################################

#setOldClass("varMetadata");
#setOldClass("dfMetadata");
#setOldClass("mframe");

#' Functions to get or set an object's metadata.  
#' 
#' Metadata (also known as "data dictionaries") represent 
#' meta-information about an object. This package provides
#' classes \code{\link{varMetadata}} and \code{\link{dfMetadata}} to 
#' represent metadata for variables (usually stored as  data frame's columns) 
#' and data frames, respectively, and class \code{\link{mframe}} to store
#' data frames together with their associated metadata. 
#'  
#' \code{metadata} is an \link[base:UseMethod]{S3 generic} accessor 
#' function, and \code{metadata<-} is an S3 generic replacement function. 
#' The default methods get and set the 
#' "metadata" attribute of the object, except when the first argument of 
#' \code{metadata} is a string. 
#' In this case, the string is interpreted as a class name and method 
#' \code{metadata.<class>} is invoked. 
#' This is useful to get metadata for a given class without an object of the
#' class.      
#'  
#' In addition, the replacement method \code{metadata<-} for \code{mframe}s 
#' modifies \code{x} to ensure that it complies with the new metadata,,
#' or raises an error if the metadata constraints cannot be satisfied.
#' Compliance may imply removing columns, coercing them to a different class, etc.
#' If the metadata is set to \code{NULL}, current metadata is removed, and
#' the \code{mframe} class is dropped, returning a plain data frame
#' (i.e. this is the same as calling \code{as.data.frame}).
#' 
#' @title Metadata of an Object
#' @aliases metadata metadata<- metadata.mframe metadata<-.mframe
#' @usage  
#' metadata(x, \dots)
#' metadata(x, \dots) <- value
#'  
#' \method{metadata}{mframe}(x, vars)
#' \method{metadata}{mframe}(x, vars) <- value
#' 
#' @param x an \R object.
#' @param vars specification of variables to select or replace. Accepts all
#'  standard indexing types (logical, numeric or character vectors). 
#' @param value an \R object representing metadata for \code{x}, or \code{NULL}
#'  to remove the metadata. 
#'  For objects of class \code{\link{mframe}}, \code{value} must be an object 
#'  of class \code{\link{dfMetadata}}, or that can be coerced to it.
#' @param \dots placeholder in the generic function to accomodate new arguments
#'  in methods.
#' 
#' @return For \code{metadata}, an object representing \code{x}'s metadata,
#'  or \code{NULL} if metadata is not found.
#' 
#'  For \code{metadata<-}, \code{x} is updated with the new metadata, and 
#'  possibly modified to comply with the metadata constraints. 
#' @seealso Classes \code{\link{varMetadata}}, \code{\link{dfMetadata}},
#' 	and \code{\link{mframe}}.
#' @export

metadata <- function(x, ...) UseMethod("metadata");

#' @name metadataReplace
#' @nord
#' @export `metadata<-`

`metadata<-` <- function(x, ..., value) UseMethod("metadata<-");

#' Functions to get or set the key of an object.  
#' 
#' Key specifications can be defined for metadata-related objects such as
#' those of classes \code{\link{dfMetadata}} and \code{\link{mframe}}. 
#' 
#' A key specification defines the parts of an object that compose
#' its key, such as column names whose combination should be unique
#' across rows in a data frame. This is similar to a table primary key in
#' relational databases.
#'
#' \code{key} is an \link[base:UseMethod]{S3 generic} accessor function, 
#' and \code{key<-} is an S3 generic replacement function. 
#' The default methods get and set the "key" attribute of the object.
#'     
#' Methods for \code{dfMetadata} and \code{mframe} always keep the key
#' specification as a character vector of variable names, even if they
#' are specified using an integer or logical index vector. 
#' 
#' Realize the \code{key} methods refer to the key \emph{specification}.
#' To get the actual values of an object that has a key defined, use the
#' \code{\link{keys}} function.     
#' 
#' @title Key of an Object
#' @aliases key key<- key.dfMetadata key<-.dfMetadata 
#'  key.mframe key<-.mframe
#' @usage  
#'  key(x, \dots)
#' 	key(x, \dots) <- value
#'  
#'  \method{key}{dfMetadata}(x) <- value
#'
#'  \method{key}{mframe}(x) 
#'  \method{key}{mframe}(x) <- value
#' 
#' @param x an \R object.
#' @param value a vector that defines the parts of \code{x} that compose
#'  its key. For \code{dfMetadata} and \code{mframe}s this is a character, 
#'  integer or logical vector that refers to variables (columns) of \code{x}.
#' @param \dots placeholder in the generic function to accomodate new arguments
#'  in methods.
#' 
#' @return For \code{key}, an object representing \code{x}'s key,
#'  normally a character vector of variable names, or \code{NULL} if a key 
#'  is not found.
#' 
#'  For \code{key<-}, \code{x} is updated, which may include modifying
#'  it to comply with the new key.
#' @seealso \code{\link{keys}}, \code{\link{metadata}}.
#' @export

key <- function(x, ...) UseMethod("key");

#' @name keyReplace
#' @nord
#' @export `key<-` 

`key<-` <- function(x, ..., value) UseMethod("key<-");

#' Function \code{keys} gets the values of the \link[key]{key} of an object.  
#' 
#' \code{keys} is an \link[base:UseMethod]{S3 generic} accessor function.
#' The default method grabs the \link[key]{key specification} for the object 
#' using the \code{key} method, and, if defined, uses it to index the object
#' via the generic extractor 
#' \code{\link[base:Extract]{[}}, coerces each element to character,
#' and concatenates all its items separating them by ":".
#'  
#' If the \code{key} method returns a zero-length object (like \code{NULL}), 
#' \code{keys} returns a zero-length character vector.  
#'    
#' Realize the \code{keys} method refers to the key \emph{values}.
#' To get the key specification for an object, use the \code{\link{key}} 
#' function.     
#' 
#' @title Values for the Key of an Object
#' @aliases keys keys.mframe
#' @usage  
#'  keys(x, \dots)
#'  
#'  \method{keys}{mframe}(x, vars)
#' 
#' @param x an \R object.
#' @param vars character vector of variable names that compose the key.
#' 	When \code{vars} is not specified, the metadata spec for the key is used.
#' @param value a vector that defines the parts of \code{x} that compose
#'  its key. For \code{dfMetadata} and \code{mframe}s these can be character, 
#'  integer or logical vectors that refer to columns of \code{x}.
#' @param \dots placeholder in the generic function to accomodate new arguments
#'  in methods.
#' 
#' @return For \code{keys}, a character vector with the key value for each 
#'  element of \code{x} (e.g. for each row of an \code{mframe}). 
#'  If a key specification is not found for \code{x}, a zero-length character
#'  vector.
#' 
#' @seealso \code{\link{key}}, \code{\link{metadata}}.
#' @export

keys <- function(x, ...) UseMethod("keys");

#' Functions to get and set the variable type(s) of an object.  
#' 
#' Variables types specifications can be defined for metadata-related objects 
#' such as those of classes \code{\link{varMetadata}}, \code{\link{dfMetadata}},
#' and \code{\link{mframe}}. 
#' 
#' Variable type is by default the class of each component of an object.  
#' For example, for lists or data frames, it is a list with the class of
#' each element. For metadata objects, the variable type is a broader 
#' concept that also includes expressions or functions to validate the data 
#' in the variable. For example, it may state ``this variable must be a 
#' positive, non-zero, numeric vector.'' 
#' 
#' \code{varType} is an \link[base:UseMethod]{S3 generic} accessor 
#' function, and \code{varType<-} is an S3 generic replacement function. 
#' The default methods get and set the variable type of several components of 
#' an object at once, coercing as necessary in the setting (replacement) case. 
#'
#' When applied to metadata objects, or to data objects having metadata, 
#' these methods refer to the type definition in the metadata specification, 
#' \emph{not to the actual class} of the data themselves.   
#'    
#' @title Metadata Specification of Types
#' @aliases varType varType<- 
#'  varType.varMetadata varType<-.varMetadata 
#'  varType.dfMetadata varType<-.dfMetadata
#'  varType.mframe varType<-.mframe 
#' @usage  
#'  varType(x, \dots)
#'  varType(x, \dots) <- value
#'
#' 	\method{varType}{varMetadata}(x)
#' 	\method{varType}{varMetadata}(x) <- value
#'  
#' 	
#' 	\method{varType}{dfMetadata}(x, vars)
#' 	\method{varType}{dfMetadata}(x, vars) <- value
#' 
#' 	\method{varType}{mframe}(x, vars)
#' 	\method{varType}{mframe}(x, vars) <- value
#' 
#' @param x an \R object.
#' @param vars subset of variables for which to get or set the type spec. 
#'  This can be any standard list subsetting index (character, integer, 
#'  or logical vector). An error is raised whenever this index does not point
#'  to a valid variable of \code{x}.
#' @param value specification of the variable type(s). This can take the 
#'  following forms for each variable:
#'  \itemize{
#' 		\item a character vector of class names, or \code{NA_character_}
#' 			to represent "any class" (which mimics the default data frame 
#' 			coercion behaviour.)
#' 		\item an expression that uses the variable \code{'x'} as data and 
#' 			returns a logical of length 1 (\code{TRUE} or \code{FALSE}). The
#' 			expression must be wrapped in a \code{\link[base:substitute]{quote}}
#' 			call for it to be evaluated later.
#' 		\item a function that returns a logical of length 1 (\code{TRUE} 
#' 			or \code{FALSE}). It will be invoked using a single unnamed 
#' 			argument with the variable data.
#' 		\item \code{NULL} to remove the current type spec (sets it to 
#' 			\code{NA_character_}).
#' 	}
#'  For \code{dfMetadata} and \code{mframe}s, \code{value} should be a 
#'  \emph{list} of types specifications. (TO BE FILLED) 
#'  
#' @param \dots placeholder in the generic function to accomodate new arguments
#'  in methods. 
#' 
#' @return For \code{varType}, the default method returns just the actual class 
#'  of each component of the object. 
#'  The method for \code{varMetadata} returns a single variable type 
#'  specification, and methods for \code{dfMetadata} and \code{mframe} 
#'  return a named list of types specifications. When \code{all = TRUE}
#'  is used with \code{mframe}s, variables that are not in the metadata 
#'  specification return \code{NA_character_}. 
#' 
#'  \code{varType<-} allows to modify all or some of the variable type(s) 
#'  specifications. 
#' 
#' @seealso \code{\link{metadata}}, and \code{\link[methods.as]{as}} in 
#'  package \pkg{methods}.
#' @export
 
varType <- function(x, ...) UseMethod("varType");

#' @name varTypeReplace
#' @nord
#' @export `varType<-`

`varType<-` <- function(x, ..., value) UseMethod("varType<-");

#' Functions to get and set the coercion behaviour for the variables
#' of an object.  
#' 
#' Variable coercion specifications can be defined for metadata-related objects 
#' such as those of classes \code{\link{varMetadata}}, \code{\link{dfMetadata}} 
#' and \code{\link{mframe}}. They establish how data must be coerced to
#' the target variable type, using any arbitrary function or expression.
#' 
#' Coercion need not be explicitly set when the 
#' \link[varType]{variable type spec} is a class name. In this case the 
#' \link[methods:as]{default coercion} is used
#' automatically. On the other hand, whenever the variable type spec is
#' an expression or function, specifying the coercion manually is required
#' as it cannot be deduced from the type. 
#' 
#' \code{varCoerce} is an \link[base:UseMethod]{S3 generic} accessor 
#' function, and \code{varCoerce<-} is an S3 generic replacement function. 
#' The default methods raise an error as these only apply to metadata-related 
#' objects. 
#'
#' @title Metadata Specification of Coercion Behaviour
#' @aliases varCoerce varCoerce<-
#'  varCoerce.varMetadata varCoerce<-.varMetadata 
#'  varCoerce.dfMetadata varCoerce<-.dfMetadata
#'  varCoerce.mframe varCoerce<-.mframe  
#' @usage  
#'  varCoerce(x, \dots)
#'  varCoerce(x, \dots) <- value
#'
#' 	\method{varCoerce}{varMetadata}(x)
#' 	\method{varCoerce}{varMetadata}(x) <- value
#'  
#' 	\method{varCoerce}{dfMetadata}(x, vars)
#' 	\method{varCoerce}{dfMetadata}(x, vars) <- value
#' 
#' 	\method{varCoerce}{mframe}(x, vars)
#' 	\method{varCoerce}{mframe}(x, vars) <- value
#' 
#' @param x an \R object.
#' @param vars subset of variables for which to get or set the coercion spec. 
#'  This can be any standard list subsetting index (character, integer, 
#'  or logical vector). An error is raised whenever this index does not point
#'  to a valid variable of \code{x}.
#' @param value specification of the variable coercion spec(s). This can take  
#'  the following forms for each variable:
#'  \itemize{
#' 		\item an expression that uses the variable \code{'x'} as data and 
#' 			returns \code{x} transformed (coerced). The expression must 
#' 			be wrapped in a \code{\link[base:substitute]{quote}}
#' 			call for it to be evaluated later.
#' 		\item a string (character vector or length 1) with the name of the
#'			function that must perform the coercion. It will be invoked 
#'  		using a single unnamed argument with the variable data. 			
#' 		\item a function, subject to the same constraints as the previous
#' 			case.
#' 		\item \code{NULL} to remove the current coercion spec.
#' 	}
#'  For \code{dfMetadata} and \code{mframe}s, \code{value} should be a 
#'  \emph{list} of coercion specifications. (TO BE FILLED) 
#'  
#' @param \dots placeholder in the generic function to accomodate new arguments
#'  in methods. 
#' 
#' @return For \code{varCoerce}, the method for \code{varMetadata} returns 
#'  a single variable coercion
#'  specification, and methods for \code{dfMetadata} and \code{mframe} 
#'  return a named list of coercion specs. When \code{all = TRUE}
#'  is used with \code{mframe}s, variables that are not in the metadata 
#'  specification return \code{NULL}. 
#' 
#'  \code{varType<-} allows to modify all or some of the variable(s) coercion 
#'  specifications. 
#' 
#' @seealso \code{\link{metadata}}, and \code{\link[methods.as]{as}} in 
#'  package \pkg{methods}.
#' @export 

varCoerce <- function(x, ...) UseMethod("varCoerce");

#' @name varCoerceReplace
#' @nord
#' @export `varCoerce<-`

`varCoerce<-` <- function(x, ..., value) UseMethod("varCoerce<-");

#' Functions to get and set whether an object should accept missing 
#' (\code{NA}) values or not.  
#' 
#' "NA Ok?" specifications can be defined for metadata-related objects 
#' such as those of classes \code{\link{varMetadata}}, \code{\link{dfMetadata}},
#' and \code{\link{mframe}}. 
#' 
#' \code{naOk} is an \link[base:UseMethod]{S3 generic} accessor function, 
#' and \code{naOk<-} is an S3 generic replacement function. 
#' The default methods get and set the 
#' "NA Ok?" specification of an object's metadata, issuing an error if the 
#' object does not have attached metadata.
#'
#' @title Are Missing (NA) Values Accepted?
#' @aliases naOk naOk<-
#'  naOk.varMetadata naOk<-.varMetadata 
#'  naOk.dfMetadata naOk<-.dfMetadata
#'  naOk.mframe naOk<-.mframe  
#' @usage  
#' 	naOk(x, \dots)
#' 	naOk(x, \dots) <- value
#'
#' 	\method{naOk}{varMetadata}(x)
#' 	\method{naOk}{varMetadata}(x) <- value
#'  
#' 	\method{naOk}{dfMetadata}(x, vars)
#' 	\method{naOk}{dfMetadata}(x, vars) <- value
#' 
#' 	\method{naOk}{mframe}(x, vars)
#' 	\method{naOk}{mframe}(x, vars) <- value
#' 
#' @param x an \R object.
#' @param vars subset of variables for which to get or set the "NA Ok?" spec. 
#'  This can be any standard list subsetting index (character, integer, 
#'  or logical vector). An error is raised whenever this index does not point
#'  to a valid variable of \code{x}.
#' @param value specification of whether \code{NA}s are accepted. For 
#'  single-variable objects (\code{varMetadata}) this must be a logical vector 
#'  of length one. For multi-variable objects (\code{dfMetadata} and 
#'  \code{mframe}), it can take any of these forms:
#' 	\itemize{
#' 		\item a logical vector representing which variables accept \code{NA}s.   
#' 			This is recycled if necessary to the total number of variables.
#' 		\item a character vector of variables names that accept \code{NA}s.
#' 		\item a named list of logical values for all or some variables, to be
#' 			interpreted by \code{\link[piUtils]{valParamList}}. An unnamed  
#' 			element acts as default value for all variables not explicitly named.
#' 			Beware the matching to variables is always done by the
#' 			list names, even if the \code{vars} argument is specified.
#' 			Variables that are not specified do not change their "NA Ok?" value.
#'  } 
#'  \code{NA}s is always interpreted as the default value, \code{TRUE}. 
#' @param \dots placeholder in the generic function to accomodate additional 
#'  arguments in methods. 
#' 
#' @return For \code{naOk}, a logical vector of the same length as
#'  \code{x}.
#'  For \code{mframe}s, when \code{all = TRUE}, returns also \code{FALSE} for 
#'  columns that are not in the metadata specification.  
#' 
#'  \code{naOk<-} allows to modify all or some of the "NA Ok?" variables
#'  specification. An error is raised if an \code{mframe} variable is  
#'  changed from accepting to rejecting \code{NA}s and it already
#'  contains some (\code{NA}) value.
#' 
#' @seealso \code{\link{metadata}}, and \code{\link[base:NA]{NA}} in 
#'  package \pkg{base}.
#' @export

naOk <- function(x, ...) UseMethod("naOk");

#' @name naOkReplace
#' @nord
#' @export `naOk<-`

`naOk<-` <- function(x, ..., value) UseMethod("naOk<-");

#' Functions to get and set variable's default values.  
#' 
#' Default values of variables specifications can be defined for  
#' metadata-related objects such as those of classes \code{\link{varMetadata}}, 
#' \code{\link{dfMetadata}}, and \code{\link{mframe}}. 
#' 
#' Variables (normally stored as columns of data frames) with associated
#' metadata can have a default value to be used when new data is added
#' to the variable but a specific value is not provided. 
#' 
#' Default values can also be unevaluated (quoted) expressions or functions.
#' In that case, the value is not computed until needed. 
#' 
#' Default values that are not language expressions or funcions are always 
#' coerced to the target class using the 
#' \link[varCoerce]{user-specified coercion} method or the default one 
#' implied by the \link[varType]{variable type}.
#' 
#' \code{NA} default values can be set even if missing values are 
#' \link[naOk]{not allowed} for the variable, but an error will be raised 
#' if the default has to be used.
#' 
#' \code{varDefault} is an \link[base:UseMethod]{S3 generic} accessor 
#' function, and \code{varDefault<-} is an S3 generic replacement function. 
#' The default methods get and set the default values of variables in an 
#' object's metadata.
#'
#' @title Default Values of Variables
#' @aliases varDefault varDefault<-
#'  varDefault.varMetadata varDefault<-.varMetadata 
#'  varDefault.dfMetadata varDefault<-.dfMetadata
#'  varDefault.mframe varDefault<-.mframe  
#' @usage  
#' 	varDefault(x, \dots)
#' 	varDefault(x, \dots) <- value
#'
#' 	\method{varDefault}{varMetadata}(x, eval = FALSE)
#' 	\method{varDefault}{varMetadata}(x) <- value
#'  
#' 	\method{varDefault}{dfMetadata}(x, vars, eval = FALSE)
#' 	\method{varDefault}{dfMetadata}(x, vars) <- value
#' 
#' 	\method{varDefault}{dfMetadata}(x, vars, eval = FALSE)
#' 	\method{varDefault}{dfMetadata}(x, vars) <- value
#'  
#' @param x an \R object.
#' @param vars subset of variables for which to get or set the default values. 
#'  This can be any standard list subsetting index (character, integer, 
#'  or logical vector). An error is raised whenever this index does not point
#'  to a valid variable of \code{x}.
#' @param eval logical flag; if \code{TRUE} and the default value is a 
#' 	function or expression, it is called or evaluated.
#' @param value specification of default variable(s) value(s).
#'  For individual variables (\code{varMetadata} objects), this must be
#'  a value, an expression, or a function that will be evaluated when the data
#'  is added. Expressions must be wrapped in a 
#'  \code{\link[base:substitute]{quote}} call.
#' 
#'  For objects with several variables (\code{dfMetadata} or \code{mframe}s),
#'  the specification has to be a vector of defaults for each variable (if
#'  they're all of the same type), or a list of defaults. The list can
#'  provide a partial specification to be interpreted by 
#'  \code{\link[piUtils]{valParamList}}. In this case, variables that are
#'  not supplied do not have their default value modified.
#' @param \dots placeholder in the generic function to accomodate new arguments
#'  in methods.
#' 
#' @return For \code{varDefault}, a single default value if applied to 
#'  \code{varMetadata} or a single element of \code{varMetadata} or 
#'  \code{mframe}. When multiple variables are requested, a named list of 
#'  default values for each variable.
#' 
#'  \code{varDefault<-} modifies all or some of the variables default values. 
#' 
#' @seealso \code{\link{metadata}}.
#' @export
 
varDefault <- function(x, ...) UseMethod("varDefault");

#' @name varDefaultReplace
#' @nord
#' @export `varDefault<-`

`varDefault<-` <- function(x, ..., value) UseMethod("varDefault<-");

#' Functions to get and set variable's user-defined metadata.  
#' 
#' Variables (normally stored as columns of data frames) with associated
#' metadata can have arbitrary data defined by the user attached to them. 
#' 
#' \code{varMeta} is an \link[base:UseMethod]{S3 generic} accessor 
#' function, and \code{varMeta<-} is an S3 generic replacement function. 
#' The default methods get and set the default values of variables in an 
#' object's metadata.
#'
#' @title User-Defined Variables' Metadata
#' @aliases varMeta varMeta<-
#'  varMeta.varMetadata varMeta<-.varMetadata 
#'  varMeta.dfMetadata varMeta<-.dfMetadata
#'  varMeta.mframe varMeta<-.mframe  
#' @usage  
#' 	varMeta(x, \dots)
#' 	varMeta(x, \dots) <- value
#'
#' 	\method{varMeta}{varMetadata}(x, i)
#' 	\method{varMeta}{varMetadata}(x, i) <- value
#'  
#' 	\method{varMeta}{dfMetadata}(x, vars, i)
#' 	\method{varMeta}{dfMetadata}(x, vars, i) <- value
#' 
#' 	\method{varMeta}{dfMetadata}(x, vars, i)
#' 	\method{varMeta}{dfMetadata}(x, vars, i) <- value
#'  
#' @param x an \R object.
#' @param vars subset of variables for which to get or set the data. 
#'  This can be any standard list subsetting index (character, integer, 
#'  or logical vector). An error is raised whenever this index does not point
#'  to a valid variable of \code{x} (!!)
#' @param i subset of user-defined elements for which to get or set the
#' 	data. Can be any standard list subsetting index (character, integer, 
#'  or logical vector). 
#' @param value any data.
#' 
#' @param \dots placeholder in the generic function to accomodate new arguments
#'  in methods.
#' 
#' @return For \code{varMeta}, a list of lists.
#' 
#'  \code{varMeta<-} modifies all or some of the variables user-defined
#'  metadata. 
#' 
#' @seealso \code{\link{metadata}}.
#' @export

varMeta <- function(x, ...) UseMethod("varMeta");

#' @name varMetaReplace
#' @nord
#' @export `varMeta<-`

`varMeta<-` <- function(x, ..., value) UseMethod("varMeta<-");

#' @nord
#' @export

as.varMetadata <- function(x, ...) UseMethod("as.varMetadata")

#' @nord
#' @export

as.dfMetadata <- function(x, ...) UseMethod("as.dfMetadata")

#' @nord
#' @export

as.mframe <- function(x, ...) UseMethod("as.mframe");

