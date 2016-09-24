#' Method for co_fuzzycluster classes
#'
#' @param x an co_fuzzycluster object
#' @param object an co_fuzzycluster object
#' @export
#' @docType methods
#' @rdname co_fuzzycluster-methods
#' @aliases is.na,co_fuzzycluster-method
setMethod("is.na","co_fuzzycluster",function(x) FALSE)

#' @rdname co_fuzzycluster-methods
#' @aliases show,co_fuzzycluster-method
setMethod("show","co_fuzzycluster",function(object){
  print.co_fuzzycluster(object)
})

#' @rdname co_fuzzycluster-methods
#' @aliases method.consensus, co_fuzzycluster-method
#' @exportMethod method.consensus
setGeneric("method.consensus",function(x){standardGeneric("method.consensus")})

#' @rdname co_fuzzycluster-methods
#' @aliases method.consensus,co_fuzzycluster-method
setMethod("method.consensus","co_fuzzycluster",function(x) x@method.consensus)

