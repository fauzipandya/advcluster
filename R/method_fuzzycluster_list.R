#' Method for fuzzycluster_list classes
#'
#' @param object an fuzzycluster_list object
#' @export
#' @docType methods
#' @rdname fuzzycluster_list-methods
setGeneric("pair",function(object){standardGeneric("pair")})

#' @rdname fuzzycluster_list-methods
#' @aliases pair,fuzzycluster_list-method
setMethod("pair","fuzzycluster_list",
          function(object) {object@pair})
