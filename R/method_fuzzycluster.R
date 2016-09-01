#' Method for fuzzycluster classes
#'
#' @param x an fuzzycluster object
#' @param object an fuzzycluster object
#' @param y an fuzzycluster object
#' @export
#' @docType methods
#' @rdname fuzzycluster-methods
#' @aliases is.na,fuzzycluster-method
setMethod("is.na","fuzzycluster",function(x) FALSE)

#' @rdname fuzzycluster-methods
#' @aliases show,fuzzycluster-method
setMethod("show","fuzzycluster",function(object){
  print.fuzzycluster(object)
})

#' @rdname fuzzycluster-methods
#' @aliases label, fuzzycluster-method
#' @exportMethod label
setGeneric("label",function(x){standardGeneric("label")})

#' @rdname fuzzycluster-methods
#' @aliases label,fuzzycluster-method
setMethod("label","fuzzycluster",function(x) x@label)

#' @rdname fuzzycluster-methods
#' @aliases centroid, fuzzycluster-method
#' @exportMethod centroid
setGeneric("centroid",function(x){standardGeneric("centroid")})

#' @rdname fuzzycluster-methods
#' @aliases centroid,fuzzycluster-method
setMethod("centroid","fuzzycluster",function(x) x@centroid)

#' @rdname fuzzycluster-methods
#' @aliases label, fuzzycluster-method
#' @exportMethod label
setGeneric("label",function(x){standardGeneric("label")})

#' @rdname fuzzycluster-methods
#' @aliases label,fuzzycluster-method
setMethod("label","fuzzycluster",function(x) x@label)


#' @rdname fuzzycluster-methods
#' @aliases distance, fuzzycluster-method
#' @exportMethod distance
setGeneric("distance",function(x){standardGeneric("distance")})

#' @rdname fuzzycluster-methods
#' @aliases distance,fuzzycluster-method
setMethod("distance","fuzzycluster",function(x) x@distance)


#' @rdname fuzzycluster-methods
#' @aliases func.obj, fuzzycluster-method
#' @exportMethod func.obj
setGeneric("func.obj",function(x){standardGeneric("func.obj")})

#' @rdname fuzzycluster-methods
#' @aliases func.obj,fuzzycluster-method
setMethod("func.obj","fuzzycluster",function(x) x@func.obj)


#' @rdname fuzzycluster-methods
#' @aliases call.func, fuzzycluster-method
#' @exportMethod call.func
setGeneric("call.func",function(x){standardGeneric("call.func")})

#' @rdname fuzzycluster-methods
#' @aliases call.func,fuzzycluster-method
setMethod("call.func","fuzzycluster",function(x) x@call.func)


#' @rdname fuzzycluster-methods
#' @aliases fuzzyfier, fuzzycluster-method
#' @exportMethod fuzzyfier
setGeneric("fuzzyfier",function(x){standardGeneric("fuzzyfier")})

#' @rdname fuzzycluster-methods
#' @aliases fuzzyfier,fuzzycluster-method
setMethod("fuzzyfier","fuzzycluster",function(x) x@fuzzyfier)


#' @rdname fuzzycluster-methods
#' @aliases method.fuzzy, fuzzycluster-method
#' @exportMethod method.fuzzy
setGeneric("method.fuzzy",function(x){standardGeneric("method.fuzzy")})

#' @rdname fuzzycluster-methods
#' @aliases method.fuzzy,fuzzycluster-method
setMethod("method.fuzzy","fuzzycluster",function(x) x@method.fuzzy)


#' @rdname fuzzycluster-methods
#' @aliases partition, fuzzycluster-method
#' @exportMethod partition
setGeneric("partition",function(x){standardGeneric("partition")})

#' @rdname fuzzycluster-methods
#' @aliases partition,fuzzycluster-method
setMethod("partition","fuzzycluster",function(x) x@partition)

#' @rdname fuzzycluster-methods
#' @aliases cPair,fuzzycluster-method
#' @exportMethod .cPair
setGeneric(".cPair",function(x,y){standardGeneric(".cPair")})

#' @rdname fuzzycluster-methods
#' @aliases cPair,fuzzycluster-method,ANY-method
setMethod(".cPair",c("fuzzycluster","fuzzycluster"),
          function(x,y) {.fuzzy.cPair(x,y)})

#' @rdname fuzzycluster-methods
#' @aliases cPair,fuzzycluster-method,ANY-method
setMethod(".cPair",c("fuzzycluster","ANY"),
          function(x,y) {.fuzzy.cPair(x,y)})

#' @rdname fuzzycluster-methods
#' @aliases cPair,fuzzycluster-method,ANY-method
setMethod(".cPair",c("ANY","fuzzycluster"),
          function(x,y) {.fuzzy.cPair(x,y)})

#' @rdname fuzzycluster-methods
#' @aliases cPair,fuzzycluster-method,ANY-method
setMethod(".cPair",c("ANY","ANY"),
          function(x,y) {.fuzzy.cPair(x,y)})