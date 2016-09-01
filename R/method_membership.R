#' Method for membership classes
#'
#' @param x an object
#' @param object an object
#' @export
#' @docType methods
#' @rdname membership-methods
#' @aliases is.na,membership-method
setMethod("is.na","membership",function(x) FALSE)

#' @export
#' @rdname membership-methods
#' @aliases show,membership-method
setMethod("show","membership",function(object){
  print.membership(object)
})

#' @rdname membership-methods
#' @aliases member, membership-method
#' @export
setGeneric("member",function(x){standardGeneric("member")})

#' @rdname membership-methods
#' @aliases member,membership-method
setMethod("member","membership",function(x) x@member)

#' @rdname membership-methods
#' @export
setGeneric("hard.label",function(x){standardGeneric("hard.label")})

#' @rdname membership-methods
#' @aliases hard.label,membership-method
setMethod("hard.label","membership",function(x) x@hard.label)
