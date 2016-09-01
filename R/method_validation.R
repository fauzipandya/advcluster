#' Method for validation classes
#'
#' @param x an object
#' @param object an object
#' @export
#' @docType methods
#' @rdname validation-methods
#' @aliases show,validation-method
setMethod("show","validation",function(object){
  print.validation(object)
})

#' @rdname validation-methods
#' @aliases PC, validation-method
#' @export
setGeneric("PC",function(x){standardGeneric("PC")})

#' @rdname validation-methods
#' @aliases PC,validation-method
setMethod("PC","validation",function(x) x@PC)

#' @rdname validation-methods
#' @aliases MPC, validation-method
#' @export
setGeneric("MPC",function(x){standardGeneric("MPC")})

#' @rdname validation-methods
#' @aliases MPC,validation-method
setMethod("MPC","validation",function(x) x@MPC)


#' @rdname validation-methods
#' @aliases CE, validation-method
#' @export
setGeneric("CE",function(x){standardGeneric("CE")})

#' @rdname validation-methods
#' @aliases CE,validation-method
setMethod("CE","validation",function(x) x@CE)


#' @rdname validation-methods
#' @aliases XB, validation-method
#' @export
setGeneric("XB",function(x){standardGeneric("XB")})

#' @rdname validation-methods
#' @aliases XB,validation-method
setMethod("XB","validation",function(x) x@XB)


#' @rdname validation-methods
#' @aliases S, validation-method
#' @export
setGeneric("S",function(x){standardGeneric("S")})

#' @rdname validation-methods
#' @aliases S,validation-method
setMethod("S","validation",function(x) x@S)

#' @rdname validation-methods
#' @aliases Tang, validation-method
#' @export
setGeneric("Tang",function(x){standardGeneric("Tang")})

#' @rdname validation-methods
#' @aliases Tang,validation-method
setMethod("Tang","validation",function(x) x@Tang)

#' @rdname validation-methods
#' @aliases Kwon, validation-method
#' @export
setGeneric("Kwon",function(x){standardGeneric("Kwon")})

#' @rdname validation-methods
#' @aliases Kwon,validation-method
setMethod("Kwon","validation",function(x) x@Kwon)


