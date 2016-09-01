#' dddddd Result
#' @import methods
#' @name ddddd-class
#' @rdname ddddd-class
#' @slot centroid centroid matrix
#' @slot distance distance matrix
#' @slot func.obj function objective
#' @slot call.func called function
#' @slot fuzzyfier fuzzyness parameter
#' @slot method.fuzzy method of fuzzy clustering used
#' @exportClass ddddd
setClass("ddddd",
         representation= representation(centroid="matrix",
                                        distance="matrix",
                                        func.obj="numeric",
                                        call.func="character",
                                        fuzzyfier="numeric",
                                        method.fuzzy="character"),
         contains = "membership"
)
