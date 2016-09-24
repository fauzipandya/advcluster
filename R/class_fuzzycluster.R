#' Fuzzy Result
#' @import methods
#' @name fuzzycluster-class
#' @rdname fuzzycluster-class
#' @slot centroid centroid matrix
#' @slot distance distance matrix
#' @slot func.obj function objective
#' @slot call.func called function
#' @slot fuzzyfier fuzzyness parameter
#' @slot method.fuzzy method of fuzzy clustering used
#' @slot member membership matrix
#' @slot hard.label hard.label
#' @exportClass fuzzycluster
#' @include  class_membership.R
setClass("fuzzycluster",
         representation= representation(centroid="matrix",
                                        distance="matrix",
                                        func.obj="numeric",
                                        call.func="character",
                                        fuzzyfier="numeric",
                                        method.fuzzy="character"),
         contains = "membership"
)
