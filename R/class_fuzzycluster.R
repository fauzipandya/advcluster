#' Fuzzy Result
#' @import methods
#' @name fuzzycluster-class
#' @rdname fuzzycluster-class
#' @slot partition membership matrix
#' @slot label vector of hard partition
#' @slot centroid centroid matrix
#' @slot distance distance matrix
#' @slot func.obj function objective
#' @slot call.func called function
#' @slot fuzzyfier fuzzyness parameter
#' @slot method.fuzzy method of fuzzy clustering used
#' @exportClass fuzzycluster
setClass("fuzzycluster",
         representation= representation(partition="matrix",
                                        label="vector",
                                        centroid="matrix",
                                        distance="matrix",
                                        func.obj="numeric",
                                        call.func="character",
                                        fuzzyfier="numeric",
                                        method.fuzzy="character")
)
