#' Consensus Fuzzy Cluster  Result
#' @import methods
#' @name co_fuzzycluster-class
#' @rdname co_fuzzycluster-class
#' @slot partition.ensemble membership matrix
#' @slot label.ensemble vector of hard partition
#' @slot method.consensus method of fuzzy clustering used
#' @exportClass co_fuzzycluster
setClass("co_fuzzycluster",
         representation= representation(partition.ensemble="matrix",
                                        label.ensemble="vector",
                                        method.consensus="character")
)
