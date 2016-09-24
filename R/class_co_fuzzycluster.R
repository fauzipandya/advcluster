#' Consensus Fuzzy Cluster  Result
#' @import methods
#' @name co_fuzzycluster-class
#' @rdname co_fuzzycluster-class
#' @slot member membership matrix
#' @slot hard.label vector of hard partition
#' @slot method.consensus method of fuzzy clustering used
#' @exportClass co_fuzzycluster
#' @include class_membership.R
setClass("co_fuzzycluster",
         representation= representation(
                                        method.consensus="character"),
         contains = "membership"
)
