#' Fuzzy Result List
#' @import methods
#' @name fuzzycluster_list-class
#' @rdname fuzzycluster_list-class
#' @slot pair list of fuzzy cluster object
#' @exportClass fuzzycluster_list
setClass("fuzzycluster_list",
         representation= representation(pair="list")
)