#' Combine fuzzy cluster result
#' @description combining fuzzy cluster result before ensembling
#' @param x a fuzzy cluster object
#' @param ... a fuzzy cluster object
#' @export
"c_fuzzycluster"<-function(x,...){
  if(nargs()<3) .cPair(x,...)
  else .cPair(x,Recall(...))
}