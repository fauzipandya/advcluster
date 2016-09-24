#' Combine fuzzy cluster result
#' @description combining fuzzy cluster result before ensembling
#' @param x a fuzzy cluster object
#' @param ... a fuzzy cluster object
#' @return Fuzzy Clustering List
#' @slot pair pair list
#' @export
#' @examples
#' fuzzy.CM(iris[,1:4],K=2,m=2,max.iteration=20,threshold=1e-3,RandomNumber=1234)->cl1
#' fuzzy.GK(iris[,1:4],K=2,m=2,max.iteration=20,threshold=1e-3,RandomNumber=1234)->cl2
#' c_fuzzycluster(cl1,cl2)
"c_fuzzycluster"<-function(x,...){
  if(nargs()<3) .cPair(x,...)
  else .cPair(x,Recall(...))
}