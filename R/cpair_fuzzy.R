#' Pairing fuzzy cluster
#' @param x an fuzzycluster object
#' @param y an fuzzycluster object
fuzzy.cPair<-function(x,y){
  if(!is(x,"fuzzycluster")||!is(y,"fuzzycluster"))
    if(!is(x,"fuzzycluster_list"))
      if(!is(y,"fuzzycluster_list"))
        stop("Both x and y must be fuzzy cluster result")
  if(is(x,"fuzzycluster_list") && is(y,"fuzzycluster_list"))
    result<-new("fuzzycluster_list",pair=append(pair(x),pair(y)))
  else if(is(x,"fuzzycluster_list"))
    result<-new("fuzzycluster_list",pair=append(pair(x),y))
  else if(is(y,"fuzzycluster_list"))
    result<-new("fuzzycluster_list",pair=append(x,pair(y)))
  else
    result<-new("fuzzycluster_list",pair=append(x,y))
}
