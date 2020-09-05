#' @title  medianclass
#'
#' @description Calculates median with data in classes
#'
#' @param freq
#' @param brackets
#' @return median
#' @examples :
#' brackets=c(0,5,10,15,20,25,30,35,40,45)
#' freq=c(2,2,7,5,4,5,2,1,4)
#' median_class(freq = freq , brackets = brackets)
#'
#' @export
median_class<- function(freq, brackets){
  if(length(brackets)!=(1+length(freq))) {
    cat("length of brackets incoherent to freq in classes ")
    return(NULL)
  }
  n=sum(freq)
  if(n%%2==0)   idx=n/2
  else idx=(n+1)/2
  cums=cumsum(freq)
  class=which(idx<cums)[1]
  scale=(cums[class]-idx)/(brackets[class+1]-brackets[class])
  median=brackets[class]+ (brackets[class]-brackets[class+1])*scale
  return(median)
}


