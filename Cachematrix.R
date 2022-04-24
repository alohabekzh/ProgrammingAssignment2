makeCacheMatrix <- function(p = matrix()) {
a<-NULL
  set<-function(z){
  p<<-z
  a<<-NULL
}
get<-function() p
setI<-function(solve) a<<- solve #calculate the inverse matrix
getI<-function() a
list(set=set, get=get,
   setI=setI,
   getI=getI)
}

cacheSolve <- function(p, ...) {
     a<-p$getI()
    if(!is.null(a)){
      message("getting cached inv matrix")
      return(a)
    }
    m<-p$get()
    a<-solve(m, ...)
    p$setI(a)
    a
}
