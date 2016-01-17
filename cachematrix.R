##These functions take an invertible matrix and caches it's inverse.
##They eliminate the need to repeatedly calculate the inverse of a matrix,
##since this is an expensive computation.

##Creates a special matrix object which can cache it's inverse.
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
        setmatrix=setmatrix,
        getmatrix=getmatrix)
}

##Computes the inverse of a special matrix created from the above function
##and caches the result. If the inverse has already been calculated, the
##cached value is retrieved with no additional calculations.
cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}