## Pair of functions to cache the inverse of a matrix

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        set <- function(y){
                x<<-y
                m<<-NULL
        }
        get <- function() x
        setMatrix<-function(solve) m<<-solve
        getMatrix<-function() m
        list(set=set, get=get, setMatrix=setMatrix, getMatrix=getMatrix)

}


## Computes inverse of the special "matrix" returned by makeCacheMatrix above. 
## If inverse already calculated,  retrieve inverse from cache.

cacheSolve <- function(x=matrix(), ...) {
        m <- x$getMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrix(m)
        m
}