## Exercise 2 solution
#
#  creates a special memoize type, "CacheMatrix",
# implemented as a list of four functions
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the matrix inverse
# 4.  get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
 minv <- NULL
 set <-function(y){
     x <<- y
     minv <<- NULL
 }
 get <- function() x
 setminv <- function(minv) m <<- minv
 getminv <- function() minv
 list(set = set, get = get,
      setminv= setminv,
      getminv = getminv)

 
}


## following function calculates the inverse of a cacheMatrix
## using the cached value if if exists

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getminv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setminv(m)
    m
}


