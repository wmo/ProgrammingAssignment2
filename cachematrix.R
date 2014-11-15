## Cache the calculation of the inverse of a matrix. 
##
## Initialize your matrix as follows : 
##
##     m <- makeCacheMatrix( matrix( c( 3,4,5,7 ), 2,2) ) 
##
## To get the inverse of the matrix :
## 
##     cacheSolve(m) 
##            [,1] [,2]
##      [1,]    7   -5
##      [2,]   -4    3
##
## On any subsequent calls to cacheSolve(m), the inverse will not be recalculated
## but it will be retrieved from the cache.


## The function to setup the cache and store the matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    setinv <- function(mInverse) inv <<- mInverse
    getinv <- function() inv 
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function to calculate or to retrieve the cached calculation of the inverse of the matrix. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv 
}

# ---------------------------------------------------------------------- 
# TESTCASE 
# ---------------------------------------------------------------------- 
# 1) source the code 
#   
# > source("cachematrix.R")
#
#
# ---------------------------------------------------------------------- 
# 2) define and solve two matrices (uncached yet)
# 
# > m1 <- makeCacheMatrix( matrix( c(-3,5,2,-3) ,2,2)) 
# > cacheSolve(m1) 
#      [,1] [,2]
# [1,]    3    2
# [2,]    5    3
# 
# 
# > m2 <- makeCacheMatrix( matrix( c(2,-17,11,-1,11,-7,0,3,-2) ,3,3)) 
# > cacheSolve(m2)
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    1    4    6
# [3,]    2   -3   -5
# 
#
#
# ---------------------------------------------------------------------- 
# 3) call cacheSolve again, now the message 'getting cached data' 
#    should be displayed, followed by the same inverse matrices as in 2)
# 
# > cacheSolve(m1) 
# getting cached data
#      [,1] [,2]
# [1,]    3    2
# [2,]    5    3
# 
# > cacheSolve(m2)
# getting cached data
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    1    4    6
# [3,]    2   -3   -5
