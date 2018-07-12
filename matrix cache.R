##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.
# makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x=matrix()){
                                        inverse1 <- NULL 
                                        setA <- function(y) {
                                                                x <<- y
                                                                inverse1 <- NULL
                                        } #Creation of a set 
                                        getA <- function() x #Creation of return object
                                        setAinverse1 <- function(inverse) inverse1 <<- inverse
                                        getAinverse1 <- function() inverse1
                                        list( setA = setA, getA = getA, setAinverse1 = setAinverse1, getAinverse1 = getAinverse1)
}
#cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
                                inverse1 <- x$getAinverse1() #Requesting to return a matrix that is inverse of "x"
                                if(!is.null(inverse1)){
                                        print("Cache on the way")
                                        return(inverse1)
                                }
                                data <- x$getA()
                                inverse1 <- solve(data, ...)
                                x$setAinverse1(inverse1)
                                inverse1
}
#End 
