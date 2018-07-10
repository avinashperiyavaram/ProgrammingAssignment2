makeCacheMatrix <- function(x=matrix()){
                                        inverse1 <- NULL
                                        setA <- function(y) {
                                                                x <<- y
                                                                inverse1 <- NULL
                                        }
                                        getA <- function() x
                                        setAinverse1 <- function(inverse) inverse1 <<- inverse
                                        getAinverse1 <- function() inverse1
                                        list( setA = setA, getA = getA, setAinverse1 = setAinverse1, getAinverse1 = getAinverse1)
}

cacheSolve <- function(x, ...) {
                                inverse1 <- x$getAinverse1()
                                if(!is.null(inverse1)){
                                        print("Cache on the way")
                                        return(inverse1)
                                }
                                data <- x$getA()
                                inverse1 <- solve(data, ...)
                                x$setAinverse1(inverse1)
                                inverse1
}
