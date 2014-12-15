## (TBD)

##
# makeCacheMatrix function acts as a container that has 
# 2 member variable - matrices, original and inverse, and methods (functions): 
# get - returns original matrix member
# set - sets original matrix member to matrix argument
# getinverse - returns inverse matrix member
# setinverse - sets inverse matrix member to matrix argument
makeCacheMatrix <- function(x = matrix()) {
        ix <- NULL
        set <- function(m) {
                x <<- m
                ix <<- NULL
        }
        get <- function() x
        setinverse <- function(im) ix <<- im
        getinverse <- function() ix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##
# cacheSolve function takes matrix container created by makeCacheMatrix function
# and checks if inverse matrix member had been set and if yes then returns its value
# if inverse matrix member had not been set function
# - calculates inverse using solve() function
# - sets inverse matrix member to calculated value
# - returns inverse matrix
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        m <- x$get()
        i <- solve(m)
        x$setinverse(i)
        i
}

