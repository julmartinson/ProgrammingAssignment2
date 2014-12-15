## makeCacheMatrix function creates container to store matrix and its inverse
## cacheSolve function uses makeCacheMatrix to cache inverse of a matrix

##
# makeCacheMatrix function acts as a container that stores 2 matrices: matrix1 and matrix2 - inverse of matrix1
# matrix1 is passed as argument or stored in the container using set method. 
# get - returns matrix1
# set - sets matrix member to matrix1
# getinverse - returns matrix2
# setinverse - sets matrix2 to matrix argument
makeCacheMatrix <- function(x = matrix()) {
# x - matrix argument or matrix member if set
# ix - matrix member to store inverse
        ix <- NULL
        set <- function(m) {
		# sets x and ix values in parent environment
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
# - gets original matrix and calculates its inverse using solve() function
# - sets inverse matrix member to calculated value
# - returns inverse matrix
cacheSolve <- function(x, ...) {
# retrieve inverse
        i <- x$getinverse()
# check if inverse had been set (not null)		
        if(!is.null(i)) {
		# yes - return existing (cached) inverse		
                message("getting cached inverse")
                return(i)
        }
		# no - get original matrix, calculate inverse using solve(), store in cache and return
        m <- x$get()
        i <- solve(m)
        x$setinverse(i)
        i
}

