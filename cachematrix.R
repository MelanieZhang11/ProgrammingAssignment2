#The function keeps the inverse of a matrix in its cache so that the value of the inverse doesn't have to be calculated again when it's needed in the future. 

#This function creates a "matrix" that caches the original matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) inv <<- solve
    getmatrix <- function() inv
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


#This function first checks whether there is a cached inverse in the memory. If yes, it will return the cached value; if not it will perform the calculation on the input. 

cacheSolve <- function(x, ...) {
    inv <- x$getmatrix()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setmatrix(inv)
    inv
}

