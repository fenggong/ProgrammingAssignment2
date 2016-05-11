## This program shows the <<- operator which can be used to assign a value to an object in an enviornment different from the current environment. These two functions cache the inverse of an invert matrix. Matrix inversion is usually costly and there may be some benifit to cach it.

## The first function creates a special vector, which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <-function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The second function calculates the inverse of the matrix created with the above function. It first checks to see if the inverse of the matrix has already been calculated. If so, it gets it from the cache; otherwise, it calculates it and sets its value in the cache. 

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
