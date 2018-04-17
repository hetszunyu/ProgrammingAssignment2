## If I understand it correctly, this assignment can be done with
## slight modification of the original vector example in the assignment
##description.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL  ##sets m also in parent environment (note to self)
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get, setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(x, ...) {
       m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}
