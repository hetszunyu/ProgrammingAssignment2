## If I understand it correctly, this assignment can be done with
## slight modification of the original vector example in the assignment
##description.

## For key takeaway from this homework, see comments in code
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL  ##sets m also in parent environment (note to self)
        }
        get <- function() x
        setinv <- function(inv) m <<- inv ## these <<-s ensure that m is set to inv outside the function
        getinv <- function() m
        list(set = set, get = get, setinv = setinv,
             getinv = getinv)
}

## cacheSolve is easier to understand, it basically check if there's a cached version available and then
## proceeds to calculate the inverse if there's none (and retreives the cached inverse if there is)
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
