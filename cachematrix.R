## These two functions collectively enable us to store the inverse of a matrix in a special object so
## that, if we repeatedly need to access the inverse then we can look it up rather than calculate it
## each time.  It uses the 'global environment' assignment operator <<-

## makeCacheMatrix takes a matrix as an argument and returns an list object that holds 3 elements:
## (i) a function that returns the vaue of the matrix
## (ii) a function that assigns the inverse of the matrix to a 'global' variable m
## (iii) a function that returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL

        get <- function() x

        setinv <- function(inv) m <<- inv
        
        getinv <- function() m
        
        list(get = get, setinv = setinv, getinv = getinv)
}


## This function returns the inverse of a matrix that is held as a makeCacheMatrix object, as defined
## above.  This is done by first checking whether the $getinv element of the object is empty - if it
## is then this signals that this is the first time the inverse of this matrix has been calculated. 
## In this case the inverse is calculated using the solve() funciton and the result stored in the global
## variable m and linked to $getinv.
## But if $getinv is not empty then this indicates the inverse has already been calculated and therefore
## this is returned by simply returning $getinv

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinv()

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m        
}
