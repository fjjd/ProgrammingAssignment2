#    These two functions are used to create a special object that 
#    stores a matrix and caches its inverse. 

# This function creates the object to store the matrix and its inverse. It returns a list with
# the functions through which the object can be manipulated. set() allows to set the matrix
# associated with the object. get() allows to retrieve the matrix associated with the object. setsolve()
# allows to store the calculated inverse matrix in the object. getsolve() allows to retrieve the inverse
# matrix stored in the object (if there is one)
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    
    get <- function() x
    
    setsolve <- function (inv) im <<- inv
    
    getsolve <- function() im
    
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function
# This function calculate the inverse of the matrix stored in the object.
# If the inverse matrix has not been computed before, it uses the function solve()
# to compute the inverse and then cache it in the object. If the inverse of the 
# stored matrix has been previously computed, the function returns the previous
# calculation cached in the object
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    im <- x$getsolve()
    if(!is.null(im)) {
        message("getting cached data (inverse matrix)")
        return(im)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
}
