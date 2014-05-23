## This function allows you to give an input of a matrix using
## the set function within makeCacheMatrix. After the matrix is set,
## you can call makeCacheMatrix with cacheSolve, to find the inverse
## of the matrix you input. cacheSolve also caches this inverse, so
## if you try to call cacheSolve on the same input again, it will
## display the result already stored, instead of recalculating it

## This first function allows you to input a matrix using set
## It also displays the set matrix using the get function. There are
## two other functions setinv, and getinv that are called later on using
## the cacheSolve function, and they set the inverse and display the
## inverse, respectively.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {    
        x <<- y             #sets the matrix as a cached value, to be called elsewhere
        inv <<- NULL        #clears values in inv
    }
    get <- function() x                      #displays the matrix set above
    setinv <- function(solve) inv <<- solve  #stores inv as a cached value
    getinv <- function() inv                 #displays inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function finds the inverse of the matrix created in 
## makeCacheMatrix by using the solve() function, and caches that
## inverse to be called later. So if this function is ran again later, 
## instead of calculating the inverse again, it displays the cached value

cacheSolve <- function(x, ...) {
    inv <- x$getinv()       #calls getinv above to retrieve the inv value from prior function
    if(!is.null(inv)) {     #if inv is not null, it displays a message and the inv value
        message("getting cached data")
        return(inv)
    }
    data <- x$get()         #if inv was null, this pulls in the matrix created in the prior function
    inv <- solve(data, ...) #finds the inverse of the matrix and stores it as inv
    x$setinv(inv)           #caches the value inv
    inv                     #displays inv, which is the inverse of the matrix 'x'
}   

