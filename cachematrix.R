## This function saves a list of functions to set and to get a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL               #setting a value of a matrix's inverse being cached to zero
    set <- function(y) {    #setting a matrix and indicating that its inverse 
                            #is not stored 
        x <<- y
        m <<- NULL
    }
    get <- function() x                         #extracting a stored matrix
    setinv <- function(solve) m <<- solve       #setting an inverse of a matrix
    getinv <- function() m                      #getting a stored inverse of a matrix
    s<-list(set = set, get = get,               #setting a list of pre-defined functions
         setinv = setinv,
         getinv = getinv)
}


## This function operates with the results of the previous function and returns 
## an inverse of the matrix specified as an input for the previous function. If 
## its inverse has already been calculated, it returns the cached value.

cacheSolve <- function(x, ...) {
    m <- x$getinv()                         #extracting inversed matrix from cache
    if(!is.null(m)) {                       #checking if the extracted data is not null
        message("getting cached data")      #if it is not null, printing the message
        return(m)                           #and returning its value, finishing the function
    }
    data <- x$get()                         #otherwise, taking out the matrix
    m <- solve(data, ...)                   #computing its inverse
    x$setinv(m)                             #storing its inverse in a cache
    m                                       #returning the value of inverse
    
    }
