## Put comments here that give an overall description of what your
## functions do

#this function creates a matrix, and outputs it, as well as other functions into a list. 
#this function can be called to change the matrix, or store the inverse of the matrix.
#if the inverse of the matrix is saved, it can be retrieved. if a new matrix is generated, 
#the old inverse is deleted. cacheSolve can be called to generate a new inverse. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x<<- y
        inv <<- NULL
    }
    

    get <- function() x
    
    setinv <- function(inverse) inv <<- inverse
    
    getinv <- function() inv
    
    list(set = set, get=get, setinv=setinv, getinv=getinv)
}


# check to see if an inverse matrix already axists. if it does, retrieve it, otherwise recalculate
cacheSolve <- function(x, ...) {

    inv <- x$getinv()
    
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    
    data <- x$get()
    
    inv<- solve(data, ...)
    
    x$setinv(inv)
    inv
    
    }
