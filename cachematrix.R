## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Creates matrix object that can cache its inverse after it has been calculated once
#Returns list of member functions to set and get the matrix and set and get the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        set.inv <- function(inverse) inv <<- inverse
        
        get.inv <- function() inv
        
        list(set = set, get = get,
             set.inv = set.inv,
             get.inv= get.inv)

}


## Write a short comment describing this function

#Returns the inverse of a matrix, cached inverse is returned if it exists
#if inverse is not already calculated it is cached and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get.inv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set.inv(inv)
        inv
}
