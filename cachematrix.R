## This programming assignment will create an R function is able to cache 
##potentially time-consuming computations. This function will create and cache the inverse
##of a mtrix


## Getter and Setter functions for the input matrix and the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    ##Initialize inverse matrix to NULL (parent environment)
    inv_matrix <- NULL
    
    ##Setter function sets the input martix, y, to variable x.
    set <- function(y) {
        x <<- y 
        ## Reset the inv_matrix in parent environment to null
        inv_matrix <<- NULL 
    }
    ##Getter function to return input x
    get <- function() { x }
    
    ##Setter function to set the inverse matrix, inv_matrix
    setinverse <- function(inverse) { inv_matrix <<- inverse } 
    
    ##Getter function to return the invers matrix, inv_matrix
    getinverse <- function() inv_matrix
    
    ##List function showing available functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function take the input matrix and returns the inverse of that matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    #Call getInverse function and assign to inv_matrix
    inv_matrix <- x$getinverse()
    
    ## Return inverse if it already exists
    if(!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    ##Getter function to return input matrix, input_matrix
    input_matrix <- x$get()
    ## Call the solve() function to return the inverse of input_matrix
    inv_matrix <- solve(input_matrix, ...)
    ##Setter function to set the inverse matrix
    x$setinverse(inv_matrix)
    ##Return inverse matrix
    inv_matrix
}
