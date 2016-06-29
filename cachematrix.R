## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # create an empty matrix
    inverse_matrix <- NULL
    
    # create a function that stores the matrix and the inverse matrix
    setvars <- function(y){
        x <<- y
        inverse_matrix <<- NULL
    }
    
    # create a function to call matrix x
    getmatrix <- function() {
        x
    }
    
    # call the solve function to solve the inverse of the matrix and store this value to inverse_matrix
    setsolve <- function(solve) {
        inverse_matrix <<- solve
    }
    
    # create a function to call the inverse matrix
    getinverse <- function() {
        inverse_matrix
    }
    
    # returns list of functions listed above
    list(setvars = setvars, getmatrix = getmatrix, setsolve = setsolve, getinverse = getinverse)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## x = list of functions from makeCacheMatrix
    
    #obtain inverse matrix returned by getinverse function from list x; is NULL if none found
    inverse_matrix <- x$getinverse() 
    
    if (!is.null(inverse_matrix)) {
        message("getting cached inverse matrix")
        return(inverse_matrix) #returns stored inverse matrix
    }
    
    # obtains matrix returned by getmatrix function from list x
    matrix_x <- x$getmatrix() 
    
    # calculate inverse of matrix x
    inverse_matrix <- solve(matrix_x, ...) 
    
    # store or call the stored inverse matrix
    x$setsolve(inverse_matrix) 
    
    # returns inverse of the matrix
    inverse_matrix 
}
