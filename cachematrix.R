## R-programming Assignment -2 :
## Two functions which cache the inverse of a matrix



############################################################################
## makeCacheMatrix
############################################################################

## Aim of this function is to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function( x = matrix() ) {
  
        ## Inverse property innitialization
        inv <- NULL
        
  
        ## Method for setting the matrix
        set <- function( matrix ) {
                x <<- matrix
                inv <<- NULL
        }
        
  
        ## Method for getting the matrix
        get <- function() {
                ## Returning the matrix
                x
        }
        
  
        ## Method for setting the inverse of the matrix
        set-inve <- function(inverse) {
                inv <<- inverse
        }
        
  
        ## Method for getting the inverse of the matrix
        get-inv <- function() {
                ## Return the inverse property
                inv
        }
        
  
        ## Returning a list of all the methods
        list(set = set, get = get,
                set-inv = set-inv,
                get-inv = get-inv)
}





############################################################################
## cacheSolve
############################################################################

## Aim of this function is to compute the inverse of the special matrix returned by "makeCacheMatrix"
## defined above. 
## If some how the inverse being already calculated and the matrix has not
## changed, this function will retrieve values from the cache.

cacheSolve <- function(x, ...) {
  
        ## Returning a matrix that is the inverse of 'x'
        x <- x$get-inv()
        
  
        ## Check and if exsist, return the inverse if its already set
        if( !is.null(x) ) {
                message("DAta is exsist: getting data from cached")
                return(x)
        }
        
  
        ## Getting the matrix from the object
        data <- x$get()
        
  
        ## Calculating the inverse using mrthod of matrix multiplication
        inv <- solve(data) %*% data
        
  
        ## Setting the inverse to the object
        x$set-inv(inv)
        
  
        ## Returning the matrix
        inv
}