## This R code is a solution to assignment 2 of the Johns Hopkins University
## R Programming course. The aim of the code to is to allow the inverse of a
## matrix to be cached and so only calculated once.

## There are two functions:

##      1. makeCacheMatrix: This function creates a special "matrix" object
##         that can cache the matrix and its inverse.

##      2. cacheSolve: This function computes the inverse of the special 
##         "matrix" returned by makeCacheMatrix above. If the inverse has
##         already been calculated (and the matrix has not changed), then
##         cachesolve will retrieve the inverse from the cache, otherwise
##         it calculates the inverse using solve().

##      NOTE: the code assumes that the matrix is square and has an inverse.

makeCacheMatrix <- function(x = matrix()) {

        ## makeCacheMatrix takes as a matrix object as its input.  The function
        ## returns a list containing four functions as follows: 
        ##      a) set - allows the value of the matrix to be set
        ##      b) get - returns the matrix 
        ##      c) getinverse - returns the cached inverse of the matrix.
        ##         A NULL value is retruned if inverse not yet caclulated
        ##      d) setinverse - sets the value of the cached inverse of x        
                
        ## Create var to store cached inverse of matrix initially set to NULL
        inv <- NULL   
        
        # Craete set function to store new matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        #Create get function to return matrix
        get <- function() x
        
        #Function to set the variable
        setinverse <- function(inverse) inv <<- inverse
        
        #Function to return the variable
        getinverse <- function() inv
        
        #Return a list containing the four functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        

}


cacheSolve <- function(x, ...) {
        
        ##  cacheSolve return a matrix that is the inverse of 'x'
        ##  If the inverse of 'x' has already been calculated the inverse
        ##  is retrieved from the cache, 
        ##  otherwise it is calculated using the SOlve function.  

        ## Retrieve the cached value of the inverse for 'x'
        inv <- x$getinverse()  
        
        ## If the cached value is not NULL return it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## If the cached value is NULL, retrieve 'x' & calculate the inverse
        m <- x$get() 
        
        inv <- solve(m)  
        
        ## Pass the inverse of 'x' to the cache and then return inverse
        x$setinverse(inv) 
        
        inv  
}
