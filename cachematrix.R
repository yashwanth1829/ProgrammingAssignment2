## The first function, makeVector creates a special matrix, which is really a list containing a function to
##set the matrix
##get the matrix
##set the Inverse matrix
##get the Inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        
        #set the value of the Matrix
        setMatrix <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        
        getMatrix <- function() x                              #get the value of the Matrix
        setInverse <- function(inverse) invMatrix <<- inverse  #set the value of the invertible matrix
        getInverse <- function() invMatrix                     #get the value of the invertible matrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
}


## The following function calculates the inverse of matrix using matrix created with the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {                       #if inverse matrix is not NULL
                message("Getting Cached Invertible Matrix")   #Type message: Getting Cached Invertible Matrix 
                return(invMatrix)                             #return the invertible matrix
        }
        
        #if value of the invertible matrix is NULL then  
        MatrixData <- x$getMatrix()                     #get the original Matrix Data 
        invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
        x$setInverse(invMatrix)                         #set the invertible matrix 
        return(invMatrix)                               #return the invertible matrix
}
