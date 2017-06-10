## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function acsepts a matrix as an input,
## has functions to set, get a value of a matrix and set , get the values of inverse of the given matrix
## pretares using those functions by handler:

makeCacheMatrix <- function(mtx = matrix()) {
     invt <- NULL      
     set <- function(y) {           
         mtx <<- y          
         invt <<- NULL      
     }      
     get <- function() mtx      
     setinverse <- function(inverse) invt <<- inverse      
     getinverse <- function() invt      
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)  
} 

## Write a short comment describing this function
## this function checks if the value of inverse was calculated before
## if it is exists in cache it prints a message and returns the value
## if it wasn't calculated before it gets new matrix, calculates inverse, sets cache and returns new value:

cacheSolve <- function(hndlr, ...) {      
     invt <- hndlr$getinverse()      
     if(!is.null(invt)) {         
        message("getting cached data.")          
        return(invt)      
     }      
     mtxdata <- hndlr$get()      
     invt <- solve(mtxdata)           
     hndlr$setinverse(invt)      
     invt  
}
        ## Return a matrix that is the inverse of 'mtx'
# Test:
mtx <- matrix(rnorm(9),3,3)
hndlr = makeCacheMatrix(mtx)
hndlr$get()
            [,1]       [,2]       [,3]
[1,] -0.99551870  0.7566935  0.3336327
[2,] -0.00306546 -0.2810057 -0.4188962
[3,]  0.75983718  0.1439443  0.9301513
> 
> 
> cacheSolve(hndlr)
          [,1]      [,2]       [,3]
[1,] -6.172656 -20.13188  -6.852400
[2,] -9.683269 -36.20738 -12.832838
[3,]  6.540941  22.04888   8.658722
> 
> cacheSolve(hndlr)
getting cached data.
          [,1]      [,2]       [,3]
[1,] -6.172656 -20.13188  -6.852400
[2,] -9.683269 -36.20738 -12.832838
[3,]  6.540941  22.04888   8.658722
