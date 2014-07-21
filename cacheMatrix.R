## Author: Arora_G
## Date: 07.21.2014
## Purpose: The purpose of this exercise is to demonstrate caching of time 
## consuming computations. Matrix inversion is computationally expensive.
## This exercise will demonstrate the benefits of caching the inverse of a 
## rather than calculating it repeatedly
## Algorithm: 1) Create a matrix-remember to use the function matrix
##               Remember that the matrix supplied is "ALWAYS" invertible
##               Use a Square matrix e.g. x <- matrix(1:4,nrow=2)
##            2) Function creates the special "matrix" 
##            3) Return the matrix
##            4) Second function will calculate the inverse of a matrix
##            5) Function will return the inverse of the matrix

## makeCacheMatrix will create the matrix which is a list of commands that 
## 1) Set the value of the matrix
## 2) Get the value of the matrix
## 3) Set the value of the inverse 
## 4) Get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
          # inv will store the cached inverse matrix using the operator <<-

          inv <<- NULL
          
          ## 1) Sets the value of the matrix
          
          set <- function(y) {
                 x <<- y
                 inv <<- NULL
                 }
           
           ## 2) Gets the value of the matrix
           
           get <- function()x
           
            ## 3) Sets the value of the inverse and sends it to cache
            
            setinv <- function(inverse)inv <<- inverse
            
            ## 4) Gets the value of the inverse
            
            getinv <- function()inv
            
            ## Returns the matrix after evaluation from our functions
            
            list(set=set, get=get,setinv=setinv,getinv=getinv)
                      

}


## cacheSolve function will calculate the inverse of the special "matrix".
## The function will first check if the inverse is already calculated and
## stored in the cache. If inverse is already calculated, it will get it from
## the cache. If not, it will calculate the inverse and sets the value of the
## inverse via the setinv function



cacheSolve <- function(x, ...) {

        inv <- x$getinv() 
        
        ## Checking to see if inverse is already calculated
        if(!is.null(inv)){
             message ("getting cahed data")
             return(inv)
           }
        
        ## If inverse is not calculated, the function calculates it using the solve function
        
        data <- x$get()
        inv <- solve(data,...)
        
        ## Sets the value of the inverse and sends it to cache
        x$setinv(inv)
        
        ## Returns inv
        inv
           
           
}
