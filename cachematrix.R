
##Sources the script.
source("cachematrix.R")

## The script has two functions - makeCacheMatrix and cacheSolve.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the square matrix has not changed), 
## then the cachesolve should retrieve the matrix inverse from the cache.



##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) 
{
     x_inv <- NULL
     set <- function(val) 
       {  
  ##Checks if the value passed is matrix       
          if(class(val)=="matrix")
             {
             x <<-val
             x_inv <<- NULL
             }
          else
          print("Input is not a matrix")
       }
  ##Returns the matrix that was set and stored in object "x" above.
    get <- function() x
  
  ## Sets the inverse once it is computed in the function below and is stored in  variable"x_inv".  
    setinverse <- function(inv) 
         {x_inv <<- inv}
 
  ## Returns the inverse stored in "x_inv" variable.
    getinverse <- function() x_inv
 
    list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)

}


## The cacheSolve function calculates the inverse of the special "matrix" created with the above function. 
##Firstly, it checks to see if the inverse has already been calculated. 
##If it is already computed, the function gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the "data" and sets the value of the inverse in the cache via the 
##setinverse function.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinverse()
  
  ## If the inverse is not null and already computed, then it returns cached data that was set previously.
  if(!is.null(x_inv)) 
    {
    message("getting cached data")
    return(x_inv)
    }  
  #Gets the matrix into the "data" object.
  data <- x$get()
  #The function "solve" computes the inverse. The value is stored in "x_inv".
  x_inv <- solve(data, ...)
  #The computed inversed is then set/stored in the cache through "setinverse" function for future retrieval.
  x$setinverse(x_inv)
  #The inverse is then returned and displayed in the console.
  x_inv
}



## Sample Execution is shown below.

##A square matrix "j" is created, and the function makeCacheMatrix is called.
##The function returns a list("a") which contains function within it to set and get values.Then function 
##cacheSolve is called by passing "a" as a parameter. The function returns the inverse of matrix "j".


##Creating a square matrix "j".
j <- matrix(1:4,2,2 )
j
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4



##makeCacheMatrix is called, the value list is retuned in "a".
a <- makeCacheMatrix()

## Sets the value of matrix
a$set(j)

## Gets the value of matriz
a$get()  
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4


##The inverse of matrix is returned when cacheSolve is called.
cacheSolve(a) 
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5


##If cacheSolve is called again for same value of "j", then it returns data from the cache.
cacheSolve(a) 
##getting cached data 
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5




