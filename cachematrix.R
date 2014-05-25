## Overall description of what the functions do:
## (it is assumed that the matrix supplied is always inversible.)
## 1. makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above, unless the inverse has already been calculated
## (and the matrix has not changed): in this case the cacheSolve function 
## should retrieve the inverse from the cache.


## ## makeCacheMatrix creates a list containing 4 functions:
## 1. the first one to set the value of the matrix
## 2. the second one to get the value of the matrix
## 3. the third one to set the value of the inverse of the matrix
## 4. the fourth one to get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve calculates the inverse of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the data
## and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x = matrix(), ...) {
## Return a matrix that is the inverse of 'x'
        
        ## to obtaining the list of the four functions
        ## created with the makeCacheMatrix function :
        lst <- makeCacheMatrix(x)
        
        i <- lst$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- lst$get()
        i <- solve(data, ...)
        lst$setinverse(i)
        i
}
