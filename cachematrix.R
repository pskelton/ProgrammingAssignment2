## Coursera RPROG-011 FEB 2015
## Programming assignment 2 - pskelton

## The follwing functions make a special matrix which can cache the inverse of itself for future use.



## This functions specifies the fucntions of the special matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # creates empty holder for chache inverse
    minv <- NULL
    
    # function to set the value of the matrix
    # here we use the "<<-" for assignment in the global environment
    set <- function(y) {
        x    <<- y
        minv <<- NULL
    }
    
    # function to retrieve the matrix
    get <- function() {
        x
    }
    
    # function to set inverse
    setinv <- function(inv) {
        minv <<- inv
    }
    
    # fucntion to retrieve the inverse
    getinv <- function() {
        minv
    }
    
    # return a list of the functions
    list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}



## This function calculates and caches the inverse of our sepcial matrix
## If the inverse has already been calculated it will return the cached value

cacheSolve <- function(x, ...) {
    
    # try to retreive the inverse
    minv <- x$getinv()
    
    # check if the inverse is already there and return it if so
    if (!is.null(minv)) {
        message("Getting cached inverse")
        return(minv)
    }
    
    # inverse is null so needs to be calculated..
    
    # get the matrix
    mat <- x$get()
    # solve the matrix - we are assuming the matrix is always invertable for this assignment
    minv <- solve(mat, ...)
    # set the inverse in the cache
    x$setinv(minv)
    # return the inverse
    minv
    
}
