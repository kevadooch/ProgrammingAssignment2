##  makeCacheMatrix

##  This function creates a special matrix object that supports the caching of its inverse.
##
##  makeCacheMatrix exposes four functions:  
##      1.  set:        set value of matrix
##      2.  get:        get value of matrix
##      3.  setinverse: set (cache) matrix inverse  
##      4.  getinverse: get cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) 
    
}


##  cacheSolve checks if a matrix inverse has previously been calculated 

##      If inverse exists:
##          return cached result

##      If inverse does not exist: 
##          calculate inverse of matrix using solve() 
##          cache result using setinverse
##          return result

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    
}