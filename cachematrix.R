## Find the Inverse of a matrix and uses stored value if previously calculated. If not, 
## calculated and then stores the value for later use.


## Creates a variable m in the parent environment and then puts the inverse of the 
## matrix m into setInverse. 
## This is then saved to the parent environment so if called can be found without having 
## to recalculate the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                         #set the matrix, m to NULL. This creates m  
                                      # so it will not crash on first run through.
    set <- function(y){               
        x <<- y                     # assign variables to the parent environment
        m <<- NULL                  # set m in the parent environment to null
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve(m)     ## creat a function, setInverse
                                                    ## which sets tthe value of solve(m)
                                                    ## in the parent environment
    getInverse <- function() m
    list(set=set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## This function will check in the parent directory to see if the matrix inverse has
## previously been calculated. If it has it returns that the inverse of the matrix. 
## If not previously calculated, the inverse is calculated and then this is stored 
## in the parent environment for the next time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    if(!is.null(m){                         ## If m is not null, ie contains a value,
        message("getting cached data")      ## return a message saying getting the 
                                            ## previously caluclated value,
        return(m)                           ## and return the previously calculated value.
    }    
    
    data <- x$get()                 ## otherwise calculate the inverse of the matrix
    m <- solve(data)                ## and put into setInverse, i.e in the parent
    x$setInverse(m)                 ##environment for the next time the value is required.
    m                               ## return m, the inverse of the inital matrix
    
}
