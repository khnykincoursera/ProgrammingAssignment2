## Put comments here that give an overall description of what your
## functions do

## The function accepts a matrix and initializes internal variables
makeCacheMatrix <- function(x = matrix()) {
        inversion <- NULL
        set <- function(y) {
                x <<- y
                inversion <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inversion <<- solve
        getsolve <- function() inversion
        list(set = set,
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The function returns inverse of a matrix
## Any other parameters to solve function  except for the matrix itself are ignored
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## do we have a result already?
        inversion <- x$getsolve()
        if(!is.null(inversion)) {
		## cache hit, then take cached result and return it
                return(inversion)
        }

        ## cache miss, then retrieve data and cache it
        data <- x$get()
        ## to find inverse of a matrix we do not need other parameters (and we did not remember them)
        inversion <- solve(data)
        x$setsolve(inversion)
        inversion
}
