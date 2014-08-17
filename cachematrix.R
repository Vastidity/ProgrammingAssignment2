#makeCacheMatrix takes a matrix and creates a list of metadata and methods
#cacheSolve alters that list to include inverse of that matrix so it only needs
#to be computed once.

makeCacheMatrix <- function(x = matrix()) {
        #x is a matrix
        #returns a list containing the matrix and associated functions to
        #get matrix, set new matrix, set inverse, and get inverse if cached
        
        inverse <- NULL
        #allows user to set a new matrix. This seems dumb.
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        
        #allows user to get matrix
        get <- function(){
                x
        }
        
        #allows user to set new inverse. This also seems dumb
        setInverse <-function(newInverse){
                inverse <<- newInverse
        }
        
        #allows user to get inverse
        getInverse <- function(){
                inverse
        }
        
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #x is a matrix fed through makeCacheVector function
        inverse <- x$getInverse()
        
        #check if inverse has already been computed
        if(!is.null(inverse)){
                message("getting cached data...")
                return(inverse)
        }
        
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse
        
}
