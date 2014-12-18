## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to cache inverse of Matrix
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL # creating a variable m in global environment
	set <- function(y){ # This function is used to set matrix direction
		message("Setting y to x")
		# assigning value to x created in parent environment. Without super assignment it would create local variable x
		x <<- y
		m <<- NULL
		}
	get <- function() { # Get Value of x
		x
		}
	setInv <- function(inv) {# Store Inverse to cache
		m <<- inv
		}
	getInv <- function(){
		m
		}
	list (set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        message(class(data))
        m <- solve(data)
        message(m)
        x$setInv(m)
        m
}
