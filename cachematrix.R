# Function returns a list of functions which can be used to set/get data either to/from cache
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
		m <<- inv # Storing data to variable in global environment
		}
	getInv <- function(){ # Retrive from Cache
		m
		}
	list (set = set, get = get, setInv = setInv, getInv = getInv)
}

# Function returns inverse of Matrix.
# Expects matrix as input.
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
