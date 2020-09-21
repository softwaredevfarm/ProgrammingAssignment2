
# makeCacheMatrix Object creates a list of functions to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = numeric()) {
  i <- NULL

  # this can assign a new matrix without creating new Obj
  set <- function(y) {
    x <<- y # new matrix is assigned
    i <<- NULL # inverse variable is assigned NULL here as cacheSolve computes it
  }
  get <- function() {x} # returns the matrix

  setInverse <- function(inverse) i <<- inverse  # inverse is assigned in parent env
  getInverse <- function() i # if its null then cacheSolve computes inverse
  list(set = set, get = get, # name value pairs with each name having
       setInverse = setInverse, # function as its value and so it can be
       getInverse = getInverse) # called using $ operator like Obj$getInverse
}

cacheSolve <- function(x, ...){
  i <- x$getInverse() #calls function from object's parent env
  if(!is.null(i)){ #checks to see if i is NULL or not
    message("getting cached data") # if inverse was computed earlier then
    return(i) # it wont compute it again as got stored in parent env and returns it

  }

  # if inverse for the given matrix is not computed yet
  data <- x$get() # matrix(data) is obtained from parent env by calling get() from obj x
  i <- solve(data) # inverse of matrix is calculated using solve and assigned to i
  x$setInverse(i) #setInverse function is called from obj x so iverse i is
                  # assigned to variable i in parent env
  i
}
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)
