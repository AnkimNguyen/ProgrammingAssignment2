## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix creates a special matrix, which is a list 
# containing a function to
#1. Set value of Matrix
#2. Get value of Matrix
#3. Set value of inverse
#4. Get value of the inverse
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<- function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<- function(inv) m <<-inv#Cache Results
  getinverse<- function() m #Return Result 
  list(set=set, get=get,
      setinverse= setinverse,
      getinverse=getinverse)
}


## Write a short comment describing this function
#The following function calculate the mean of the matrix
#created with the above function.
#This following funciton checks to see if the inverse 
#has already been calculated. If so, it gets the inverse
#from the cache and skips re-calculation.
#Otherwise, it calculates the inverse of the data
#and sets the value of the inverse in the cache via setinverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<- x$getinverse()#Retrieve data from Cache
  if(!is.null(m)){#If there data in cache, return stored data
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)#Calculates Inverse
  x$setinverse(m)#Store Result in Cache
  m#Return Result
}
