##The following are the two functions which, when used together, should
##create the inverse of a matrix and cache it.

##Step 1: create the makeCacheMatrix function
  #This function needs to create a list of functions that
    #1. set the value of the matrix
    #2. get the value of the matrix
    #3. set the value of the inverse
    #4. get the value of the inverse
  #The functions should be returned as a list to then be inputted into cacheSolve

makeCacheMatrix <- function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(inverse) inv <<- inverse
  getinv<-function() inv
  list(set=set, get=get, 
       setinv=setinv, 
       getinv=getinv)
}

##Step 2: create the cacheSolve function
  #This function will create the inverse of the matrix created above
  #after checking to see if the inverse has already been calculated.
    #If inverse has already been calculated:
      #collects inverse from cache and skips further calculation
    #If inverse has not been calculated:
      #proceeds with calculation of inverse via the setinverse function

cacheSolve <- function(x,...){
  inv<-x$getinv()
  if(!is.null(inv)){
      message("getting cached data")
      return(inv)
  }
  datamatrix<-x$get()
  inv<-solve(datamatrix)
  x$setinv(inv)
  inv 
}

##After running both functions a matrix that is the inverse of your input matrix should be returned.

