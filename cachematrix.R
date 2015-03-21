## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#######################################################
#  Requirements from Assignment:
#
#  makeCacheMatrix:  This function creates a special "matrix" 
#  object that can cache its inverse.
#
#######################################################
#  makeCacheMatrix:  This function does several things
#  1. This is a function that contains a set of functions
#     used to create the values for the global env and 
#     for the vector "list"
#  2. The functions are: set, get, setinv, getinv
#     A. set : initilizes the global varibles
#     B. get : returns the current value of the global matrix x
#     C. setinv : computes the inverted matrix of the value x
#     D. getinv : gets the saved value of the inverted matrix
#  3. The "list" vector contains the addresses of the function.
########################################################

makeCacheMatrix <- function(x = matrix()) {
      #######################################################
      #     Parameter check
      # First test to make sure that the matrix is "square"
      #######################################################
      xrow<-nrow(x)
      ycol<-ncol(x)
      if (xrow != ycol){
            message ("This is not a square matrix")
            return (NULL)     
      }

      ##################################
      #  Parameter OK, start function
      ##################################
      m<-NULL                      # not needed
      set<-function(y=matrix()){
            # assign("x",y=.GlobalEnv)
            x<<-y
            m<<-NULL
      }
      #####################################
      get<-function(){ return(x) }       # get stored value
      setinv<-function(x=matrix()){ 
            #m<-solve(x)
            #
            #assign("m",m=.GlobalEnv)        # generate inverted matrix
            m<<-solve(x)
      }
      getinv<-function(){ return (m) }
      list<-c(set=set,get=get,setinv=setinv,getinv=getinv)      
      vlist<<-list
}

## Write a short comment describing this function
###################################################
#  Requirements from Assignment:
#
#  cacheSolve: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. 
#  
#  If the inverse has
#  already been calculated (and the matrix has not changed:
#  [Test for matrix change or assumption ?? ), then
# `cacheSolve` should retrieve the inverse from the cache.x
##################################################
#
#  cacheSolve.
#  1. It gets the value "m" from the environment and tests
#     to make sure that it isn't "NULL".
#  2. If it isn't NULL it returns that value after it checks
#     to be sure that the two matricies are the same.
#  3. ELSE:  it calls the function "get" to retrieve
#     the matrix (x) from the "super-environment"
#  4. The inversion of the matrix is calculated and
#     stored in two places, the value "m" and in the
#     list vector for easy access.
#

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'

      #####################################
      # Now follow the example code
      #####################################
      m<-vlist$getinv()    # get the current value
      if (!is.null(m)){   # value exists??
                  data<-vlist$get()
            if( identical(data, x)){   # comparison for new output
                  message("getting cached data")
                  return (m)    # value does exist - return
            }else{       # data arrays not the saem
                 message("Stored Matrix and answer don't match")
                 message("Will calculate and return new values")
                 vlist$set(x)     # data recovery from list vector
                 m<-solve(x)     # solve the matrix
                 vlist$setinv(m)     # store the value in the list
                 return( m )
            } 
      }
      #####################################
      #  Data does not exist:  Create here
      data<-vlist$get()        # data recovery from list vector 
      m<-vlist$setinv(data)       # store the value in the list
      m<<-m
}
