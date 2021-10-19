###################################################################################
###                                                                             ###    
###  This pair of functions below enable the user to more efficiently compute   ###
###  the inverse of a matrix by either computing the value of the inverse using ###
###  the 'solve()' function or accessing the inverse from cached memory if it   ###
###  has already been computed.                                                 ###
###                                                                             ###
###  This is possible in R due to lexical scoping approach to searching for     ###
###  floating variables (nesting) and makes R a powerful language for           ###
###  statistical programming.                                                   ###
###                                                                             ###
###  Note these function assumes that the initial input matrix is invertable    ###
###                                                                             ###
###  Example Use:                                                               ###
###    mtx <- [An Invertable Matrix]                                            ###                                            
###    sp_mtx <- makeCacheMatrix(mtx)                                           ###
###    cacheSolve(sp_mtx)     [on first run calculates inverse of matrix]       ###
###    cacheSolve(sp_mtx)     [on second run returns cached value of inverse]   ###
###    sp_mtx$set_init        resets initial matrix                             ###
###    sp_mtx$get_init        returns initial matrix                            ###
###    sp_mtx$set_inv         sets the cached value of the inverse matrix       ###
###    sp_mtx$get_inv         returns cached value of the inverse matrix        ###
###                                                                             ###
###################################################################################

##  The 'makeCacheMatrix' function takes an input matrix and sets up a list of 
##  functions as follows:
##    1 (set_init):  This allows the user to reset the value of the initial matrix
##    2 (get_init):  This returns the value of the initial matrix
##    3 (set_inv):   This sets the cached inverse of the initial matrix
##    4 (get_inv):   This returns the cached inverse of the initial matrix

makeCacheMatrix <- function(m_init = matrix()) {    # ensure called with a matrix

    m_inv <- NULL                                   # set initial inverse value to NULL
    set_init <- function(m_new) {                   # fn to reset initial matrix
        m_init <<- m_new                            # reset by assigning values to  
        m_inv <<- NULL                              #   variables in matrix cache
    }
    get_init <- function() m_init                   # fn to return initial matrix        
    set_inv <- function(inverse) m_inv <<- inverse  # fn to set inverse matrix
    get_inv <- function() m_inv                     # fn to return inverse matrix     
    list(set_init = set_init,                       # return a list of the four functions 
         get_init = get_init,
         set_inv = set_inv,
         get_inv = get_inv)
    }


## The 'cacheSolve' function returns the inverse of a matrix which has been 
## 'initialised' using the makeCacheMatrix function.  The 'cacheSolve' matrix will
## either compute the value of the matrix or retrieve from cached memory if it has 
## already been computed

cacheSolve <- function(mcm, ...) {                  # needs to be called with special matrix
                                                    #   already initialised with the MCM fn
    m_inv <- mcm$get_inv()                          # determine stored value for the inverse                     
    if(!is.null(m_inv)) {                           # if there is a non-null value stored
        message("retrieving cached inverse ...")    # then tell the user
        return(m_inv)                               # and exit and return that value
    }                                               # otherwise
    m_init <- mcm$get_init()                        # retrieve the initial value of the matrix
    m_inv <- solve(m_init, ...)                     # compute the inverse using 'source()'
    mcm$set_inv(m_inv)                              # set value of inverse in the special matrix list
    m_inv                                           # return the inverse value to the user
}
