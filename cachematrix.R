## Function "makeCacheMatrix" create these functions:
##      mSet    - set the matrix
##      mGet    - get the matrix
##      mInvSet - set the inverse matrix
##      mInvGet - get the inverse matrix
##
## Used variables are:
##      oMatr   - original matrix
##      mCache  - cached inverse matrix
##      nMatr   - new matrix

makeCacheMatrix <- function(oMatr = matrix()) {
        ## initialization of cache matrix (mCache) to NULL
        mCache = NULL
        
        ## definition of mSet function in working environment 
        mSet <- function(nMatr) {
                ## assign a value to matrix from different environment
                oMatr <<- nMatr
                ## new matrix do not have inverse matrix calculated yet (mCache = NULL)
                mCache <<- NULL
        }
        
        ## mGet function returns actual matrix
        mGet <- function() oMatr
        
        ## mInvSet function allows you to setup inverse matrix manually
        mInvSet <- function(mInv) mCache <<- mInv
        
        ## mInvGet function returns actual inverse matrix
        mInvGet <- function() mCache
        
        list(mSet = mSet, mGet = mGet, mInvSet = mInvSet, mInvGet = mInvGet)
}


## Fuction "cacheSolve" check if there is no inverse matrix already assigned
## in the cache and if not calculate inverse matrix from martix in cache.
## Used variables are:
##      oMatr   - original matrix
##      mCache  - cached inverse matrix
##      matr    - temporary matrix for inverse matrix calculation

cacheSolve <- function(oMatr, ...) {
        ## get actual inverse matrix
        mCache <- oMatr$mInvGet()
        
        ## if actual inverse matrix exist in cache then return it as result
        ## (according assignment) 
        if(!is.null(mCache)){
                message("getting cached data")
                return(mCache)
        }
        
        ## calculate inverse matrix and assign it to cache
        matr <- oMatr$mGet()
        mCache <- solve(matr)
        oMatr$mInvSet(mCache)
}
