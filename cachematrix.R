## These two functions are imitations of the mean cache example.
## Author: JackDavid127 (Coursera rprog-011)
## Time: 2015/02/22 18:23 (UTC+8)


## This function makeCacheMatrix has two local variables named
## "changed" and "inv". The first variable shows whether this "x"
## matrix has been changed ,i.e. its latest inverse form has been
## calculated. The other variable stores the inverse of this matrix.
## It also has five functions named "setmat", "getmat", "setinv",
## "getinv", and "ischanged" used for modifying the matrix, getting
## the matrix, updating the inverse, getting the inverse, and ensure
## whether it's been changed.

## Noted: The "changed" variable is set as TRUE in the beginning
## because we don't calculate the inverse form as we create this object.

makeCacheMatrix <- function(x = matrix()) {
	changed<-TRUE
	inv<-NULL
	setmat<-function(y){
		x<<-y
		changed<<-TRUE
	}
	getmat<-function() x
	setinv<-function(y){
		inv<<-y
		changed<<-FALSE
	}
	getinv<-function() inv
	ischanged<-function() changed
	list(setmat=setmat,getmat=getmat,setinv=setinv,getinv=getinv,ischanged=ischanged)
}


## This function cacheSolve is used for solving matrix inverse.
## If we have up-to-date cache we can just return it. But if we don't,
## we'll have to solve it, replace the original one in the cache,
## and return it.

cacheSolve <- function(x, ...) {
	if(!x$ischanged()) return(x$getinv())
	mat<-x$getmat()
	inv<-NULL
	r<-nrow(mat)
	for(i in 1:r){
		newrow<-mat[i,]
		inv<-cbind(inv,newrow,deparse.level=0)
	}
	oldnames<-dimnames(mat)
	if(!is.null(oldnames)) dimnames(inv)<-list(oldnames[[2]],oldnames[[1]])
	x$setinv(inv)
	inv
}
