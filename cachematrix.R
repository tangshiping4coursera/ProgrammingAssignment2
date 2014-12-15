## Use function makeCacheMatrix to cache the inverse of a matrix
## Use function cacheSolve to get the cached result to speed the calc
## Usage: 
##  x<-matrix(c(1,2,3,4), 2,2)
##  y<-makeCacheMatrix(x)
##  z<-cacheSolve(y)

## 用于创建可缓存逆矩阵的特殊“矩阵”对象
makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  get <- function() x
  setinverse <- function(y) inverse_x <<- y
  getinverse <- function() inverse_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 用于计算上述makeCacheMatrix返回的特殊“矩阵”的逆矩阵。
## 如果已经计算逆矩阵（且尚未更改矩阵），那么cachesolve将检索缓存中的逆矩阵
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
