##  Funciones que regresan la inversa de una matriz


## Crea un "Vector" con caracteristicas para guardar en cache la inversa

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  
  set <- function(y) {
    x <<- y
    Inv <- NULL
  }
  
  
  get <- function() x
  
  
  setInv <- function(Inversa) Inv <<- Inversa
  
  
  getInv <- function() Inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## devuelve la inversa de una matriz, ya sea calculandola o buscando el resultado en cahe

cacheSolve <- function(x, ...) {
  Inv <- x$getInv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  M <- x$get()
  Inv<- solve(M, ...)
  x$setInv(Inv)
  Inv
}
