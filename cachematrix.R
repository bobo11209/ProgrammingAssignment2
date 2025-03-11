
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Stockage du cache pour l'inverse
  
  set <- function(y) {
    x <<- y  # Met à jour la matrice
    inv <<- NULL  # Réinitialise le cache
  }
  
  get <- function() x  # Retourne la matrice
  
  setInverse <- function(inverse) inv <<- inverse  # Stocke l'inverse dans le cache
  getInverse <- function() inv  # Retourne l'inverse mis en cache
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(cache) {
  inv <- cache$getInverse()
  
  if (!is.null(inv)) {
    message("Utilisation du cache")
    return(inv)
  }
  
  data <- cache$get()
  inv <- solve(data)  # Calcul de l'inverse
  cache$setInverse(inv)
  inv
}

# Exemple d'utilisation
mat <- matrix(c(2, 1, 1, 2), 2, 2)
cached_mat <- makeCacheMatrix(mat)
cacheSolve(cached_mat)  # Calcule et stocke l'inverse
cacheSolve(cached_mat)  # Utilise l'inverse en cache
