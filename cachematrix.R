## La idea es crear una funcion que permita obtener la inversa de una matriz. 
## Primero se genera un vector con cierto tamaño, y en la segunda funcion
## se coge el vector anteriormente generado y almacenado en la funcion 
##makecachematrix() para calcular su inversa mediante la funcion solve()


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL             ## Declaramos la variable con valor nulo
    set <- function(y){     ##Recibe el valor
        x <<- y             ##Asigna y a x
        inv <<- NULL
    }
    get <- function() x     ##Devuelve el valor de la matriz que se tenga
    setinv <- function (inversa) inv <<- inversa ##Darle el valor de la inversa cuando es llamada
    getinv <- function() inv        #Sirve para ver el valor de la inversa
    list(set = set, get = get,       ##Se crea una lista como en el ejemplo 
         setinv = setinv,
         getinv = getinv)
}



cacheSolve <- function(x, ...) {
    inv <- x$getinv()           #En esta linea adquirimos los valores de la funcion anterior
    if (!is.null(inv)){         #Preguntarnos si tiene algun valor
        message ("Obtener datos almacenados")
        return(inv)
    }
    data <- x$get()             #Creamos la varaible data para asugnar la matriz anterior
    inv <- solve(data, ...)     #Se realiza el calculo de la matriz inversa y se le asigna a la variable inv
    x$setinv(inv)
    
    inv ## inv arroja el resultado: la inversa de la matriz x
}