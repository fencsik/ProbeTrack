### latinsq takes a variable that holds an NxN matrix or a filename
### referring to a file that 
latinsq <- function(x) {
  if (is.matrix(x)) {
    ls <- x
  } else if (is.character(x) && file.exists(x[1])) {
    ls <- as.matrix(read.table(x[1]))
  } else {
    stop("argument not recognized")
  }

  if (nrow(ls) != ncol(ls) || nrow(ls) < 2) {
    stop("number of rows and columns must be equal and greater than 1")
  }

  n <- nrow(ls)

  rownames(ls) <- colnames(ls) <- as.character(1:n)

  print(ls, quote=F)

  ls <- ls[order(runif(n, min=1, max=n)),]
  ls <- ls[,order(runif(n, min=1, max=n))]

  rownames(ls) <- colnames(ls) <- as.character(1:n)

  ls
}
