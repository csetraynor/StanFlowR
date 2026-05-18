#' Pad an Object with NAs
#'
#' This function pads an \R object (list, data.frame, matrix, atomic vector)
#' with \code{0}s. For matrices, lists and data.frames, this occurs by extending
#' each (column) vector in the object.
#' @param x An \R object (list, data.frame, matrix, atomic vector).
#' @param n The final length of each object.
#' @param pad_numeric handy if want to pad with something different than 0.
#' @export
pad <- function(x, n, pad_numeric = 0) {
  
  if(!is.numeric(pad_numeric)) {
    stop("pad_numeric must be numeric!")
  }
  if (is.data.frame(x)) {
    
    nrow <- nrow(x)
    attr(x, "row.names") <- 1:n
    for( i in 1:ncol(x) ) {
      x[[i]] <- c( x[[i]], rep(pad_numeric, times=n-nrow) )
    }
    return(x)
    
  } else if (is.list(x)) {
    if (missing(n)) {
      max_len <- max( sapply( x, length ) )
      return( lapply(x, function(xx) {
        return( c(xx, rep(pad_numeric, times=max_len-length(xx))) )
      }))
    } else {
      return( lapply(x, function(xx) {
        if (n > length(xx)) {
          return( c(xx, rep(pad_numeric, times=n-length(xx))) )
        } else {
          return(xx)
        }
      }))
    }
  } else if (is.matrix(x)) {
    
    return( rbind( x, matrix(pad_numeric, nrow=n-nrow(x), ncol=ncol(x)) ) )
    
  } else {
    
    return( c( x, rep(pad_numeric, n-length(x)) ) )
    
  }
  
}

