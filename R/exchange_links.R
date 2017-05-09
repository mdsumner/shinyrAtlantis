





exchange_links <- function(object, t) {
 
  numdests <- nrow(object$exchange)
  numlayers <- ncol(object$exchange)
  numboxes <- dim(object$exchange)[3]
  exchange.links <-  exchange.links <- array(0, dim = c(numboxes, numlayers, 
                                                        numboxes, numlayers))
  exchange.links[ , , , ] <- 0
  for (i in 1:numboxes) {
    for (j in 1:numlayers) {
      for (k in 1:numdests) {
        if (!is.na(input.object$exchange[k,j,i,t])) {
          dest.i <- input.object$dest.box[k,j,i,t] + 1
          dest.j <- input.object$dest.layer[k,j,i,t] + 1
          if (!is.na(dest.i) & !is.na(dest.j)) {
            if (!((i == dest.i) & (j == dest.j))) { # only consider adjacent boxes
              exchange.links[i,j,dest.i,dest.j] <- exchange.links[i,j,dest.i,dest.j] + 1
              exchange.links[dest.i,dest.j,i,j] <- exchange.links[dest.i,dest.j,i,j] + 1
            }  
          }
        }
      }
    }
  }
  exchange.links
}

total_links <- function(exchange.links, numboxes, numlayers) {
  total.links <- array(0, dim = c(numboxes, numlayers))
  print(dim(exchange.links))
  for (i in 1:numboxes) {
    for (j in 1:numlayers) { # surface to bottom
    
      total.links[i,j] <- sum(exchange.links[i,j, , ], na.rm = TRUE)
    }
  }
  total.links
}