#' Create an order-preserving, roughly square grid based on lat/long
#'
#' @param x a vector of unit longitudes 
#' @param y a vector of unit latitude 
#' @param unit.name a vector of names
#' @param num.iterations an integer of how many times to try shrinking
#' @importFrom magrittr "%>%"
#' @importFrom reshape2 melt
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @export

create_new_grid <- function(x,y, unit.name, num.iterations = 100){
  num.units <- length(unit.name)
  index <- as.integer(1:num.units)
  # Order units both horizontally & verticaly 
  row <- rank(y)
  col <- rank(x)
  df <- data.frame(x,y,row,col,index, unit.name)

  # Populate position matrix 
  P = matrix(rep(0,num.units*num.units), nrow = num.units, ncol = num.units)
  for(i in 1:nrow(df)){
    r <- df[i,"row"]
    c <- df[i,"col"]
    P[r,c] <- df[i,"index"]
  }

  RowCollision <- function(r,P){
    "Returns true if by merging this row w/ one below generates a collision "
    top <- P[r,] > 0
    bot <- P[r + 1,] > 0
    as.integer(top %*% bot)
  }

  ColCollision <- function(c,P){
    "Returns true if by merging this col w/ one to right generates a collision "
    left <- P[,c] > 0
    right <- P[,c + 1] > 0
    as.integer(left %*% right)
  }

  CollapseRow <- function(r,P){
    "Returns a new matrix with collapsed row removed (shrinks matrix by 1 row)"
    top <- P[r,] 
    bot <- P[r + 1,]
    P2 <- P
    P2[r,] <- top + bot
    P2[-(r+1),]
  }

  CollapseCol <- function(col,P){
    "Returns a new matrix with collapsed col removed (shrinks matrix by 1 row)"
    left <- P[,col] 
    right <- P[,col + 1]
    P2 <- P
    P2[,col] <- left + right
    P2[,-(col+1)]
  }

  CollapsibleRows <- function(P){
    "Finds rows that could be collapsed"
    num.rows <- dim(P)[1]
    if(is.null(num.rows)) return(NULL)
    candidate.rows <- c()
    for (r in 1:(num.rows - 1)){
      if (RowCollision(r,P) == 0){
      candidate.rows <- c(candidate.rows, r)
      }
    }
    candidate.rows
  }

  CollapsibleCols <- function(P){
    "Finds columns that could be collapsed"
    num.cols <- dim(P)[2]
    if( is.null(num.cols) ) return(NULL)
    candidate.cols <- c()
    for (col in 1:(num.cols - 1)){
      if (ColCollision(col,P) == 0){
        candidate.cols <- c(candidate.cols, col)
      }
    }
    candidate.cols
  }

  ShrinkRow <- function(P){
    "Shrinks a row, if possible. If more than 1 row possible, selects at random"
    collapsible.rows <- CollapsibleRows(P)
    if(length(collapsible.rows) == 0) return(P)
    if(length(collapsible.rows) == 1) {
     return(CollapseRow(collapsible.rows, P))
    } else {
      r <- sample(collapsible.rows, 1)
      return(CollapseRow(r, P))
    }
  }

  ShrinkCol <- function(P){
  "Shrinks a row, if possible. If more than 1 row possible, selects at random"
    collapsible.cols <- CollapsibleCols(P)
    if(length(collapsible.cols) == 0) return(P)
    if(length(collapsible.cols) == 1) {
      # if there is only 1, it gets turned into a scalar. Ugh. 
      return(CollapseCol(collapsible.cols, P))
    } else {
      c <- sample(collapsible.cols, 1)
      return(CollapseCol(c, P))
    }
  }

  OK <- function(P, n){
    "Helper function useful when debugging shirnking"
    x <- as.vector(P)  
    length(sort(x[x !=0 ])) == n
  }

  ShrinkMatrix <- function(P){
    "Shrinks longest dimension first, stops when can't shrink on either dimension"
    num.rows <- dim(P)[1]
    num.cols <- dim(P)[2]
    if (num.rows > num.cols){
      P.smaller <- ShrinkRow(P)
      P.smaller <- ShrinkCol(P.smaller)
    } else {
      P.smaller <- ShrinkCol(P)
      P.smaller <- ShrinkRow(P.smaller)
    }
    if (all(dim(P) == dim(P.smaller))) {
      return(P.smaller)
    } else {
      return(ShrinkMatrix(P.smaller))
    }
  }

  # brute force to find a good shrunken matrix  
  best.dim <- Inf 
  P.best <- P
  for(i in 1:num.iterations){
    P.smaller = ShrinkMatrix(P)
    if(sum(dim(P.smaller)) < best.dim){
      P.best = P.smaller
      best.dim <- sum(dim(P.smaller))
    }
  }

  df.grid <- melt(P.best) %>% filter(value != 0)
  colnames(df.grid) <- c("new.row", "new.col", "index")
  df.combo <- df.grid %>% left_join(df)
  df.combo
}
