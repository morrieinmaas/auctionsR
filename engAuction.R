#MIT License
#
#Copyright (c) 2016 Moritz Schlichting
#
#Please, email me about any improvements, critique, feedback or bugs to
#
# m.schlichting@student.maastrichtuniversity.nl
#
#Permission is hereby granted, free of charge, to any person obtaining a copy
#of this software and associated documentation files (the "Software"), to deal
#in the Software without restriction, including without limitation the rights
#to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#copies of the Software, and to permit persons to whom the Software is
#furnished to do so, subject to the following conditions:
#  
#  The above copyright notice and this permission notice shall be included in all
#copies or substantial portions of the Software.
#
#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#SOFTWARE.
#this will introduce a different approach for an english auction
#the algorithm will initialize an empty matrix with size = evaluation matrix
#it compares every cell i,j of the eval matrix with every cell i,j of the auction matrix 
#and raise auctionmatrix[i,j] by bidstep, if auctionmatrix[i,j]+bidstep is <= evaluationmatrix[i,j]

library(abind)
library(microbenchmark)


newauction <- function(evaluations, #an m x n matrix; rows as bidders;cols as items;entrie i j the evaluation
                          bidsteps,    #the amount to increase by every round of the auction
                          iterations){ #the number of rounds; default inf
  
  if (missing(bidsteps)) {
    bidsteps <- 1
  }
  
  if (missing(iterations)){
    #find the max entry in the evaluation matrix
    h_ind <- which.max(x)
    h_eval <- x[row(x)[h_ind], col(x)[h_ind]]
    #set the no. of iterations to the amount of time it takes to reach the max entry in bisteps
    #step size plus an extra two
    iterations <- as.integer(h_eval/bidsteps) + 2
  }
  
  #store the evaluation matrix
  x <- evaluations
  
  #create an empty copy of the auction matrix 
  compareEval <- matrix(0, nrow = nrow(x),ncol = ncol(x),dimnames = dimnames(x))
  
  aucMatrix <- abind(x,compareEval,along = 3)
  
  checkWins <- function(b){
    !length(which(b[,2] == max(b[,2]), arr.ind = TRUE)) <= 1
  }
  
  raiseBid <- function(b){
    apply(b, 1, function(a) if(a[2]+bidsteps <= a[1]){a[2] <- a[2]+bidsteps}else{a[2] <- a[2]})
  }
  
  m <- iterations
  while (m > 0){
    
    aucMatrix[,,2] <- apply(aucMatrix, 2, function(a) if(checkWins(a)){a <- raiseBid(a)}else{a[,2]<-a[,2]})
    
    m <- m - 1
  }
  
  #store the highest bidder/winner of item [,j,]
  winners <- apply(aucMatrix, 2, function(a) if(sum(a[,2])!= 0){which(a[,2] == max(a[,2]),arr.ind = T)}else{NA})
  
  #store the highest bids
  winBids <- apply(aucMatrix, 2, function(a) max(a[,2]))
  
  #return the results
  aucRes <- cbind(winBids,winners)
  
  return(aucRes)
}
