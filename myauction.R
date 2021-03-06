#MIT License
#
#Copyright (c) 2016 Moritz Schlichting
#
#inspired by https://agtb.wordpress.com/2009/07/13/auction-algorithm-for-bipartite-matching/
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
library(phonTools)
library(tictoc)
library(pracma)
library(rje)
library(knitr)
library(rbenchmark)

#create a random matrix as an airline evaluaton matrix dummie
#the row are the bidders/airlines and the cols are the slots respectively
#e.g. alEval <- matrix(sample.int(100, 5*7, TRUE), 5, 7)

#function "myauction"
#alEval,      airline slot evaluation matrix
#iterations,  number of iterations; Inf by default
#bidsteps,    increae of bids per round; 1 by default
#output       True prints each round and results to console, true by default
#myauction perforns an ascending multiple round auction with initial prices 0


myauction <- function(alEval,      #airline slot evaluation matrix
                      iterations,  #number of iterations; pref. set to Inf
                      bidsteps,    #increae of bids per round
                      output)      #if TRUE (default) prints each round and results to console  
{
  #set some defaults for missing inputs
  if (missing(alEval)){
    alEval <- matrix(sample.int(100, 25*35, TRUE), 25, 35)
  }
  if(missing(iterations)){
    iterations <- Inf
  }
  if(missing(bidsteps)){
    bidsteps <- 1
  }
  if (missing(output)){
    output <- TRUE
  }
  
  #initialize vectors of size = no. of bidders to store the bids, (current) winners and create a 
  #queue to determine whos turn it is to bid
  bids =  zeros(1,ncol(alEval))
  wins =  zeros(1,ncol(alEval)) #this should be ncols instead of nrow as before to include all items not all bidders
  # the was also a mistake later on with [bb,h] being swapped
  bidq =  matrix(1:nrow(alEval), 1, nrow(alEval)) #bidder queue
  
  #print evaluation matrix to console; maybe comment out for large matrices
  if (output){
    message("The bidders (rows) evaluate the slots (cols):", '\n',appendLF=FALSE)
    prmatrix(alEval)
  }
  
  #find the first willing bidder
  b = which(bidq>0)[1]
  bb = bidq[1, b]
  #start timing
  tic()
  
  #execute auctioning of slots (items) whilst there are iterations left and items to auction
  while (bb != 0 && (iterations > 0) ) {
    #replace first (available) bidder out of the queue
    if(!is.na(bb)){
    bidq <- replace(bidq, bidq==bb, 0) 
    }
    #find the row index of higher bid for same item
    h <- which.max(alEval[bb,] - bids) 
    if( (alEval[bb, h] - (bids[1,h]+bidsteps)) >= 0 && wins[1,h] != bb){ 
      if (wins[1,h] > 0) {
        #look for the next accessible bid
        nb = which(bidq==0)[1] 
        #shift all bidders in the queue to the left
        bidq[1, (nb:(length(bidq)-1))] <- bidq[1, (nb+1):length(bidq)] 
        # put former left-most bidder back in the queue at the end
        bidq[1, length(bidq)] <- wins[1,h] 
      }
      #allocate winners bb to item h
      wins[1, h] <- bb
      #store increased new bid
      bids[1, h] <- bids[1, h] + bidsteps
      
      #uncomment the following if you want bidder to shift their bid to a 
      #different item if another bidder offers a higher bid 
  #    realoc <- which(alEval[,h]<bids[1,h])
  #    if (length(realoc) >= 1){
  #      subeval <- alEval[realoc,]
      
      #find lowest bid slots to bid on ; is.list(subeval)
  #      if (length(subeval) > 0){
  #        rndNewSlots <- alply(subeval,1,function(a)  which(a == min(a)))
      
      #if there are multiple find the closest
  #        if(length(rndNewSlots) >= 1){
      #this does the job now;problem is that if there is only a single entry in rndNewSlots sample()
      #gives a random slot that might not be the lowest or zero
  #            rndNS <- lapply(rndNewSlots,function(a) sample(a,1))
  #            rndNS <- as.integer(rndNS)
      
  #           if (length(alEval[realoc,h]) >= 1) {
  #            diag(alEval[realoc , rndNS]) <- alEval[realoc,h]
  #            } else if (length(alEval[realoc,h]) == 1){
  #            alEval[realoc , as.integer(rndNS)] <- alEval[realoc,h]
  #          }
         #set the old slot to 0
  #      alEval[realoc,h ] <- 0
      
  #    }
  #        }
  #     }
    }
    #print each round to console; could be nicer in the long run
    #for e.g. current leader the number at entry n of ins is the current owner/has highest bid
    #for item n
    #analogue for Bids remaining 
    if (output){
      message("Number of Bids: ", bids/bidsteps)
      message("Current Leader for item: ", wins)
      message("Bids remaining: ", bidq)
    }
    #find the next bidder 
    b = which(bidq>0)[1]
    #fix NA exeption in case there is no bidder or an item
    if (is.na(b)){
      bb <- 0
    }else{
      bb = bidq[1, b] 
    }
    #decrease the iterations (rounds) left
    iterations = iterations - 1
  }
  
  owned <- which( wins > 0)
  
  #initialize with zeros: noone has an item 
  satisfaction <- zeros(1,length(alEval[2,])) 
  for (i in owned){
    #place a one if bidders have successfully obtained an 
    satisfaction[1, i] <- 1
  }
  #stop timing
  t_end <- toc(echo = FALSE)
  #print who won item(s) and elapsed time to console
  if (output){
    #1 if satisfied, 0 otherwise
    message("Satisfied bidders: ", satisfaction) 
    message("Calculated in: ", t_end ," seconds")
  }
  result <- matrix(c(wins,bids),nrow = 2,ncol = length(bids), byrow = TRUE)
  return(result)
  
}
