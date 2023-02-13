# February 13, 2023
# Ashley Implements Kaplan Meier
# Tables 464, 465

#Since interval construction is a choose your own adventure, 
#I'm going to just skip listing the months of the deaths and refer to 
#the intervals with their integer indices j.


#Table 464:

#the data
intervals <- c(1,2,3,4,5)
n <- c(8,5,4,2,1)
nprime <- c(7,4,3,1,1)
lambda <- c(2,0,1,0,1)

#initialize estimate vector
phat <- rep(0, times = 0) 

#start product chain
phat[1] <- nprime[1]/n[1]

#continue product chain
for(j in 2:length(intervals)) {
  phat[j] <- phat[j-1]*(nprime[j]/n[j])
}

#Table 465:

#the data
intervals <- c(1,2,3,4,5,6,7)
n <- c(100, rep(NA, times = 6)) #set initial n
delta <- c(3,5,4,10,9,6,15)
lambda <- c(0,20,0,0,12,0,16)

#compute n and nprime
for(i in 2:length(n)){
  n[i] <- n[i - 1] - delta[i-1] - lambda[i - 1]
}
nprime <- n - delta

#initialize estimate vector
phat <- rep(0, times = 0) 

#start product chain
phat[1] <- nprime[1]/n[1]

#continue product chain
for(j in 2:length(intervals)) {
  phat[j] <- phat[j-1]*(nprime[j]/n[j])
}


#don't ask questions, just run the line....
browseURL("https://www.youtube.com/watch?app=desktop&v=6dYWe1c3OyU")