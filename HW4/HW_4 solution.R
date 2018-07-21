library(ggplot2)
library(reshape2)

# Reference Letters
ref=readLines("junglebook.txt")
ref=toupper(ref)

# Create a transition Matrix
transitionMatrix=matrix(0,27,27)
rownames(transitionMatrix)=colnames(transitionMatrix)=c(toupper(letters),"")
lastletter=""

# Check if this can print the lines
for (ln in 1:3) {
  print(ref[ln])
}


# If the check is successful, move on to reading lines
# Loop through each line, within each line loop through each character
for (ln in 1:length(ref)) {
  # Print line number every 1k
  if (ln %% 1000 ==0) {cat("Line",ln,"\n")}
  for (pos in 1:nchar(ref[ln])) {
    curletter=substring(ref[ln],pos,pos)
    if (curletter %in% toupper(letters)) {
      transitionMatrix[rownames(transitionMatrix)==lastletter,
                       colnames(transitionMatrix)==curletter]=
        transitionMatrix[rownames(transitionMatrix)==lastletter,
                         colnames(transitionMatrix)==curletter]+1
      lastletter=curletter
    } else {
      if (lastletter!="") {
        transitionMatrix[rownames(transitionMatrix)==lastletter,27]=
          transitionMatrix[rownames(transitionMatrix)==lastletter,27]+1
        lastletter=""
      }
    }
  }
  curletter=""
  if (lastletter!="") {
    transitionMatrix[rownames(transitionMatrix)==lastletter,27]=
      transitionMatrix[rownames(transitionMatrix)==lastletter,27]+1
  }
  lastletter=""
}

transitionProb=sweep(transitionMatrix+1,1,rowSums(transitionMatrix+1),FUN="/")

# Let's plot this transition probability matrix 
ggplot(melt(transitionProb),aes(Var2,Var1))+geom_tile(aes(fill=value))+
  scale_fill_gradient(low="white",high="black",limits=c(0,1))+
  labs(x="Probability of Second Letter",y="Conditioning on First Letter",fill="Prob")+
  scale_y_discrete(limits = rev(levels(melt(transitionProb)$Var1)))+
  coord_equal()



# MCMC you start with a random mapping of letters
# Next you propose a new mapping by randomly switching 2 of the characters in the mapping
# If A mapped to G and L mapped Z and you switched those two, A-> Z and L-> G
# With the new mapping, you measure the likelihood and divide it by the likelihood of the previous one
# if this ratio greater than 1 then move to this mapping, if it is less move with the prob. equal to the ratio

# Decoding function
decode <- function(mapping,coded) {
  coded=toupper(coded)
  decoded=coded
  for (i in 1:nchar(coded)) {
    if (substring(coded,i,i) %in% toupper(letters)) {
      substring(decoded,i,i)=toupper(letters[mapping==substring(coded,i,i)])
    }
  }
  decoded
}

# Log probability function
log.prob <- function(mapping,decoded) {
  logprob=0
  
  lastletter=""
  for (i in 1:nchar(decoded)) {
    curletter=substring(decoded,i,i)
    if (curletter %in% toupper(letters)) {
      logprob=logprob+log(transitionProb[rownames(transitionMatrix)==lastletter,
                                         colnames(transitionMatrix)==curletter])
      lastletter=curletter
    } else {
      if (lastletter!="") {
        logprob=logprob+log(transitionProb[rownames(transitionMatrix)==lastletter,27])
        lastletter=""
      }
    }
  }
  
  if (lastletter!="") {
    logprob=logprob+log(transitionProb[rownames(transitionMatrix)==lastletter,27])
    lastletter=""
  }
  logprob
}



#coded=decode(sample(toupper(letters)),correctTxt) # randomly scramble the text

coded="RQG LWY C AGG CR CF YOP WXG BOCJB RO EPCDM W RCVG VWNQCJG CJRO W NWX LQY JOR MO CR LCRQ AOVG ARYDG"

# MCM procedure with 2000 iterations
mapping=sample(toupper(letters)) # initialize a random mapping
i=1
iters=2000
cur.decode=decode(mapping,coded)
cur.loglike=log.prob(mapping,cur.decode)
max.loglike=cur.loglike
max.decode=cur.decode
while (i<=iters) {
  proposal=sample(1:26,2) # select 2 letters to switch
  prop.mapping=mapping
  prop.mapping[proposal[1]]=mapping[proposal[2]]
  prop.mapping[proposal[2]]=mapping[proposal[1]]
  
  prop.decode=decode(prop.mapping,coded)
  prop.loglike=log.prob(prop.mapping,prop.decode)
  
  if (runif(1)<exp(prop.loglike-cur.loglike)) {
    mapping=prop.mapping
    cur.decode=prop.decode
    cur.loglike=prop.loglike
    
    if (cur.loglike>max.loglike) {
      max.loglike=cur.loglike
      max.decode=cur.decode
    }
    
    cat(i,cur.decode,"\n")
    i=i+1
  }
}
