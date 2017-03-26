library(data.table)
library(Matrix)

tx = data.table(read.csv("c:/users/paul/consensys/txs_sample.csv",header = T,as.is = T,
    colClasses = c("integer","numeric","numeric","numeric",
                   "numeric","character","character","integer","character")))

blocks = data.table(read.csv("c:/users/paul/consensys/txs_sample.csv",header = T,as.is = T,
    colClasses = c("numeric","numeric","integer","character",
                   "character","character","integer")))



## 
c3From = tx[from=="0xb284e6a25d0972f9a92fec45d2075067db2d49b0",] #empty
c3To = tx[to=="0xb284e6a25d0972f9a92fec45d2075067db2d49b0",]

suicideFrom = tx[from == "0x6a0a0fc761c612c340a0e98d33b37a75e5268472",] #empty
suicideTo = tx[to == "0x6a0a0fc761c612c340a0e98d33b37a75e5268472",]

createFrom = tx[from=="0x7c20218efc2e07c8fe2532ff860d4a5d8287cb31",]#empty
createTo = tx[to=="0x7c20218efc2e07c8fe2532ff860d4a5d8287cb31",]

c4From = tx[from=="0xbd37ee00e47c0fe0def80bd95c498698b4a15235",]#empty
c4To = tx[to == "0xbd37ee00e47c0fe0def80bd95c498698b4a15235",]

c5From = tx[from == "0xb6389d6575966f946e190b9d538c3259857ed2c7",]#empty
c5To = tx[to == "0xb6389d6575966f946e190b9d538c3259857ed2c7",]

cANDs = intersect(createTo$from,suicideTo$from) #especially bad?

addresses = unique(union(unique(tx$to),unique(tx$from)))
addressesTo = unique(tx$to)

aggData=tx[,list("N"=.N,"value"=sum(value),"flag"=ifelse(.N>0,1,0)),by = c("to","from")]
#aggData=aggData[to%in%addressesTo,]

sm =sparseMatrix(i=c(match(aggData$from,addresses),seq(length(unique(aggData$to))+1,length(addresses))),
                        j=c(match(aggData$to,addresses),seq(length(unique(aggData$to))+1,length(addresses))),
                        x=c(aggData$flag,rep(0,length(addresses)-length(unique(aggData$to)))),
                        giveCsparse = F)

ex=which(aggData$from == "0x9ad57c8a76ac8528dc6ecdda801865a317e36758"
         &aggData$to=="0x6a0a0fc761c612c340a0e98d33b37a75e5268472")

degree = colSums(sm)
degree  = c(degree,rep(0,length(addresses)-length(degree)))
degreeMat =sparseMatrix(j=seq(1,length(addresses)),
                 i=seq(1,length(addresses)),
                 x=degree,
                 giveCsparse = F)
laplacian = degreeMat - sm
k=.01

badInd = match(
                c("0x6a0a0fc761c612c340a0e98d33b37a75e5268472",
                 "0x7c20218efc2e07c8fe2532ff860d4a5d8287cb31"),
              addresses)

maybeBadAddresses = unique(createTo$from)

maybeBadInd = match(
  maybeBadAddresses,
addresses)

startVec = as.matrix(rep(0,length(addresses)))
startVec[badInd,]=1
delSM = 
nextVec =startVec -as.matrix(k*laplacian%*%startVec)
nextVec=nextVec-as.matrix(k*laplacian%*%nextVec)
nextVec[maybeBadInd]

#write.csv(addresses,"~/consensys/uniqueAddresses.csv")
listVec = rep(list(),length(addresses))

weightVec = rep(0,length(addresses))

weightVec[which(addresses == "0x6a0a0fc761c612c340a0e98d33b37a75e5268472")]=1
weightVec[which(addresses == "0x7c20218efc2e07c8fe2532ff860d4a5d8287cb31")]=1


