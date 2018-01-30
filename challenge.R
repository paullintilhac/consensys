
library(data.table)
library(Matrix)
library(lattice)
library(reshape2)
library(EMCluster)
library(expm)
library(mclust)
library(rARPACK)
library(spam)
library(pmclust)
library(foreach)
library(doParallel)
tx = data.table(read.csv("consensys/txs_sample.csv",header = T,as.is = T,
                         colClasses = c("integer","numeric","numeric","numeric",
                                        "numeric","character","character","integer","character")))

blocks = data.table(read.csv("c:/users/paul/consensys/blocks_sample.csv",header = T,as.is = T,
                             colClasses = c("numeric","numeric","integer","character",
                                            "character","character","integer")))
badAddresses=c("0x6a0a0fc761c612c340a0e98d33b37a75e5268472",
               "0x7c20218efc2e07c8fe2532ff860d4a5d8287cb31",
               "0xbd37ee00e47c0fe0def80bd95c498698b4a15235",
               "0xb6389d6575966f946e190b9d538c3259857ed2c7")
blocks = blocks[!duplicated(blocks),]
dat =merge(tx,blocks,by = "block.number",suffixes=c(".tx",".block"))
dat$bad = ifelse(tx$to%in%badAddresses,1,0)
badTX = dat[bad==1,]
##


# mean(tx$gas) #125032.6
# mean(badTX$gas) # 173268
# mean(tx$gas.price) #27243941280
# mean(badTX$gas.price) #51532042413
# mean(tx$gas.used) #38539.59
# mean(badTX$gas.used)# 149609.9
# badBlockNums = unique(badTX$block.number)
# badBlocks = blocks[block.number %in%badBlockNums,]
# mean(blocks$gas.used) #282889.8
# mean(badBlocks$gas.used) #1217368
mean(badTX$value)
mean(tx$value)
mean(dat$gas.used.block)
mean(badTX$gas.used.block)
########Question 2
#thing = melt(dat, id.vars = c("bad"),measure.vars = c("gas","gas.price","gas.used.tx","value","gas.used.block"))
#test= thing
#test[,bad:=NULL]
#boxplot(value~variable,test)

addresses = unique(union(unique(tx$to),unique(tx$from)))
addressesTo = unique(tx$to)

aggData=tx[,list("N"=.N,"value"=sum(value),"flag"=ifelse(.N>0,1,0),"address1"=max(to,from),"address2"=min(to,from)),by = c("to","from")]

aggData$left = paste0(aggData$from,aggData$to)
aggData$right = paste0(aggData$to,aggData$from)
sum(aggData$right%in%aggData$left) #=~6000
aggData$hasTwin = ifelse(aggData$left%in%aggData$right,1,0)
aggData2=aggData[,list("N"=sum(N),"value"=sum(value),"flag"=max(flag),"hasTwin"=max(hasTwin)),by = c("address1","address2")]


indI=match(aggData2$address1,addresses)
indJ=match(aggData2$address2,addresses)
comb1 = c(indI,indJ)
comb2= c(indJ,indI)
comb = data.table(cbind(comb1,comb2))
newInd = which(!duplicated(comb))
comb = comb[newInd,]
twinData = aggData2[hasTwin ==1,]
twinAddresses = unique(union(twinData$from,twinData$to))

comb=comb[comb1<=comb2,]

sm =sparseMatrix(i=comb$comb1,
                 j=comb$comb2,
                 x=rep(1,nrow(comb)),
                 symmetric = T)

tm =sparseMatrix(i=match(twinData$address1,addresses),
                 j=match(twinData$address2,addresses),
                 x=twinData$flag,
                 giveCsparse =T)
degTwins = colSums(tm)
mean(degTwins[degTwins!=0])
ex=which(aggData$from == "0x9ad57c8a76ac8528dc6ecdda801865a317e36758"
         &aggData$to=="0x6a0a0fc761c612c340a0e98d33b37a75e5268472")

degree = colSums(sm)
mean(degree[degree!=0])


degreeMat =sparseMatrix(j=seq(1,length(addresses)),
                        i=seq(1,length(addresses)),
                        x=degree,
                        giveCsparse = T)

L = degreeMat-sm

k=.01
identity = sparseMatrix(i=seq(1,length(addresses)),j=seq(1,length(addresses)),x = rep(1,length(addresses)))

startVec = as.matrix(rep(0,length(addresses)))
badInd = match(badAddresses,  addresses)

startVec[badInd,]=1

## second order Taylor expansion of matrix exponential exp(-kL)
nextVec =identity%*%startVec -k*L%*%startVec+.5*(k^2)*L%*%(L%*%startVec)

badFlag = nextVec>=.001

ev <- eigs(L,4)



#add "from" and "to" degree to transaction dataset
degFrame = data.table(cbind(addresses,degree))
degFrame$degree = as.numeric(degFrame$degree)
setnames(degFrame,c("from","from.degree"))
dat = merge(dat,degFrame,by = "from")
setnames(degFrame,c("to","to.degree"))
dat = merge(dat,degFrame,by = "to")
#take only numeric features
GMM = dat[,c("gas","gas.price","gas.used.tx","value","gas.used.block","from.degree","to.degree")]
GMM$gas = log(GMM$gas)
GMM$gas[GMM$gas==-Inf]=0
GMM$gas.price = log(GMM$gas.price)
GMM$gas.price[GMM$gas.price==-Inf]=-1 #want to differentiate from gas.price = 1, which has log 0
GMM$gas.used.tx=log(GMM$gas.used.tx)
GMM$gas.used.block=log(GMM$gas.used.block )
GMM$value = log(GMM$value)
save(GMM,file="c:/users/paul/consensys/GMM.RDATA")
GMM$value[GMM$value==-Inf]=-40
GMM=as.matrix(GMM)
#run parallelized EM algorithm for GMM

chunkSize = 10000
# 
rm(tx)
rm(dat)
rm(aggData)
rm(aggData2)
rm(addresses)
gc()

registerDoParallel(cores=4)
getDoParWorkers()
start = Sys.time()
list<-foreach(i=1:4,.packages="mclust") %dopar% {
  print(i)
  cluster = Mclust(GMM[((i-1)*chunkSize+1):(i*chunkSize),],G=4)
}
end = Sys.time()
print(end-start)
load("c:/users/paul/downloads/listBig.RDATA")

save(list,file="consensys/clusterSmall.RDATA")

badTransactionInd = which(dat$from%in%addresses[badFlag[,1]]|dat$to%in%addresses[badFlag[,1]])
badTransactions = GMM[badTransactionInd,]


mean(bestProbs)
mean(bestProbs[badTransactionInd],na.rm=T)
mean(uncertainty)
mean(uncertainty[badTransactionInd],na.rm=T)
results = data.table(matrix(0,0,8))
setnames(results,c("bad","fold",paste0("C",seq(1,4)),"prob","uncertainty"))
results$fold =as.integer(results$fold)
results$bad = as.character(results$bad)
results$prob=as.numeric(results$prob)
results$uncertainty = as.numeric(results$uncertainty)
for (j in 1:4){
  classes = as.factor(list[[j]]$classification)
  #classes = as.factor(list[[j]]$class)
  distr = round(table(classes)/sum(table(classes)),3)
  thisInd = ((j-1)*chunkSize+1):(j*chunkSize)
  thisBadInd = intersect(thisInd,badTransactionInd)
  distrBad = round(table(classes[thisBadInd])/sum(table(classes[thisBadInd])),3)
  badRow = data.table(as.matrix(t(as.numeric(distrBad))))
  colnames(badRow)=paste0("C",seq(1,4))
  goodRow= data.table(as.matrix(t(as.numeric(distr))))
  colnames(goodRow)=paste0("C",seq(1,4))
  bad = "good"
  fold = j
  goodRow = cbind(fold,goodRow)
  goodRow = cbind(bad,goodRow)
  bad = "bad"
  badRow = cbind(fold,badRow)
  badRow = cbind(bad,badRow)
  uncertaintyVec = list[[j]]$uncertainty
  probs = list[[j]]$z
  bestProbs = rep(0,nrow(probs))
  for (i in 1:nrow(probs)){
    bestProbs[i]=probs[i,classes[i]]
  }
  
  prob = mean(bestProbs)
  goodRow = cbind(goodRow,prob)
  prob = mean(bestProbs[thisBadInd-(j-1)*chunkSize])
  badRow = cbind(badRow,prob)
  
  
  uncertainty = mean(uncertaintyVec)
  goodRow = cbind(goodRow,uncertainty)
  
  uncertainty = mean(uncertaintyVec[thisBadInd-(j-1)*chunkSize])
  badRow = cbind(badRow,uncertainty)

  results = rbind(results,badRow)
  results = rbind(results,goodRow)
}

meltedResults = melt(results,id.vars = c("bad","fold"),variable.name = "cluster")
bwplot(value~cluster,meltedResults[bad=="bad"])
boxplot()
mean(bestProbs)

mean(bestProbs[badTransactionInd],na.rm = T)
means = cluster$parameters$mean
means=cbind(c("gas","gas.price","gas.used.tx","value","gas.used.block","from.degree","to.degree"),means)
means = data.table(means)
setnames(means,c("feature","C1","C2","C3"))
means$feature = as.factor(means$feature)
xyplot(C1+C2+C3~feature,means)

