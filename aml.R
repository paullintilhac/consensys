
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
