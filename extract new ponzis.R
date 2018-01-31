library(data.table)


dat = read.csv("~/consensys/ponzis.csv",header = F)
thing = dat[,1]
thing = as.character(thing)
ind = grep("0x",thing)

ponziAddys = tolower(thing[ind])
ponziNames = thing[ind-1]
ponzis = cbind(ponziAddys,ponziNames)
oldPonzis = data.table(read.csv("~/consensys/formatted_address_classes.csv"))
ncol1 = ncol(oldPonzis)
oldPonzis = data.table(read.csv("~/consensys/formatted_address_classes.csv",colClasses = rep("character",ncol1)))

oldPonziAddys = oldPonzis[pyramid_2 ==1]$address

mean(oldPonziAddys%in%ponziAddys)

oldPonziAddys = tolower(oldPonziAddys)

ind = which(!ponziAddys%in%oldPonziAddys)
newPonzis = ponzis[ind,]
write.csv(newPonzis,"~/consensys/new ponzis.csv")

length(ponzis)
length(oldPonziAddys)
length(allPonzis)

tolower("0xfEa8d770B86EbFf82De9D102DA5bab89FD02408F")
