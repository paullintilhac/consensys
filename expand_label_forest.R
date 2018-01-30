library(data.table)
dat = data.table(read.csv("~/consensys/labels.csv",colClasses = rep("character",6)))
syn = as.matrix(read.csv("~/consensys/synonyms.csv",header = F))
tax = as.matrix(read.csv("~/consensys/Label Taxonomy.csv",header = F))

categories = tax[,1]
fullTable = list()

getRecursiveLabels = function(tax, label){
  highLevelInd = which(categories ==label)
  result = NULL
  len = length(highLevelInd)
  if (len>0){
    newLabels= tax[highLevelInd,2:ncol(tax)]
    newLabels = newLabels[which(newLabels!=""&!is.na(newLabels))]
    for (i in 1:length(newLabels)){
      thisLabel = newLabels[i]
      newLabels = c(newLabels,getRecursiveLabels(tax,thisLabel))
    }
    result = newLabels
  } else{
    result = label
  }
  return(unique(result))
}

for (i in 1:nrow(tax)){
  catName = tax[i,1]
  thisRow  = getRecursiveLabels(tax,catName)
  labelList = NULL
  for (j in 1:length(thisRow)){
    thisLabel = thisRow[j]
    ind = which(syn==thisLabel,arr.ind = T)
    synonyms=syn[ind[1],]
    synonyms = synonyms[which(synonyms!=""&!is.na(synonyms))]
    labelList = c(labelList,synonyms)
  }
  labelList = unique(labelList)
  fullTable[[catName]]=labelList
}

max_length  = max(unlist(lapply(fullTable,length)))
fullTable <- sapply (fullTable, function (x) {length (x) <- max_length; return (x)})
tagLength = nchar(as.character(dat$tags))
dat = dat[tagLength>0]
ml = matrix(0,nrow(dat),ncol(fullTable))

for (i in 1:nrow(dat)){
  theseTags =  unlist(strsplit(as.character(dat[i,]$tags),","))
  theseTags = gsub("(^\\s+)|(\\s+$)", "", theseTags)
  for (j in 1:length(theseTags)){
    thisTag = theseTags[j]
    for (k in 1:ncol(fullTable)){
      tagSet = fullTable[,k]
      tagSet = tagSet[!is.na(tagSet)]
      flag = thisTag%in%tagSet
      ml[i,k]=flag
    }
  }
}

ml = data.table(ml)
ml = cbind(dat$address,ml)
names(ml)[1]="acct_id"
names(ml)[2:ncol(ml)]=colnames(fullTable)
write.csv(fullTable, "~/consensys/expanded_labels.csv",row.names = F)
write.csv(ml,"~/consensys/formatted_address_classes.csv",row.names = F)
