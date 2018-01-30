dat = read.csv("~/consensys/labels.csv")


tags = tolower(unique(unlist(strsplit(as.character(dat$tags),","))))
tags = gsub("(^\\s+)|(\\s+$)", "", tags)

length(unique(tags))

syn = unique(grep("vest",tags,value = T))
syn
if (length(syn)>0)
tags = tags[-which(tags%in%syn)]


dat2 = read.csv("~/consensys/synonyms.csv",header = F)
syns = unique(unlist(lapply(dat2,as.character)))
thing = (unique(c(as.matrix(dat2))))
thing = thing[!is.na(thing)]
tags= tags[-which(tags%in%thing)]
