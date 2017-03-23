library(data.table)
tx = data.table(read.csv("~/downloads/txs_sample.csv",header = T,as.is = T,
    colClasses = c("integer","numeric","numeric","numeric",
                   "numeric","character","character","integer","character")))

blocks = data.table(read.csv("~/downloads/blocks_sample.csv",header = T,as.is = T,
    colClasses = c("numeric","numeric","integer","character",
                   "character","character","integer")))

#to and from two questionable contracts
c1From = tx[from=="0xd6a64D7E8C8A94fa5068ca33229D88436A743B14",] #empty 
c1To = tx[to=="0xd6a64D7E8C8A94fa5068ca33229D88436A743B14",] #empty

c2From = tx[from=="0x0c40cF6dd1B74c6eDfC4729021A11f57B31e3c27",] #empty
c2To = tx[to=="0x0c40cF6dd1B74c6eDfC4729021A11f57B31e3c27",] #empty

## 
c3From = tx[from=="0xb284e6a25d0972f9a92fec45d2075067db2d49b0",] #empty
c3To = tx[to=="0xb284e6a25d0972f9a92fec45d2075067db2d49b0",]

suicideFrom = tx[from == "0x6a0a0fc761c612c340a0e98d33b37a75e5268472",] #empty
suicideTo = tx[to == "0x6a0a0fc761c612c340a0e98d33b37a75e5268472",]

createFrom = tx[from=="0x7c20218efc2e07c8fe2532ff860d4a5d8287cb31",]
createTo = tx[to=="0x7c20218efc2e07c8fe2532ff860d4a5d8287cb31",]

cANDs = intersect(createTo$from,suicideTo$from)
c3ANDs = intersect(suicideTo$from,c3To$from) #empty 
c3ANDc = intersect(createTo$from,c3To$from)  #empty


exBlock = blocks[timestamp==exTX$timestamp]
