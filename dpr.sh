#!/bin/bash

# Required: sudo apt-get install parallel && sudo pip install azure-cli

KEY="DefaultEndpointsProtocol=https;AccountName=alethioparityraw;AccountKey=fyqa+KOtE9mK2A5OQ17e/kL7GqkGXWCDvMADQEtcpCR/AbOhKTTbih+ZHWj5J0NHGPv/SkQ6+uwSEIqXWHFbgA==;EndpointSuffix=core.windows.net"

DST="./"

partSize=1000
numParts=10
start=$@
end=$(($start+$numParts*$partSize-1))
outdir="$start-$end"
dir=trace_block
mkdir -p $DST/$outdir


for block in `seq 0 $(($partSize-1))`; do
for part in `seq 0 $(($numParts-1))`; do
  thisBlock="$(($start+$part*$partSize+$block))"
  divisor="$(($start+$part*$partSize))"
  #echo $divisor
  #echo $thisBlock
  outname=$outdir/$(printf "%012d" $divisor).json
  BLK=$(printf "%012d" $thisBlock)
  NAME=$dir/$BLK.json
  #echo $NAME
  echo az storage blob download --max-connections 1 --open-mode ab --connection-string \'$KEY\' --container-name parity --name $NAME --file $DST/$outname\;
done | parallel
done