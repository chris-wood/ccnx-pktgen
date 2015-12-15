#!/bin/bash
PREFIX=$1
IP=$2
PORT=$3

PIPE="ncin"
rm -rf $PIPE

echo mkfifo $PIPE
mkfifo $PIPE

#echo `nc -u $IP $PORT < ncin`
nc -vu $IP $PORT < $PIPE &

for fname in `ls ${PREFIX}*`
do
    echo "cat ${fname} > ${PIPE}"
    cat ${fname} > ${PIPE}
    echo  > ${PIPE}
    sleep 0.5
done
