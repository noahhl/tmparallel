#!/bin/sh
# Author: Ingo Feinerer
# Extract documents with specified topic
if [ $# -le 1 ]
then
    echo "Error: wrong number of arguments."
    echo "Usage: $0 <topic> <xml-file> <xml-file> ..."
    exit 1
fi

if ! [ -f genSplit ]
then
    make genSplit
fi

topic=$1
shift
for filename in $*
do
    ./genSplit $filename "/LEWIS/REUTERS[not(TOPICS/D='${topic}')]" > ${filename%.*}.${topic}Topic.xml
done
