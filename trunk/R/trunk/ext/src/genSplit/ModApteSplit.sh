#!/bin/sh
# Author: Ingo Feinerer
# Generates the ModApte Split if applied to the Reuters21578 XML data set by removing unused items
# The remaining items are both training and test elements
if [ $# -eq 0 ]
then
    echo "Error: wrong number of arguments."
    echo "Usage: $0 <xml-file> <xml-file> ..."
    exit 1
fi
if ! [ -f genSplit ]
then
    make genSplit
fi
for filename in $*
do
    ./genSplit $filename "/LEWIS/REUTERS[@TOPICS='NO'] | /LEWIS/REUTERS[@TOPICS='BYPASS'] | /LEWIS/REUTERS[@LEWISSPLIT='NOT-USED']" > ${filename%.*}.ModApte.xml
done
