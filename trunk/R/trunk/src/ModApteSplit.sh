#!/bin/sh

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
    ./genSplit $filename "/LEWIS/REUTERS[@TOPICS='NO'] | /LEWIS/REUTERS[@TOPICS='BYPASS'] | /LEWIS/REUTERS[@LEWISSPLIT='NOT-USED']" > $filename.ModApte.xml
done
