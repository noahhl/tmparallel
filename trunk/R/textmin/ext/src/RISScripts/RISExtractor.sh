#!/bin/sh
# Author: Ingo Feinerer
if [ $# -ne 2 ]
then
    echo "Error: wrong number of arguments."
    echo "Usage: $0 start stop"
    exit 1
fi

for i in `seq $1 $2`;
do
    wget "http://ris.bka.gv.at/taweb-cgi/taweb?x=d&o=d&d=VwGHT&i=$i&v=vwgh" -O $i.html
done    
