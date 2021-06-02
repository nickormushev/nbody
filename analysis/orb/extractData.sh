#!/bin/bash

#gets the calculations from all the files
i=1
nProc=16
cat test-${nProc}-2thou-${i}-NoORB.txt |
    grep Calculations |
    cut -d ' ' -f 1,2 |
    tr ' ' ',' |
    awk -v nProc=${nProc} -v file=${i} '{print file","nProc","$0}' >> calculations200thouNoORB.csv

#generate total time files
#for nProc in {1,2,4,8,16,32}; do
#    for i in {1,2,3}; do
#        cat test-${nProc}-2thou-${i}.txt |
#            grep Total |
#            cut -d ' ' -f 1 |
#            awk -v nProc=${nProc} -v file=${i} '{print file","nProc","$0}' >> totalTime200thou.csv
#    done
#done
