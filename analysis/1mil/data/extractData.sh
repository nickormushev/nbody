#!/bin/bash

#gets the calculations from all the files
#for nProc in {1,2,4,8,16,32}; do
#    for i in {1,2,3}; do
#        cat test-${nProc}-1mil-${i}.txt |
#            grep Calculations |
#            cut -d ' ' -f 1,2 |
#            tr ' ' ',' |
#            awk -v nProc=${nProc} -v file=${i} '{print file","nProc","$0}' >> calculations1mil.csv
#    done
#done


#generate total time files
for nProc in {1,2,4,8,16,32}; do
    for i in {1,2,3}; do
        cat test-${nProc}-1mil-${i}.txt |
            grep Total |
            cut -d ' ' -f 1 |
            awk -v nProc=${nProc} -v file=${i} '{print file","nProc","$0}' >> totalTime1mil.csv
    done
done

