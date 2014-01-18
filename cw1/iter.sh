#!/bin/bash
# script for testing autoprover
lst=`ls t*.in` # files with tests
destdir="last_res/" # destionation to place result files
slashes='\\\\'
for i in $lst 
do 
	num=${i%???}
	num=${num##*_}  
	ans=`./murlock < $i`	
	prefix=${i%"in"}
	# get result for normal method
	eval `cat $destdir$prefix"old.info"`
	dur1=$DURATION
	count1=$NODESCOUNT
	# get result for konev method
	eval `cat $destdir$prefix"konev.info"`
	dur2=$DURATION
	count2=$NODESCOUNT
	echo "$num & $dur1 & $count1 & $dur2 & $count2" $slashes 
done;
