#!/bin/bash
min () {
	if [ $1 -gt $2 ]; then 
		echo $2 
	else 
		echo $1 
	fi;
}
simplify () {
	s1=$1
	s2=$2
	len1=${#s1}
	len2=${#s2}
	len=`min $len1 $len2`
	echo "len = $len"
	c=0
	for i in `seq $len`
	do
		echo "i = $i; s1 = $s1;" 
		ch1=`echo $s1 | head -c $i | tail -c 1`
		ch2=`echo $s2 | head -c $i | tail -c 1`
		echo "ch1 = $ch1"
		echo "ch2 = $ch2"
		ch1="$ch1"
		ch2="$ch2"
		if [ `echo $ch1` == "$ch2" ]; then #-a "$ch1" != "." -a "$ch1" != '0' ]; then
			c=`expr 1 + $c`
		fi;
		if [ $c -eq 2]; then break; fi;
	done;
	echo $ans1
	echo $ans2
}
test_formula () {
	path=$1
	count=$2
	res=$3
	sum=0	
	for i in `seq $count` #(( i=0; i<$count; i++ ))
	do
#	   echo "i = $i"
	   echo $path | ./murlock > .test.wtu.log
	   eval `cat $res`
	   sum=`echo $DURATION + $sum | bc`
	done;
	targetsfile=`cat .test.wtu.log | grep maxiters`
#	echo $targetsfile
	eval `echo $targetsfile`
	avg=`echo "scale=10; $sum * 1000 / $count" | bc`	
#	echo "Last targets count = $maxiters"
#	echo "Average time = $avg"
}
N=10
#indexes=( 1 2 3 4 5 6 7 8  9 10 11 12 13 14 15 16 17 18 19 20 )
#13 - ban
for i in  01 02 03 04 05 06 07 08 09 10 11 12 14 15 16 17 18 19 20
do
  for file in `ls t$i*.in`; do
	read formula < $file
#	echo "formula = '$formula'"
	num=${file%???}
        num=${num##*_}
	old_input="-dot off\n-dt normal\n-tu never\n-o ./last_res/$num\n-p0 $formula"
	konev_input="-dot off\n-dt normal\n-tu never\n-o ./last_res/$num\n-p4 $formula"
	test_formula "$old_input"   $N "./last_res/$num.old.info"
	avg1=`echo $avg | head -c 5`
	nodes1=$NODESCOUNT
	test_formula "$konev_input" $N "./last_res/$num.konev.info"
	avg2=`echo $avg | head -c 5`
	nodes2=$NODESCOUNT
	echo "$num & $avg1 & $nodes1 & $avg2 & $nodes2 & $maxiters \\\\\\"
	#simplify $avg1 $avg2
#	echo $ans1
#	echo $ans2
  done;
done;

