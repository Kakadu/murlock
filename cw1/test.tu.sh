#!/bin/bash
N=10
test_formula () {
	path=$1
	count=$2
	res=$3
	sum=0	
	for i in `seq $count` #(( i=0; i<$count; i++ ))
	do
	   echo $path | ./murlock > .temp.log #/dev/null
	   eval `cat $res`
	   sum=`echo $DURATION + $sum | bc`
	done;
	avg=`echo "scale=10; $sum * 1000 / $count" | bc`
	eval `cat .temp.log | grep maxiters`
}
for file in `ls t*.in`
do
	read formula < $file
	num=${file%???}
        num=${num##*_}
	old_input="-dot off\n-dt normal\n-o ./last_res/$num\n-p0 $formula"
	konev_input="-dot off\n-dt normal\n-o ./last_res/$num\n-p4 $formula"
	test_formula "$old_input"   $N "./last_res/$num.old.info"
	avg1=`echo $avg | head -c 5`
	nodes1=$NODESCOUNT
	test_formula "$konev_input" $N "./last_res/$num.konev.info"
	avg2=`echo $avg | head -c 5`
	nodes2=$NODESCOUNT
	echo "$num & $avg1 & $nodes1 & $avg2 & $nodes2 \\\\\\"
done;

