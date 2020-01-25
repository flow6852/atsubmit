#!/bin/env bash

dir="cache"

main=Main.hs
src=${dir}/src/source.txt
input=${dir}/src/input.txt
output=${dir}/src/output.txt
out=${dir}/src/out.txt
comp=${dir}/src/comp.txt
timer=2

img=atjudge_hs

# dockerの操作
t=$(date +%Y%m%d%H%M%S)
docker create --name ${t} --pids-limit 10 --network "none" ${img} >/dev/null 2>&1
docker cp ${src} ${t}:/home/${main} >/dev/null 2>&1
docker cp ${input} ${t}:/home/input.txt >/dev/null 2>&1
docker start ${t} >/dev/null 2>&1
timeout ${timer} docker wait ${t} >/dev/null 2>&1
if [[ $? == 0 ]] ; then # Exe
	docker cp ${t}:/home/output.txt ${out}
	diff -B --strip-trailing-cr ${output} ${out}
	if [[ $? == 0 ]] ; then
		echo "AC"
	else
		echo "WA"
		echo "==== output ===="
		cat ${out}
		echo "==== sample ===="
		cat ${output}
		echo ""
	fi
elif [[ $? == 1 ]] ; then # compile error
	docker cp ${t}:/home/comp.txt ${comp}
	echo "CE"
	cat ${comp}
elif [[ $? == 2 ]] ; then # input error
	echo "WA (input wrror)"
else # TLE : status code is maybe 124
	echo "TLE"
fi
docker rm -f ${t} >/dev/null 2>&1
