#!/bin/env sh

dir=/home

rustc -O -o ${dir}/a.out ${dir}/Main.rs 2>${dir}/comp.txt 1>/dev/null
if test $? -ne 0  ; then
	exit 1
fi
${dir}/a.out < ${dir}/input.txt > ${dir}/output.txt
if test $? -ne 0  ; then
	exit 2
fi
exit 0
