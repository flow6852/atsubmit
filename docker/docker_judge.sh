#!/bin/env bash

dir=~/.cache/atsubmit

img=${1}
main=${2}
src=${dir}/src/source.txt
input=${dir}/src/input.txt
output=${dir}/src/output.txt
out=${dir}/src/outres.txt
comp=${dir}/src/comp.txt
timer=2

# dockerの操作
echo ${img} ${main}
t=$(date +%Y%m%d%H%M%S)
docker create --name ${t} --pids-limit 10 --network "none" ${img} >/dev/null 2>&1
docker cp ${src} ${t}:/home/${main} >/dev/null 2>&1
docker cp ${input} ${t}:/home/input.txt >/dev/null 2>&1
docker start ${t} >/dev/null 2>&1
timeout ${timer} docker wait ${t} >/dev/null 2>&1
timecheck=$?
check=$(docker inspect ${t} --format='{{.State.ExitCode}}')
docker cp ${t}:/home/output.txt ${out} >/dev/null 2>&1
docker cp ${t}:/home/comp.txt ${comp} >/dev/null 2>&1
docker rm -f ${t} >/dev/null 2>&1
if [ ${check} = ${timecheck} -o ${timecheck} = 0 ] ; then exit ${check}; else exit ${timecheck}; fi
