#!/bin/env sh

cd /home/atsubmit_rs
cargo build --release --offline
# cp ${workdir}/Main.txt ${workdir}/src/main.rs
# cargo build --release --offline 2>${workdir}/../comp.txt 1>/dev/null
# if test $? -ne 0  ; then
# 	exit 1
# fi
# cd ${dir}
# cp atsubmit_rs/target/release/atsubmit_rs ./a.out
# timeout 2 ./a.out < ${workdir}/../input.txt > ${workdir}/../output.txt
# status=$?
# if test ${status} -eq 124; then
#     exit 2
# elif test ${status} -ne 0; then
#     exit 3
# fi
# exit 0
