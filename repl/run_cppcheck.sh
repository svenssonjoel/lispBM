#!/bin/bash



date=$(date +"%Y-%m-%d_%H-%M")

logfile32="cppcheck_log_32bit_${date}.log"
logfile64="cppcheck_log_64bit_${date}.log"

if [ -n "$1" ] && [ -n "$2" ]; then
    logfile32="$1"
    logfile64="$2"
fi

# 32bit run 
make clean
bear make

cppcheck --project=compile_commands.json --enable=all --suppress=unusedFunction &> $logfile32


#64bit run
make clean
bear make all64

cppcheck --project=compile_commands.json --enable=all --suppress=unusedFunction &> $logfile64




