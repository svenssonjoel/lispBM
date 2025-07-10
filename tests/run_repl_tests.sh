#!/bin/bash

success_count=0
fail_count=0
failing_tests=()

timeout_val=10

cd ../repl
make clean
make cov
cd ../tests

date=$(date +"%Y-%m-%d_%H-%M")
logfile="log_repl_tests_${date}.log"

for fn in repl_tests/*.lisp
do
    ok=false
    fail_timeout=false;
    timeout $timeout_val ../repl/repl_cov --terminate -s $fn | grep 'SUCCESS' &> /dev/null
    res=$?
    if [ $res == 124 ]; then
        fail_timeout=true;
    else
        if [ $res == 0 ]; then
            ok=true
        fi
    fi

    if $ok; then
        success_count=$((success_count+1))
        echo "Test OK: $fn"
    else
        fail_count=$((fail_count+1))
        failing_tests+=("$fn")
        if [ $fail_timeout == true ]; then
            echo "Timeout: $fn"
            echo "Timeout: $fn" >> $logfile
        else
            echo "FAIL: $fn"
            echo "FAIL: $fn" >> $logfile
        fi
    fi
done

echo Tests passed: $success_count
echo Tests failed: $fail_count


## Go to repl directory and collect the coverage data
cd ../repl
rm -f repl_tests_cov.json
gcovr --gcov-ignore-parse-errors --json repl_tests_cov.json
cd ../tests

