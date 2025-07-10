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
logfile="log_image_tests_${date}.log"

if [ -n "$1" ]; then
   logfile=$1
fi

for fn in image_tests/*.lisp
do
    fail_timeout=false;
    ok=false
    timeout $timeout_val ../repl/repl_cov --silent --terminate -s $fn &> /dev/null
    if [ $? == 124 ]; then
        fail_timeout=true;
    else
        timeout $timeout_val ../repl/repl_cov --silent --terminate --load_image=image.lbm -e "(main)" | grep 'SUCCESS' &> /dev/null
        res=$?
        if [ $res == 0 ]; then
            ok=true
        elif [ $res == 124 ]; then
            fail_timeout=true
        fi
    fi

    if $ok; then
        success_count=$((success_count+1))
        echo "Test OK: $fn"
    else
        fail_count=$((fail_count+1))
        failing_tests+=("$fn")

        if [ $fail_timeout == true ]; then ## not a real boolean...
            echo "TIMEOUT: $fn"
            echo "TIMEOUT:  $fn" >> logfile
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
rm -f image_tests_cov.json
gcovr --gcov-ignore-parse-errors --json image_tests_cov.json
cd ../tests
