#!/bin/bash

echo "*** Making tests"
make clean
make

file="/dev/ttyUSB0"

success_count=0
fail_count=0
failing_tests=()

echo "*** Running tests"
for f in *.plisp; do
    echo "*** Performing test $f"
    vesc_tool --uploadLisp $f &

    while read -r line;
    do
        if  [[ "$line" == "CHECK: SUCCESS" ]] ;
        then
            success_count=$((success_count+1))
            echo "Test successful!"
        fi
        if  [[ "$line" == "CHECK: FAILURE" ]] ;
        then
            fail_count=$((fail_count+1))
            failing_tests+=("$f")
            echo "Test failed!"
        fi
        if  [[ "$line" == "TEST END"* ]] ;
        then
            echo "Test finished"
            break
        fi
    done <"$file"
done

echo ""
echo "Failed tests"
echo "=================================================="
for ((i = 0; i < ${#failing_tests[@]}; i ++ ))
do
    echo "(FAILURE)" ${failing_tests[$i]}
done

echo ""
echo "Stats"
echo "=================================================="
echo Tests passed: $success_count
echo Tests failed: $fail_count
