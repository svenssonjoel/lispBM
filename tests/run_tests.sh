#!/bin/bash

echo "BUILDING"

make clean
make

echo "PERFORMING TESTS:"

success_count=0
fail_count=0
failing_tests="failed tests: \n"
result=0

for exe in *.exe; do

    if [ "$exe" = "test_gensym.exe" ]; then
    	continue
    fi

    ./$exe

    result=$?

    echo "------------------------------------------------------------"
    if [ $result -eq 1 ]
    then
	success_count=$((success_count+1))
	echo $exe SUCCESS
    else
	
	fail_count=$((fail_count+1))
	echo $exe FAILED
    fi
    echo "------------------------------------------------------------"
done



for lisp in *.lisp; do
    ./test_lisp_code_cps $lisp

    result=$?

    echo "------------------------------------------------------------"
    echo HUGE_HEAP!
    if [ $result -eq 1 ]
    then
	success_count=$((success_count+1))
	echo $lisp SUCCESS
    else
	failing_tests="$failing_tests $lisp \n"
	fail_count=$((fail_count+1))
	echo $lisp FAILED
    fi
    echo "------------------------------------------------------------"
done


for lisp in *.lisp; do

    ./test_lisp_code_cps_mini_heap $lisp

    result=$?

    echo "------------------------------------------------------------"
    echo MINI_HEAP!
    if [ $result -eq 1 ]
    then
	success_count=$((success_count+1))
	echo $lisp SUCCESS
    else
	failing_tests="$failing_tests $lisp \n"
	fail_count=$((fail_count+1))
	echo $lisp FAILED
    fi
    echo "------------------------------------------------------------"
done

echo -e $failing_tests
echo Tests passed: $success_count
echo Tests failed: $fail_count
