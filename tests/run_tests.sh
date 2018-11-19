#!/bin/bash


echo "PERFORMING TESTS:"

success_count=0
fail_count=0

result=0

for exe in *.exe; do
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
    ./test_lisp_code $lisp

    result=$?
    
    echo "------------------------------------------------------------"
    if [ $result -eq 1 ]
    then
	success_count=$((success_count+1))
	echo $lisp SUCCESS
    else
	fail_count=$((fail_count+1))
	echo $lisp FAILED
    fi
    echo "------------------------------------------------------------"
done    

echo Tests passed: $success_count
echo Tests failed: $fail_count
