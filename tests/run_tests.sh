#!/bin/bash

echo "BUILDING"

make clean
make 

echo "PERFORMING TESTS:"

success_count=0
fail_count=0

result=0

for exe in *.exe; do

    #if [ "$exe" = "test_gensym.exe" ]; then
    #	continue
    #fi
    
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

for lisp in *.lisp; do
    ./test_lisp_code_cps $lisp

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

for lisp in *.lisp; do

    if [ "$lisp" = "test_fib_0.lisp" ]; then
	continue
    fi

    if [ "$lisp" = "test_fib_1.lisp" ]; then
	continue
    fi

    ./test_lisp_code_mini_heap $lisp
    
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


for lisp in *.lisp; do
    ./test_lisp_code_cps_mini_heap $lisp

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
