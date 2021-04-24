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


for prg in "test_lisp_code_cps" "test_lisp_code_cps_nc" "test_lisp_code_cps_nc -e"; do 
    for lisp in *.lisp; do
	for arg in "" "-h 8388608" "-h 8192" "-g -h 8388608" "-g -h 8192" "-c -h 8192" "-c -h 8388608" "-c -g -h 8192" "-c -g -h 8388608"; do 
	    ./$prg $arg $lisp
	    
	    result=$?
	    
	    echo "------------------------------------------------------------"
	    echo $arg
	    if [ $result -eq 1 ]
	    then
		success_count=$((success_count+1))
		echo $lisp SUCCESS
	    else
		failing_tests="$failing_tests $prg $arg $lisp \n"
		fail_count=$((fail_count+1))
		echo $lisp FAILED
	    fi
	    echo "------------------------------------------------------------"
	done
    done 
done

echo -e $failing_tests
echo Tests passed: $success_count
echo Tests failed: $fail_count

if [ $fail_count -gt 0 ]
then
    exit 1
fi
