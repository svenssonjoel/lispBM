#!/bin/bash

echo "BUILDING"

make clean
make

echo "PERFORMING TESTS:"

success_count=0
fail_count=0
excluded_count=0
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
    for arg in "" "-h 8388608" "-h 8192" "-g -h 8388608" "-g -h 8192" "-c -h 8192" "-c -h 8388608" "-c -g -h 8192" "-c -g -h 8388608"; do
	for lisp in *.lisp; do

	    # exclude
	    if [ "$prg" != "test_lisp_code_cps" ]; then
		if [ "$lisp" = "test_match_0.lisp" ] ||
		       [ "$lisp" = "test_match_1.lisp" ] ||
		       [ "$lisp" = "test_match_2.lisp" ] ||
		       [ "$lisp" = "test_match_3.lisp" ] ||
		       [ "$lisp" = "test_match_4.lisp" ] ||
                       [ "$lisp" = "test_fib_3.lisp" ]; then
		    excluded_count=$((excluded_count+1))
		    continue
		fi
	    fi


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
echo Tests excluded: $excluded_count

if [ $fail_count -gt 0 ]
then
    exit 1
fi
