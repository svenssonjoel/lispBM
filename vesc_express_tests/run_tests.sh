#!/bin/bash

echo "*** Making tests"
make clean
make

#file="/dev/ttyUSB0"

success_count=0
fail_count=0
failing_tests=()

vesc_tool="vesc_tool_6.05"
vesc_express="/dev/ttyACM0"

in_use=$(ls -l /proc/[0-9]*/fd/* 2> /dev/null |grep /dev/ | grep $vesc_express | grep -o 'proc/.*' | cut -d "/" -f2  2> /dev/null)

if [ -z $in_use ]
then
    echo "TTY available: " $vesc_express
else
    echo "TTY unavailable: " $vesc_express
    exit 0
fi


echo "*** Running tests"
exec <>3
for f in *.plisp; do
    if [ -p "test_pipe" ]; then
        rm test_pipe
    fi
    echo "*** Performing test $f"
    mkfifo test_pipe
    
    $vesc_tool --bridgeAppData --vescPort $vesc_express --uploadLisp $f >>  test_pipe &
    vesc_pid=$!

    ok=false
    
    while read -r line
    do
        echo "ECHO LINE: " $line
        
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
            ok=true
            break
        fi
    done  < test_pipe

    if [ $ok ]
    then
        echo "TEST FINISHED"
    else
        echo "NO OUTPUT AVAILABLE"
    fi
    
    echo "VESC_PID: " $vesc_pid
    
    proc=$(ps --pid $vesc_pid -o command=)
    proc=$(echo $proc | cut -d " " -f 1)
    echo "Trying to kill: " $proc
    if [[ $proc == $vesc_tool ]]
    then
        kill -9 $vesc_pid 
        if [ $? -eq 0 ] 
        then
            echo "KILL: VESC_TOOL"
        else
            echo "Error: Could not kill VESC_TOOL"
        fi
    else
        echo "VESC_TOOL died by itself"
    fi    
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
