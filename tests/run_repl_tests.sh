

success_count=0
fail_count=0
failing_tests=()

cd ../repl
make clean
make cov
cd ../tests


for fn in repl_tests/*.lisp
do
    ok=false
    ../repl/repl_cov --terminate -s $fn | grep 'SUCCESS' &> /dev/null
    if [ $? == 0 ]; then
        ok=true
    fi


    if $ok; then
        success_count=$((success_count+1))
        echo "Test OK: $fn"
    else
        fail_count=$((fail_count+1))
        failing_tests+=("$fn")
        echo "Test failed: $fn"
    fi
done

echo Tests passed: $success_count
echo Tests failed: $fail_count


## Go to repl directory and collect the coverage data
cd ../repl
make clean_coverage ## being careful
gcovr
rm -rf repl_tests_coverage
cp -r coverage repl_tests_coverage
cd ../tests


