
mkdir -p coverage
rm -f coverage/*

tc1="--json-add-tracefile tests_cov_32.json"
tc2="--json-add-tracefile tests_cov_64.json"
tc3="--json-add-tracefile ../repl/repl_tests_cov.json"
tc4="--json-add-tracefile ../repl/image_tests_cov.json"

gcovr --filter ../src --merge-mode-functions merge-use-line-max $tc1 $tc2 $tc3 $tc4 --html-details coverage/coverage.html
