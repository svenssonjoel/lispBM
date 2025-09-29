
../repl/repl_cov --terminate -s $1

for ((i=1; i<=10; i++)); do
    ../repl/repl_cov --silent --terminate --load_image=image.lbm -e "(main)"
done


