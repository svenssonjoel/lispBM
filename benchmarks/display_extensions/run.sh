#!/bin/bash
# Runs the display_extensions drawing-routine benchmarks (bench.lisp) and
# writes a CSV named with the current date/time and the LBM version into
# results/, so multiple runs on the same day don't clobber each other.
#
#
# Usage:
#   ./run.sh          benchmark only
#   ./run.sh --ppm    also dump one .ppm snapshot per shape/format into ppm/,
#                     for visually sanity-checking that the benchmark draws
#                     what it claims to (see bench.lisp)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPL_DIR="$SCRIPT_DIR/../../repl"
REPL_BIN="$REPL_DIR/repl"
RESULTS_DIR="$SCRIPT_DIR/results"
PPM_DIR="$SCRIPT_DIR/ppm"
MEM_SIZE=$((16*1024*1024))
HEAP_SIZE=$((200*1000))

render_ppm="nil"
if [ "$1" == "--ppm" ]; then
    render_ppm="t"
fi

echo "Building repl..."
make -C "$REPL_DIR" FEATURES="" >/dev/null

# Grab the version by running the repl
version=$("$REPL_BIN" --silent --terminate -e '(print (lbm-version))' \
    | head -1 | tr -d '()' | tr ' ' '.')
date_str=$(date +%Y-%m-%d_%H-%M-%S)

mkdir -p "$RESULTS_DIR"
[ "$render_ppm" == "t" ] && mkdir -p "$PPM_DIR"

csv_path="$RESULTS_DIR/bench_${date_str}_${version}.csv"

echo "LBM version: $version"
echo "Writing:     $csv_path"
[ "$render_ppm" == "t" ] && echo "PPM dumps:   $PPM_DIR/"

cd "$SCRIPT_DIR"
"$REPL_BIN" -H "$HEAP_SIZE" -M "$MEM_SIZE" --silent --terminate \
    -e "(define csv-filename \"$csv_path\")" \
    -e "(define render-ppm $render_ppm)" \
    -e '(eval-program (read-program (load-file (f-open "bench.lisp" "r"))))'

echo "Done: $csv_path"

if [ "$1" == "--ppm" ]; then
    feh ./ppm/*.ppm
fi

