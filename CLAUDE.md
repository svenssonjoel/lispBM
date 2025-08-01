# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

LispBM is an embeddable Lisp/Scheme-like programming language designed for microcontrollers and embedded systems. It features:
- Support for both 32-bit and 64-bit platforms
- Concurrent programming with message passing and process monitoring (inspired by Erlang)
- Multiple runtime environments: ChibiOS, FreeRTOS, ZephyrOS, bare-metal, and Linux
- Extensions system for integrating C functions
- Garbage collection with multiple strategies

## Build System

### Library Build (Root Makefile)
- **Default build**: `make` (builds 32-bit Linux library)
- **64-bit build**: `make PLATFORM=linux-x64`
- **Cross-compilation**: Use `PLATFORM` variable (stm32f4, nrf52840, zynq, pi, macos-arm64)
- **Debug build**: `make debug`
- **Clean**: `make clean`
- **Test**: `make test` (runs tests/run_tests.sh)

Output: `build/<platform>/liblispbm.a` static library

### REPL Build (repl/ directory)
- **32-bit REPL**: `make` or `make all`
- **64-bit REPL**: `make all64`
- **With SDL support**: `make sdl` or `make sdl64`
- **Coverage build**: `make cov`
- **Debug build**: `make debug`
- **Install**: `make install` (installs to ~/.local/bin/lbm)

Dependencies: libreadline, libhistory, libpng, optionally SDL2

## Testing

### Main Test Suite
Located in `tests/` directory:
- **All tests**: `./run_tests.sh` (comprehensive 32-bit test suite)
- **64-bit tests**: `./run_tests64.sh`
- **Coverage tests**: `./run_tests_cov.sh`
- **GC tests**: `./run_tests_gc.sh`
- **Reverse GC tests**: `./run_tests_gc_rev.sh`
- **Time quota tests**: `./run_tests_time.sh` and `./run_tests64_time.sh`
- **Image tests**: `./run_image_tests.sh`
- **REPL tests**: `./run_repl_tests.sh`

### Test Binaries
Built in `tests/` via Makefile:
- `test_lisp_code_cps` - Main test runner
- `test_lisp_code_cps_64` - 64-bit version
- `test_lisp_code_cps_gc` - With aggressive GC
- `test_lisp_code_cps_revgc` - With reverse GC
- `test_lisp_code_cps_cov` - Coverage instrumented

### Test Structure
- `tests/tests/` - Unit test files (.lisp)
- `tests/repl_tests/` - REPL-specific tests
- `tests/image_tests/` - Heap image serialization tests
- Test scripts automatically build required binaries before running

### Edge Case Tests
The REPL test suite includes comprehensive edge case tests for fundamental operations:
- **test_arithmetic_edge_cases.lisp** - Tests +, -, *, /, mod with wrong types and edge cases
- **test_comparison_edge_cases.lisp** - Tests =, !=, <, >, <=, >= with invalid arguments
- **test_list_operations_edge_cases.lisp** - Tests cons, car, cdr, list, append, length, ix edge cases
- **test_type_operations_edge_cases.lisp** - Tests type-of, list?, number?, conversions with edge cases
- **test_integer_division_float_args.lisp** - Tests integer division // with floating-point arguments
- **test_random_extensions_edge_cases.lisp** - Tests seed and random functions with invalid inputs
- **test_runtime_extensions_edge_cases.lisp** - Tests runtime introspection functions with edge cases

These tests ensure robust error handling and predictable behavior for embedded applications.

### Test Output Conventions
All test suites should print **"SUCCESS"** when all tests pass:
- **C unit tests** (`tests/c_unit/`) - Each test executable should print "SUCCESS" on completion
- **REPL tests** (`tests/repl_tests/`) - Each `.lisp` test file should print "SUCCESS" when passing
- **Image tests** (`tests/image_tests/`) - Should print "SUCCESS" when serialization/deserialization works
- **SDL tests** (`tests/sdl_tests/`) - Should print "SUCCESS" when graphics operations complete

### C Unit Test Development
When creating C unit tests in `tests/c_unit/`, use the standard initialization pattern:

```c
#define _GNU_SOURCE // MAP_ANON
#define _POSIX_C_SOURCE 200809L // nanosleep?
#include <stdlib.h>
#include <stdio.h>
// ... other standard includes

#include "lispbm.h"
#include "init/start_lispbm.c"

static int test_init(void) {
  return start_lispbm_for_tests();
}
```

The `start_lispbm_for_tests()` function:
- Handles complete LispBM system initialization
- Sets up heap, memory management, and all subsystems
- Provides a clean environment for testing LispBM C API functions
- Should be called at the beginning of each test function that requires LispBM
- Automatically manages cleanup and reinitialization between tests

**Important**: Use `kill_eval_after_tests()` at the end of C unit tests that start threads (such as evaluation threads) which are not explicitly killed or waited for during the test. This function ensures proper cleanup by:
- Killing the evaluator thread before test function scope ends
- Preventing use-after-free errors when evaluator threads access test function local variables
- Ensuring clean test isolation and preventing state leakage between tests

Example usage:
```c
int test_something_with_threads() {
  if (!test_init()) return 0;
  
  // Test code that may start evaluator threads
  // ...
  
  kill_eval_after_tests(); // Clean up before function returns
  return 1;
}
```

## Architecture

### Core Components
- **Evaluator** (`src/eval_cps.c`): Continuation-passing style evaluator
- **Heap** (`src/heap.c`): Memory management and garbage collection
- **Symbols** (`src/symrepr.c`): Symbol representation and interning
- **Parser** (`src/tokpar.c`): Tokenizer and parser
- **Environment** (`src/env.c`): Variable binding management
- **Extensions** (`src/extensions.c`): C function integration system

### Extensions System
Located in `src/extensions/`:
- Array operations, display functions, math, string manipulation
- Runtime system functions, TTF font rendering
- Custom extensions can be added via `lbm_add_extension()`

### Platform Support
- `platform/` directory contains platform-specific code
- `platform/linux/` for POSIX systems
- `platform/chibios/`, `platform/freertos/`, `platform/zephyr/` for RTOSes

### Memory Model
- Fixed-size heap of cons cells
- Separate memory pools for different object types
- Multiple GC strategies: copying GC, reverse pointer GC
- Configurable heap sizes and memory pools

## Key Features for Development

### Configuration Flags
- `DLBM64`: Enable 64-bit support
- `DLBM_ALWAYS_GC`: Force GC on every allocation (testing)
- `DLBM_USE_GC_PTR_REV`: Use reverse pointer GC
- `DFULL_RTS_LIB`: Enable full runtime library
- `DVISUALIZE_HEAP`: Enable heap visualization

### Important Files
- `include/lispbm.h`: Main API header
- `lispbm.mk`: Build system include file
- `doc/lbmref.md`: Language reference documentation
- `repl/examples/`: Example LispBM programs

### LispBM Language Features

#### Error Handling
- **`trap` function**: Returns a list with error handling results
  - Success: `'(exit-ok value)` where `value` is the result
  - Error: `'(exit-error error-symbol)` where `error-symbol` is the specific error type
  - Common error symbols: `eval_error`, `type_error`, `division_by_zero`, `out_of_memory`, etc.
  - Example: `(trap (sin "string"))` returns `'(exit-error eval_error)`
  - **Important**: `trap` always returns either `'(exit-ok something)` or `'(exit-error something)`
  - Some functions return error symbols directly (like `'type-symbol`) without needing `trap`

#### Expression Sequences
- **`progn`**: Traditional syntax for evaluating multiple expressions in sequence
  - `(progn expr1 expr2 expr3)` evaluates all expressions and returns the last value
  - Allows context switches between expressions (concurrent processes can interleave)
- **Brace syntax `{}`**: Alternative syntax for `progn`
  - `{ expr1 expr2 expr3 }` is equivalent to `(progn expr1 expr2 expr3)`
  - More concise and readable than traditional `progn`
- **`atomic`**: Like `progn` but prevents context switches
  - `(atomic expr1 expr2 expr3)` evaluates expressions atomically
  - Disallows context switches between expressions (critical sections)
  - Only 3 constructs in LispBM allow expression sequences: `progn`, `{}`, and `atomic`

#### Data Types
- **Arrays**: Two types available
  - Byte arrays: `[1 2 3]` syntax
  - Lisp arrays: `[| 1 2 3 |]` syntax
- **Variable Assignment**: Use `setq` instead of `set!`
- **Lists**: Standard Lisp lists with `'(...)` syntax
- **Type names**: Use correct LispBM type names in tests
  - `'type-i` for integers, `'type-u` for unsigned integers
  - `'type-array` for strings, `'type-list` for lists
  - `'type-symbol` for symbols

#### Function Behaviors
- **Single argument arithmetic**: `(+ 5)` returns `5`, `(- 5)` returns `-5`, `(* 7)` returns `7`
- **Single argument comparisons**: `(= 5)` and `(<= 5)` return `t`, others return `nil`
- **Random functions**: `random` returns unsigned integers (`type-u`), ignores all arguments
- **Integer division**: `//` properly converts floating-point arguments to integer results
- **Type conversions**: `to-i` and `to-float` return `0`/`0.0f32` for invalid types (no errors)
- **Runtime functions**: Most ignore arguments; `lbm-heap-state` returns `nil` for invalid symbols

## Code Style Guidelines

### C Code Indentation Style
When writing or modifying C code in this project, use the following indentation style (consistent with eval_cps.c):

- **2-space indentation** for all code blocks (no tabs)
- **Consistent spacing** throughout all nesting levels
- **Compact style** suitable for deeply nested continuation-passing style code

**Examples:**
```c
static void function_name(params) {
  if (condition) {
    while (loop_condition) {
      if (nested_condition) {
        statement;
      }
    }
  }
}

switch (value) {
case CONSTANT:
  statement;
  break;
case OTHER:
  statement;
  break;
default:
  statement;
}
```

### Common Development Workflow
1. Modify core code in `src/`
2. Build library: `make clean && make`
3. Test changes: `cd tests && ./run_tests.sh`
4. For REPL testing: `cd repl && make && ./repl`
5. For coverage: `cd tests && ./run_tests_cov.sh`