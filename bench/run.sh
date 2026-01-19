#!/bin/bash
set -e

echo "=== Compiling Toka... ==="
build/src/tokac bench/fib.tk > bench/fib.ll

#echo "=== Running Toka (JIT) ==="
#time lli bench/fib.ll

echo ""
echo "=== Compiling Toka (Native) ==="
SDK_PATH=$(xcrun --show-sdk-path)
/usr/bin/clang -x ir bench/fib.ll -O3 -o bench/fib_toka_native.bin -isysroot $SDK_PATH

echo "=== Running Toka (Native) ==="
time ./bench/fib_toka_native.bin

c++ -O3 -o bench/fib_cpp.bin bench/fib.cpp
echo ""
echo "=== Running C++ (Native -O3) ==="
time ./bench/fib_cpp.bin
