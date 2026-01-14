#!/bin/bash

# Auto-Compile Compiler if needed
make -C build -j8
if [ $? -ne 0 ]; then
    echo "Compiler Build Failed"
    exit 1
fi

TEST_FILE=$1
echo "Testing $TEST_FILE..."
./build/src/tokac $TEST_FILE > ${TEST_FILE%.tk}.ll
if [ $? -eq 0 ]; then
    echo "Compilation Succeeded"
    lli ${TEST_FILE%.tk}.ll
    if [ $? -eq 0 ]; then
        echo "Execution Succeeded"
        exit 0
    else
        echo "Execution Failed"
        exit 1
    fi
else
    echo "Compilation Failed"
    exit 1
fi
