#!/bin/bash

VM_BIN="./glados-vm"
TEST_FILES_DIR="tests/test-files"

PASSED=0
FAILED=0

echo -e "Building the vm..."
if ! make all; then
    echo -e "Build failed"
    exit 1
fi
echo -e "Build successful\n"

if [ -z "$VM_BIN" ]; then
    echo "Could not find glados-vm-exe"
    exit 1
fi

test_exit_code() {
    local desc="$1"
    local inputFile="$2"
    local expectedCode="$3"
    
    "$VM_BIN" "$inputFile" 2>/dev/null
    local actual=$?
    
    if [ "$actual" -eq "$expectedCode" ]; then
        echo -e "PASS: $desc"
        PASSED=$((PASSED + 1))
    else
        echo -e "FAIL: $desc"
        echo "  Input file: $inputFile"
        echo "  Expected exit code: $expectedCode"
        echo "  Actual exit code:   $actual"
        FAILED=$((FAILED + 1))
    fi
}

echo "=== Testing VM_BIN Instructions ==="
echo ""

echo "== Stack Instructions =="
test_exit_code "iconst - push integer constant" "$TEST_FILES_DIR/stack_iconst.bc" 42
test_exit_code "iload/istore - load and store local" "$TEST_FILES_DIR/stack_load_store.bc" 10
test_exit_code "dup - duplicate top of stack" "$TEST_FILES_DIR/stack_dup.bc" 10

echo "========================================"
echo -e "Results: $PASSED passed, $FAILED failed"
echo "========================================"

if [ $FAILED -gt 0 ]; then
    exit 1
fi
exit 0
