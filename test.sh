#!/bin/bash

GLADOS="./glados"
PASSED=0
FAILED=0

WELCOME_MSG="LISP interpreter Usage :
Write in the shell your code (e.g : (define two (+ 1 1)).
To quit the interpreter, write exit.
"

echo -e "Building GLaDOS..."
if ! make all; then
    echo -e "Build failed"
    exit 1
fi
echo -e "Build successful\n"

test_expr() {
    local desc="$1"
    local input="$2"
    local expected="$3"
    
    local actual=0
    actual=$(echo "$input" | $GLADOS)
    actual="${actual#"$WELCOME_MSG"}"
    
    if [ "$actual" = "$expected" ]; then
        echo -e "PASS: $desc"
        PASSED=$((PASSED + 1))
    else
        echo -e "FAIL: $desc"
        echo "  Input:    $input"
        echo "  Expected: $expected"
        echo "  Actual:   $actual"
        FAILED=$((FAILED + 1))
    fi
}

echo ""
echo "========================================"
echo -e "Results: $PASSED passed, $FAILED failed"
echo "========================================"

if [ $FAILED -gt 0 ]; then
    exit 1
fi
exit 0
