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

echo "=== Builtins ==="
echo "== Arithmetic Operations =="
test_expr "Addition no args" "(+)" "0"
test_expr "Addition 1 arg" "(+ 5)" "5"
test_expr "Addition" "(+ 1 2)" "3"
test_expr "Addition multiple args" "(+ 1 2 3 4)" "10"
test_expr "Subtraction no args" "(-)" "Exception: incorrect argument count in call (-)"
test_expr "Subtraction 1 arg" "(- 5)" "-5"
test_expr "Subtraction" "(- 1 2)" "-1"
test_expr "Subtraction multiple args" "(- 1 2 3 4)" "-8"
test_expr "multiplication no args" "(*)" "1"
test_expr "multiplication 1 arg" "(* 5)" "5"
test_expr "multiplication" "(* 4 2)" "8"
test_expr "multiplication multiple args" "(* 1 2 3 4)" "24"
test_expr "division wrong args number" "(div)" "Exception: incorrect argument count in call (div)"
test_expr "division" "(div 4 2)" "2"
test_expr "modulo wrong args number" "(mod)" "Exception: incorrect argument count in call (mod)"
test_expr "modulo" "(mod 3 2)" "1"
test_expr "Nested Arithmetic" "(+ (* 2 3) (- 10 5))" "11"

echo ""
echo "== Conditionals =="
test_expr "Lower no args" "(<)" "Exception: incorrect argument count in call (<)"
test_expr "Lower 1 arg" "(< 1)" "#t"
test_expr "Lower True" "(< 1 2)" "#t"
test_expr "Lower False" "(< 2 1)" "#f"
test_expr "Lower multiple args True" "(< 1 2 3 4)" "#t"
test_expr "Lower multiple args False" "(< 1 2 2 3)" "#f"
test_expr "Upper no args" "(>)" "Exception: incorrect argument count in call (>)"
test_expr "Upper 1 arg" "(> 1)" "#t"
test_expr "Upper True" "(> 2 1)" "#t"
test_expr "Upper False" "(> 1 2)" "#f"
test_expr "Upper multiple args True" "(> 4 3 2 1)" "#t"
test_expr "Upper multiple args False" "(> 4 3 3 2)" "#f"
test_expr "Equality true" "(eq? 1 1)" "#t"
test_expr "Equality false" "(eq? 1 2)" "#f"
test_expr "Equality true" "(eq? 1 1)" "#t"
test_expr "Equality false" "(eq? 1 2)" "#f"
test_expr "If true branch" "(if #t 1 2)" "1"
test_expr "If false branch" "(if #f 1 2)" "2"
test_expr "If with comparison" "(if (< 1 2) 10 20)" "10"

echo "=== Variables and Functions ==="
test_expr "Define and use variable" "(define x 42)
x" "42"
test_expr "Define and call function" "(define (square x) (* x x))
(square 5)" "25"
test_expr "Lambda expression" "((lambda (x) (* x x)) 4)" "16"

echo ""
echo "=== Recursive Functions ==="
test_expr "Factorial" "(define (fact x) (if (eq? x 1) 1 (* x (fact (- x 1)))))
(fact 5)" "120"
test_expr "Factorial" "(define (fact x) (if (eq? x 1) 1 (* x (fact (- x 1)))))
(fact 4)" "24"
echo ""

echo "========================================"
echo -e "Results: $PASSED passed, $FAILED failed"
echo "========================================"

if [ $FAILED -gt 0 ]; then
    exit 1
fi
exit 0
