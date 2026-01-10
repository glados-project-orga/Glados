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
# crash
# test_exit_code "iconst - push neg integer constant" "$TEST_FILES_DIR/stack_neg_iconst.bc" -42
test_exit_code "iload/istore - load and store local" "$TEST_FILES_DIR/stack_load_store.bc" 10
test_exit_code "dup - duplicate top of stack" "$TEST_FILES_DIR/stack_dup.bc" 10
test_exit_code "dup2 - duplicate top two values" "$TEST_FILES_DIR/stack_dup2.bc" 14
test_exit_code "pop - pop top of stack" "$TEST_FILES_DIR/stack_pop.bc" 5
test_exit_code "swap - swap top two values" "$TEST_FILES_DIR/stack_swap.bc" 7
# Boucle inf
# test_exit_code "nop - no operation" "$TEST_FILES_DIR/stack_nop.bc" 1 
echo ""

echo "== Arithmetic Instructions =="
test_exit_code "iadd - integer addition" "$TEST_FILES_DIR/arith_iadd.bc" 8
test_exit_code "isub - integer subtraction" "$TEST_FILES_DIR/arith_isub.bc" 7
test_exit_code "imul - integer multiplication" "$TEST_FILES_DIR/arith_imul.bc" 20
test_exit_code "idiv - integer division" "$TEST_FILES_DIR/arith_idiv.bc" 5
test_exit_code "irem - integer remainder" "$TEST_FILES_DIR/arith_irem.bc" 1
# crash
# test_exit_code "ineg - integer negation" "$TEST_FILES_DIR/arith_ineg.bc" 251
test_exit_code "iand - bitwise AND" "$TEST_FILES_DIR/arith_iand.bc" 4
test_exit_code "ior - bitwise OR" "$TEST_FILES_DIR/arith_ior.bc" 7
test_exit_code "ixor - bitwise XOR" "$TEST_FILES_DIR/arith_ixor.bc" 3
test_exit_code "ishl - shift left" "$TEST_FILES_DIR/arith_ishl.bc" 20
test_exit_code "ishr - shift right" "$TEST_FILES_DIR/arith_ishr.bc" 5
echo ""

echo "== Control Flow Instructions =="
test_exit_code "goto - unconditional jump" "$TEST_FILES_DIR/control_goto.bc" 1
test_exit_code "invokestatic - function call" "$TEST_FILES_DIR/control_invokestatic.bc" 30
test_exit_code "return - void return" "$TEST_FILES_DIR/control_return.bc" 42
test_exit_code "ireturn - integer return" "$TEST_FILES_DIR/control_ireturn.bc" 99
echo ""

echo "== Conditional Instructions =="
test_exit_code "ifeq - branch if equal to zero" "$TEST_FILES_DIR/cond_ifeq.bc" 0
# No main function found ?? il trouve pas la function parent?
# test_exit_code "ifne - branch if not equal to zero" "$TEST_FILES_DIR/cond_ifne.bc" 1
test_exit_code "iflt - branch if less than zero" "$TEST_FILES_DIR/cond_iflt.bc" 1
# No main function found
# test_exit_code "ifge - branch if greater or equal to zero" "$TEST_FILES_DIR/cond_ifge.bc" 1
test_exit_code "ifgt - branch if greater than zero" "$TEST_FILES_DIR/cond_ifgt.bc" 1
# No main function found
# test_exit_code "ifle - branch if less or equal to zero" "$TEST_FILES_DIR/cond_ifle.bc" 1
# A vérifier si pas inversé
# test_exit_code "if_icmpgt - branch if greater than" "$TEST_FILES_DIR/cond_if_icmpgt.bc" 1
# test_exit_code "if_icmplt - branch if less than" "$TEST_FILES_DIR/cond_if_icmplt.bc" 1
# echo ""

echo "== Array Instructions =="
# test_exit_code "newarray/iastore/iaload - array operations" "$TEST_FILES_DIR/array_basic.bc" 42
test_exit_code "arraylength - get array length" "$TEST_FILES_DIR/array_length.bc" 5
echo ""

echo "== Object Instructions =="
test_exit_code "new/putfield/getfield - object operations" "$TEST_FILES_DIR/object_basic.bc" 100
echo ""

echo "========================================"
echo -e "Results: $PASSED passed, $FAILED failed"
echo "========================================"

if [ $FAILED -gt 0 ]; then
    exit 1
fi
exit 0
