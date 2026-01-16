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

test_output() {
    local desc="$1"
    local inputFile="$2"
    local expectedOutput="$3"
    
    local actualOutput
    actualOutput=$("$VM_BIN" "$inputFile" 2>/dev/null)
    
    if [ "$actualOutput" = "$expectedOutput" ]; then
        echo -e "PASS: $desc"
        PASSED=$((PASSED + 1))
    else
        echo -e "FAIL: $desc"
        echo "  Input file: $inputFile"
        echo "  Expected output: '$expectedOutput'"
        echo "  Actual output:   '$actualOutput'"
        FAILED=$((FAILED + 1))
    fi
}

echo "=== Testing VM Instructions ==="
echo ""

echo "== Stack Instructions =="
test_exit_code "iconst - push integer constant" "$TEST_FILES_DIR/stack_iconst.bc" 42
test_exit_code "iload/istore - load and store local" "$TEST_FILES_DIR/stack_load_store.bc" 10
test_exit_code "fload/fstore - load and store float local" "$TEST_FILES_DIR/stack_fload_fstore.bc" 42
test_exit_code "lload/lstore - load and store long local" "$TEST_FILES_DIR/stack_lload_lstore.bc" 42
test_exit_code "dload/dstore - load and store double local" "$TEST_FILES_DIR/stack_dload_dstore.bc" 42
test_exit_code "cload/cstore - load and store char local" "$TEST_FILES_DIR/stack_cload_cstore.bc" 65
test_exit_code "aload/astore - load and store reference" "$TEST_FILES_DIR/stack_aload_astore.bc" 42
test_exit_code "iinc - increment local variable" "$TEST_FILES_DIR/stack_iinc.bc" 15
test_exit_code "dup - duplicate top of stack" "$TEST_FILES_DIR/stack_dup.bc" 10
test_exit_code "dup2 - duplicate top two values" "$TEST_FILES_DIR/stack_dup2.bc" 14
test_exit_code "dup_x1 - duplicate and insert" "$TEST_FILES_DIR/stack_dup_x1.bc" 10
test_exit_code "dup_x2 - duplicate and insert x2" "$TEST_FILES_DIR/stack_dup_x2.bc" 15
test_exit_code "dup2_x1 - duplicate two and insert" "$TEST_FILES_DIR/stack_dup2_x1.bc" 25
test_exit_code "dup2_x2 - duplicate two and insert x2" "$TEST_FILES_DIR/stack_dup2_x2.bc" 35
test_exit_code "pop - pop top of stack" "$TEST_FILES_DIR/stack_pop.bc" 5
test_exit_code "pop2 - pop top two values" "$TEST_FILES_DIR/stack_pop2.bc" 5
test_exit_code "swap - swap top two values" "$TEST_FILES_DIR/stack_swap.bc" 7
test_exit_code "nop - no operation" "$TEST_FILES_DIR/stack_nop.bc" 1
echo ""

echo "== Integer Arithmetic Instructions =="
test_exit_code "iadd - integer addition" "$TEST_FILES_DIR/arith_iadd.bc" 8
test_exit_code "isub - integer subtraction" "$TEST_FILES_DIR/arith_isub.bc" 7
test_exit_code "imul - integer multiplication" "$TEST_FILES_DIR/arith_imul.bc" 20
test_exit_code "idiv - integer division" "$TEST_FILES_DIR/arith_idiv.bc" 5
test_exit_code "irem - integer remainder" "$TEST_FILES_DIR/arith_irem.bc" 1
test_exit_code "iand - bitwise AND" "$TEST_FILES_DIR/arith_iand.bc" 4
test_exit_code "ior - bitwise OR" "$TEST_FILES_DIR/arith_ior.bc" 7
test_exit_code "ixor - bitwise XOR" "$TEST_FILES_DIR/arith_ixor.bc" 3
test_exit_code "ishl - shift left" "$TEST_FILES_DIR/arith_ishl.bc" 20
test_exit_code "ishr - shift right" "$TEST_FILES_DIR/arith_ishr.bc" 5
echo ""

echo "== Float Arithmetic Instructions =="
test_exit_code "fadd - float addition" "$TEST_FILES_DIR/arith_fadd.bc" 6
test_exit_code "fsub - float subtraction" "$TEST_FILES_DIR/arith_fsub.bc" 7
test_exit_code "fmul - float multiplication" "$TEST_FILES_DIR/arith_fmul.bc" 12
test_exit_code "fdiv - float division" "$TEST_FILES_DIR/arith_fdiv.bc" 5
test_exit_code "frem - float remainder" "$TEST_FILES_DIR/arith_frem.bc" 1
test_exit_code "fneg - float negation" "$TEST_FILES_DIR/arith_fneg.bc" 5
echo ""

echo "== Double Arithmetic Instructions =="
test_exit_code "dadd - double addition" "$TEST_FILES_DIR/arith_dadd.bc" 6
test_exit_code "dsub - double subtraction" "$TEST_FILES_DIR/arith_dsub.bc" 7
test_exit_code "dmul - double multiplication" "$TEST_FILES_DIR/arith_dmul.bc" 12
test_exit_code "ddiv - double division" "$TEST_FILES_DIR/arith_ddiv.bc" 5
test_exit_code "drem - double remainder" "$TEST_FILES_DIR/arith_drem.bc" 1
test_exit_code "dneg - double negation" "$TEST_FILES_DIR/arith_dneg.bc" 5
echo ""

echo "== Long Arithmetic Instructions =="
test_exit_code "ladd - long addition" "$TEST_FILES_DIR/arith_ladd.bc" 5
test_exit_code "lsub - long subtraction" "$TEST_FILES_DIR/arith_lsub.bc" 7
test_exit_code "lmul - long multiplication" "$TEST_FILES_DIR/arith_lmul.bc" 12
test_exit_code "ldiv - long division" "$TEST_FILES_DIR/arith_ldiv.bc" 5
test_exit_code "lrem - long remainder" "$TEST_FILES_DIR/arith_lrem.bc" 1
test_exit_code "lneg - long negation" "$TEST_FILES_DIR/arith_lneg.bc" 5
test_exit_code "land - long bitwise AND" "$TEST_FILES_DIR/arith_land.bc" 4
test_exit_code "lor - long bitwise OR" "$TEST_FILES_DIR/arith_lor.bc" 7
test_exit_code "lxor - long bitwise XOR" "$TEST_FILES_DIR/arith_lxor.bc" 3
test_exit_code "lshl - long shift left" "$TEST_FILES_DIR/arith_lshl.bc" 20
test_exit_code "lshr - long shift right" "$TEST_FILES_DIR/arith_lshr.bc" 5
echo ""

echo "== Conversion Instructions =="
test_exit_code "i2c/c2i - int to char and back" "$TEST_FILES_DIR/conv_i2c.bc" 65
test_exit_code "i2l/l2i - int to long and back" "$TEST_FILES_DIR/conv_i2l.bc" 42
test_exit_code "i2f/f2i - int to float and back" "$TEST_FILES_DIR/conv_i2f.bc" 42
test_exit_code "i2d/d2i - int to double and back" "$TEST_FILES_DIR/conv_i2d.bc" 42
test_exit_code "l2f - long to float" "$TEST_FILES_DIR/conv_l2f.bc" 42
test_exit_code "l2d - long to double" "$TEST_FILES_DIR/conv_l2d.bc" 42
test_exit_code "f2l - float to long" "$TEST_FILES_DIR/conv_f2l.bc" 42
test_exit_code "f2d - float to double" "$TEST_FILES_DIR/conv_f2d.bc" 42
test_exit_code "d2l - double to long" "$TEST_FILES_DIR/conv_d2l.bc" 42
test_exit_code "d2f - double to float" "$TEST_FILES_DIR/conv_d2f.bc" 42
test_exit_code "c2i - char to int" "$TEST_FILES_DIR/conv_c2i.bc" 65
echo ""

echo "== Comparison Instructions =="
test_exit_code "lcmp - long comparison" "$TEST_FILES_DIR/comp_lcmp.bc" 1
test_exit_code "fcmpl - float comparison (less)" "$TEST_FILES_DIR/comp_fcmpl.bc" 1
test_exit_code "fcmpg - float comparison (greater)" "$TEST_FILES_DIR/comp_fcmpg.bc" 1
test_exit_code "dcmpl - double comparison (less)" "$TEST_FILES_DIR/comp_dcmpl.bc" 1
test_exit_code "dcmpg - double comparison (greater)" "$TEST_FILES_DIR/comp_dcmpg.bc" 1
echo ""

echo "== Control Flow Instructions =="
test_exit_code "goto - unconditional jump" "$TEST_FILES_DIR/control_goto.bc" 1
test_exit_code "goto_w - wide unconditional jump" "$TEST_FILES_DIR/control_goto_w.bc" 1
test_exit_code "invokestatic - function call" "$TEST_FILES_DIR/control_invokestatic.bc" 30
test_exit_code "return - void return" "$TEST_FILES_DIR/control_return.bc" 42
test_exit_code "ireturn - integer return" "$TEST_FILES_DIR/control_ireturn.bc" 99
test_exit_code "freturn - float return" "$TEST_FILES_DIR/control_freturn.bc" 42
test_exit_code "dreturn - double return" "$TEST_FILES_DIR/control_dreturn.bc" 42
test_exit_code "lreturn - long return" "$TEST_FILES_DIR/control_lreturn.bc" 42
echo ""

echo "== Conditional Instructions =="
test_exit_code "ifeq - branch if equal to zero" "$TEST_FILES_DIR/cond_ifeq.bc" 0
test_exit_code "ifne - branch if not equal to zero" "$TEST_FILES_DIR/cond_ifne.bc" 1
test_exit_code "iflt - branch if less than zero" "$TEST_FILES_DIR/cond_iflt.bc" 1
test_exit_code "ifge - branch if greater or equal to zero" "$TEST_FILES_DIR/cond_ifge.bc" 1
test_exit_code "ifgt - branch if greater than zero" "$TEST_FILES_DIR/cond_ifgt.bc" 1
test_exit_code "ifle - branch if less or equal to zero" "$TEST_FILES_DIR/cond_ifle.bc" 1
test_exit_code "if_icmpgt - branch if greater than" "$TEST_FILES_DIR/cond_if_icmpgt.bc" 0
test_exit_code "if_icmplt - branch if less than" "$TEST_FILES_DIR/cond_if_icmplt.bc" 0
test_exit_code "if_icmpeq - branch if equal" "$TEST_FILES_DIR/cond_if_icmpeq.bc" 1
test_exit_code "if_icmpne - branch if not equal" "$TEST_FILES_DIR/cond_if_icmpne.bc" 1
test_exit_code "if_icmpge - branch if greater or equal" "$TEST_FILES_DIR/cond_if_icmpge.bc" 1
test_exit_code "if_icmple - branch if less or equal" "$TEST_FILES_DIR/cond_if_icmple.bc" 1
echo ""

echo "== Array Instructions =="
test_exit_code "newarray/iastore/iaload - array operations" "$TEST_FILES_DIR/array_basic.bc" 42
test_exit_code "arraylength - get array length" "$TEST_FILES_DIR/array_length.bc" 5
echo ""

echo "== Object Instructions =="
test_exit_code "new/putfield/getfield - object operations" "$TEST_FILES_DIR/object_basic.bc" 100
echo ""

echo "== IO Instructions =="
test_output "invoke_write - write integer to stdout" "$TEST_FILES_DIR/io_invoke_write.bc" "42"
test_output "invoke_write - write chars to stdout" "$TEST_FILES_DIR/io_invoke_write_char.bc" "Hi"
echo ""

echo "== Constant Pool Instructions =="
test_output "ldc - load from constant pool" "$TEST_FILES_DIR/const_pool_basic.bc" "42100-7"
test_output "ldc - load every types from constant pool" "$TEST_FILES_DIR/const_pool_types.bc" "1 -7 40.023 true false C"
echo ""

echo "========================================"
echo -e "Results: $PASSED passed, $FAILED failed"
echo "========================================"

if [ $FAILED -gt 0 ]; then
    exit 1
fi
exit 0
