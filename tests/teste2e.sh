#!/bin/bash

# set -e

PROJECT_ROOT="$(pwd)"
COMPILER_DIR="$PROJECT_ROOT/inklusif"
VM_DIR="$PROJECT_ROOT/vm"
E2E_TEST_DIR="$PROJECT_ROOT/tests/e2e-files"

COMPILER_BIN="$COMPILER_DIR/inklusif"
VM_BIN="$VM_DIR/glados-vm"
BYTECODE_OUTPUT="$COMPILER_DIR/binary.ink"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m'

PASSED=0
FAILED=0

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[PASS]${NC} $1"
}

log_fail() {
    echo -e "${RED}[FAIL]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

build_compiler() {
    log_info "Building Inklusif compiler..."
    cd "$COMPILER_DIR"
    if ! make all; then
        echo -e "${RED}Compiler build failed${NC}"
        return 1
    fi
    log_info "Compiler build successful"
    return 0
}

build_vm() {
    log_info "Building GLaDOS VM..."
    cd "$COMPILER_DIR"
    if ! make all; then
        echo -e "${RED}VM build failed${NC}"
        return 1
    fi
    log_info "VM build successful"
    return 0
}

build_all() {
    echo "========================================"
    echo "Building GLaDOS compiler + vm"
    echo "========================================"
    
    if ! build_compiler; then
        exit 1
    fi
    
    if ! build_vm; then
        exit 1
    fi
    
    echo ""
}

parse_expected_file() {
    local expected_file="$1"
    EXPECTED_EXIT_CODE=""
    EXPECTED_OUTPUT=""
    
    if [ ! -f "$expected_file" ]; then
        return 1
    fi
    
    while IFS= read -r line || [ -n "$line" ]; do
        [[ -z "$line" || "$line" =~ ^# ]] && continue
        
        if [[ "$line" =~ ^exit_code:[[:space:]]*(.*) ]]; then
            EXPECTED_EXIT_CODE="${BASH_REMATCH[1]}"
        elif [[ "$line" =~ ^output:[[:space:]]*(.*) ]]; then
            EXPECTED_OUTPUT="${BASH_REMATCH[1]}"
        fi
    done < "$expected_file"
    
    return 0
}

run_test() {
    local ink_file="$1"
    local test_name="$(basename "$ink_file" .ink)"
    local expected_file="${ink_file%.ink}.expected"
    
    EXPECTED_EXIT_CODE=""
    EXPECTED_OUTPUT=""
    
    if [ -f "$expected_file" ]; then
        parse_expected_file "$expected_file"
    fi
    
    local compile_output
    local compile_exit_code
    
    compile_output=$("$COMPILER_BIN" "$ink_file")
    compile_exit_code=$?
    
    if [ $compile_exit_code -ne 0 ]; then
        log_fail "$test_name - Compilation failed (exit code: $compile_exit_code)"
        echo "  Compiler output: $compile_output"
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    if [ ! -f "$BYTECODE_OUTPUT" ]; then
        log_fail "$test_name - No bytecode file generated"
        FAILED=$((FAILED + 1))
        return 1
    fi
    
    local vm_output
    local vm_exit_code
    
    vm_output=$("$VM_BIN" "$BYTECODE_OUTPUT" 2>&1)
    vm_exit_code=$?
    
    local exit_code_match=1
    local output_match=1
    
    if [ "$vm_exit_code" -ne "$EXPECTED_EXIT_CODE" ]; then
        exit_code_match=0
    fi
    
    if [ -n "$EXPECTED_OUTPUT" ]; then
        if [ "$vm_output" != "$EXPECTED_OUTPUT" ]; then
            output_match=0
        fi
    fi
    
    if [ $exit_code_match -eq 1 ] && [ $output_match -eq 1 ]; then
        log_success "$test_name"
        PASSED=$((PASSED + 1))
        return 0
    else
        log_fail "$test_name"
        echo "  Input file: $ink_file"
        if [ $exit_code_match -eq 0 ]; then
            echo "  Exit code: $vm_exit_code (expected: $EXPECTED_EXIT_CODE)"
        fi
        if [ $output_match -eq 0 ]; then
            echo "  Output:   '$vm_output'"
            echo "  Expected: '$EXPECTED_OUTPUT'"
        fi
        FAILED=$((FAILED + 1))
        return 1
    fi
}

run_all_tests() {
    echo "========================================"
    echo "Running End-to-End Tests"
    echo "========================================"
    echo ""
    
    local test_files=("$E2E_TEST_DIR"/*.ink)
    
    for ink_file in "${test_files[@]}"; do
        run_test "$ink_file"
    done
    
    echo ""
}

build_all    
    
if [ ! -x "$COMPILER_BIN" ]; then
    echo -e "${RED}Error: Compiler not found at $COMPILER_BIN${NC}"
    exit 1
fi

if [ ! -x "$VM_BIN" ]; then
    echo -e "${RED}Error: VM not found at $VM_BIN${NC}"
    exit 1
fi

run_all_tests

echo "========================================"
echo -e "Results: ${GREEN}$PASSED passed${NC}, ${RED}$FAILED failed${NC}"
echo "========================================"

if [ $FAILED -gt 0 ]; then
    exit 1
fi
exit 0

