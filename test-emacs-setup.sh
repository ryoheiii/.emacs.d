#!/bin/bash
# test-emacs-setup.sh — emacs-setup.sh の引数パースと終了コードの回帰テスト

SCRIPT="./emacs-setup.sh"
PASS=0
FAIL=0

assert_exit() {
    local desc="$1" expected="$2"; shift 2
    "$SCRIPT" "$@" >/dev/null 2>&1
    local actual=$?
    if [ "$actual" -eq "$expected" ]; then
        echo "PASS: $desc (exit=$actual)"
        ((PASS++))
    else
        echo "FAIL: $desc (expected=$expected, actual=$actual)"
        ((FAIL++))
    fi
}

assert_output_contains() {
    local desc="$1" pattern="$2"; shift 2
    local output
    output=$("$SCRIPT" "$@" 2>&1)
    if echo "$output" | grep -qi "$pattern"; then
        echo "PASS: $desc"
        ((PASS++))
    else
        echo "FAIL: $desc — pattern '$pattern' not found in output"
        ((FAIL++))
    fi
}

echo "=== 基本オプション ==="
assert_exit "help returns 0"           0  --help
assert_exit "no args returns 1"        1
assert_exit "invalid option returns 1" 1  --invalid

echo ""
echo "=== --install 引数パース ==="
assert_exit "install without version"     1  --install
assert_exit "install with invalid ver"    1  --install abc
assert_exit "install missing gui value"   1  --install 30.1 --gui
assert_exit "install unknown sub-option"  1  --install 30.1 --unknown
assert_exit "install treats --gui as ver" 1  --install --gui pgtk
assert_exit "install multiple versions"   1  --install 30.1 30.2

echo ""
echo "=== エラーメッセージ内容 ==="
assert_output_contains "invalid ver message"  "Invalid version"    --install abc
assert_output_contains "no version message"   "No Emacs version"   --install
assert_output_contains "gui requires value"   "requires a value"   --install 30.1 --gui
assert_output_contains "unknown option msg"   "Unknown option"     --install 30.1 --unknown
assert_output_contains "multiple ver msg"     "Multiple version"   --install 30.1 30.2

echo ""
echo "=== --clean 動作 ==="
assert_exit "clean succeeds"     0  --clean
assert_exit "clean-all succeeds" 0  --clean-all

echo ""
echo "=== 構文チェック ==="
if bash -n "$SCRIPT" 2>/dev/null; then
    echo "PASS: bash -n syntax check"
    ((PASS++))
else
    echo "FAIL: bash -n syntax check"
    ((FAIL++))
fi

echo ""
echo "================================"
echo "Results: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ] && exit 0 || exit 1
