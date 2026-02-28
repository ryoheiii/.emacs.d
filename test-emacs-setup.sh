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
echo "=== --setup-node ==="
assert_output_contains "setup-node in help"  "setup-node"  --help

# ネットワーク依存テスト（環境変数ガード）
if [ "${RUN_INTEGRATION:-0}" = "1" ]; then
    echo "(integration tests enabled)"
    # 一度だけ実行して出力と終了コードを保存
    SETUP_NODE_OUTPUT=$("$SCRIPT" --setup-node 2>&1)
    SETUP_NODE_RC=$?
    if [ "$SETUP_NODE_RC" -ne 0 ]; then
        echo "FAIL: setup-node exited with $SETUP_NODE_RC"
        ((FAIL++))
    else
        echo "PASS: setup-node exit code 0"
        ((PASS++))
    fi
    if echo "$SETUP_NODE_OUTPUT" | grep -qi "installed via fnm"; then
        echo "PASS: setup-node installs via fnm"
        ((PASS++))
    else
        echo "FAIL: setup-node installs via fnm — pattern not found"
        ((FAIL++))
    fi
    if echo "$SETUP_NODE_OUTPUT" | grep -qi "v22"; then
        echo "PASS: setup-node installs node 22"
        ((PASS++))
    else
        echo "FAIL: setup-node installs node 22 — pattern not found"
        ((FAIL++))
    fi
    # 実状態検証: fnm default が v22 系か確認
    # setup_node 本体と同じく command -v で実際の fnm を検出（パス固定を避ける）
    FNM_BIN="$(command -v fnm 2>/dev/null || echo "$HOME/.local/share/fnm/fnm")"
    if [ -x "$FNM_BIN" ]; then
        eval "$("$FNM_BIN" env --shell bash)"
        FNM_DEFAULT=$("$FNM_BIN" current 2>/dev/null || true)
        if echo "$FNM_DEFAULT" | grep -q "^v22"; then
            echo "PASS: fnm current is v22.x ($FNM_DEFAULT)"
            ((PASS++))
        else
            echo "FAIL: fnm current expected v22.x, got '$FNM_DEFAULT'"
            ((FAIL++))
        fi
        NODE_PATH=$(command -v node 2>/dev/null || true)
        if [ -n "$NODE_PATH" ] && "$NODE_PATH" -v 2>/dev/null | grep -q "^v22"; then
            echo "PASS: node binary is v22.x ($("$NODE_PATH" -v))"
            ((PASS++))
        else
            echo "FAIL: node binary not v22.x (path=$NODE_PATH)"
            ((FAIL++))
        fi
    else
        echo "SKIP: fnm binary not found at $FNM_BIN — state verification skipped"
    fi
else
    echo "(skipping integration tests; set RUN_INTEGRATION=1 to enable)"
fi

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
