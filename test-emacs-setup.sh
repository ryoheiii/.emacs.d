#!/bin/bash
# test-emacs-setup.sh — emacs-setup.sh の引数パースと終了コードの回帰テスト
#
# 【重要】このスクリプトへ set -e を追加してはならない。
# ((PASS++)) は PASS=0 のとき後置インクリメントの旧値 0 を返すため終了ステータスが 1 になり、
# 最初の PASS で全体が落ちる。各コマンドの成否は明示的に検査すること。
#
# --clean / --clean-all は $HOME/.emacs.d 配下を実際に削除する。
# 直接実行による実データ破壊を防ぐため、冒頭でサンドボックス判定を行う。

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=tests/my-test-sandbox-guard.sh
. "$TEST_DIR/tests/my-test-sandbox-guard.sh"
require_test_sandbox || exit 1

SCRIPT="${SCRIPT:-./emacs-setup.sh}"
GUARD_LIB="$TEST_DIR/tests/my-test-sandbox-guard.sh"
PASS=0
FAIL=0

#### テスト基盤 ####

# 一時ディレクトリをまとめて後始末する。
HARNESS_TMPDIRS=()
harness_cleanup() {
    local dir
    for dir in "${HARNESS_TMPDIRS[@]:-}"; do
        [ -n "$dir" ] && [ -d "$dir" ] && rm -rf "$dir"
    done
}
trap harness_cleanup EXIT

harness_mktemp() {
    local dir
    dir="$(mktemp -d)" || return 1
    HARNESS_TMPDIRS+=("$dir")
    printf '%s\n' "$dir"
}

# make_stub_bin <dir> <cmd>...
# 呼び出し引数を <dir>/calls.log へ記録するスタブを作る。
# 終了コードは STUB_EXIT_<CMD> で制御する（既定 0、ハイフンは _ へ読み替え）。
make_stub_bin() {
    local dir="$1"; shift
    mkdir -p "$dir" || return 1
    local cmd upper
    for cmd in "$@"; do
        upper="$(printf '%s' "$cmd" | tr 'a-z-' 'A-Z_')"
        cat > "$dir/$cmd" <<STUB
#!/bin/bash
printf '%s %s\n' "$cmd" "\$*" >> "$dir/calls.log"
exit "\${STUB_EXIT_$upper:-0}"
STUB
        chmod +x "$dir/$cmd" || return 1
    done
}

# make_isolated_bin <dir> <name>...
# 実在するコマンドへの symlink だけを置く。ここに無いコマンドは PATH から消えるため、
# 「コマンドが存在しない」状況を再現できる（PATH 先頭への追加では実物が残り再現できない）。
make_isolated_bin() {
    local dir="$1"; shift
    mkdir -p "$dir" || return 1
    local name src
    for name in "$@"; do
        src="$(command -v "$name" 2>/dev/null)" || continue
        ln -sf "$src" "$dir/$name" || return 1
    done
}

# make_fake_home <dir> — 使い捨ての HOME を組み立てる。
make_fake_home() {
    local home="$1"
    mkdir -p "$home/.emacs.d/loads/straight/repos" \
             "$home/.emacs.d/loads/straight/versions" \
             "$home/.emacs.d/var/hist" \
             "$home/.emacs.d/var/backup" \
             "$home/.emacs.d/var/package" || return 1
}

record_pass() {
    echo "PASS: $1"
    ((PASS++))
    return 0
}

record_fail() {
    echo "FAIL: $1"
    ((FAIL++))
    return 0
}

assert_exit() {
    local desc="$1" expected="$2"; shift 2
    "$SCRIPT" "$@" >/dev/null 2>&1
    local actual=$?
    if [ "$actual" -eq "$expected" ]; then
        record_pass "$desc (exit=$actual)"
    else
        record_fail "$desc (expected=$expected, actual=$actual)"
    fi
}

assert_output_contains() {
    local desc="$1" pattern="$2"; shift 2
    local output
    output=$("$SCRIPT" "$@" 2>&1)
    if echo "$output" | grep -qi "$pattern"; then
        record_pass "$desc"
    else
        record_fail "$desc — pattern '$pattern' not found in output"
    fi
}

#### サンドボックスガード ####
# ガード自体の検証では emacs-setup.sh を一切呼ばない。
# ガードだけを別プロセスで source し、終了コードを見る。

run_guard() {
    # 使い方: run_guard <PATH 上書き（空なら現行）> <env 代入>...
    # bash -c の本体はシングルクォートで渡す。"$1" は内側の bash が受け取る
    # 位置パラメータ（GUARD_LIB）であり、外側で展開してはならない。
    local path_override="$1"; shift
    if [ -n "$path_override" ]; then
        # shellcheck disable=SC2016
        env -i PATH="$path_override" "$@" bash -c \
            '. "$1" && require_test_sandbox' _ "$GUARD_LIB" >/dev/null 2>&1
    else
        # shellcheck disable=SC2016
        env "$@" bash -c \
            '. "$1" && require_test_sandbox' _ "$GUARD_LIB" >/dev/null 2>&1
    fi
}

assert_guard() {
    local desc="$1" expected="$2" path_override="$3"; shift 3
    run_guard "$path_override" "$@"
    local actual=$?
    if [ "$actual" -eq "$expected" ]; then
        record_pass "$desc (exit=$actual)"
    else
        record_fail "$desc (expected=$expected, actual=$actual)"
    fi
}

echo "=== サンドボックスガード ==="

GUARD_REAL_HOME="$(my_test_guard__real_home)"
GUARD_SANDBOX="$(harness_mktemp)"
make_fake_home "$GUARD_SANDBOX"

# 1. マーカー未設定 + 実ホーム → 拒否
assert_guard "guard rejects without marker" 1 "" \
    "HOME=$GUARD_REAL_HOME"
# 2. マーカー設定 + 実ホーム → 拒否（マーカーは抜け道にならない）
assert_guard "guard rejects real home even with marker" 1 "" \
    "EMACS_SETUP_TEST_SANDBOX=1" "HOME=$GUARD_REAL_HOME"
# 3. マーカー設定 + 隔離ホーム → 通過
assert_guard "guard accepts sandbox home" 0 "" \
    "EMACS_SETUP_TEST_SANDBOX=1" "HOME=$GUARD_SANDBOX"

# 4. getent 不在 + 正常な dscl → 通過（macOS 相当）
GUARD_BIN_DSCL="$(harness_mktemp)"
make_isolated_bin "$GUARD_BIN_DSCL" bash id cut awk
cat > "$GUARD_BIN_DSCL/dscl" <<DSCL
#!/bin/bash
printf 'NFSHomeDirectory: %s\n' "$GUARD_REAL_HOME"
DSCL
chmod +x "$GUARD_BIN_DSCL/dscl"
assert_guard "guard falls back to dscl when getent is absent" 0 "$GUARD_BIN_DSCL" \
    "EMACS_SETUP_TEST_SANDBOX=1" "HOME=$GUARD_SANDBOX"

# 5. getent も dscl も不在 → 拒否（fail-closed）
GUARD_BIN_NONE="$(harness_mktemp)"
make_isolated_bin "$GUARD_BIN_NONE" bash id cut awk
assert_guard "guard rejects when home lookup is unavailable" 1 "$GUARD_BIN_NONE" \
    "EMACS_SETUP_TEST_SANDBOX=1" "HOME=$GUARD_SANDBOX"

# 6a. HOME を正規化できない → 拒否
assert_guard "guard rejects unresolvable HOME" 1 "" \
    "EMACS_SETUP_TEST_SANDBOX=1" "HOME=$GUARD_SANDBOX/does-not-exist"

# 6b. サンドボックスの .emacs.d が実ホーム配下を指す → 拒否
GUARD_SYMLINKED="$(harness_mktemp)"
ln -s "$GUARD_REAL_HOME/.emacs.d" "$GUARD_SYMLINKED/.emacs.d"
assert_guard "guard rejects .emacs.d symlinked into real home" 1 "" \
    "EMACS_SETUP_TEST_SANDBOX=1" "HOME=$GUARD_SYMLINKED"

echo ""
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
    record_pass "bash -n syntax check"
else
    record_fail "bash -n syntax check"
fi

echo ""
echo "================================"
echo "Results: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ] && exit 0 || exit 1
