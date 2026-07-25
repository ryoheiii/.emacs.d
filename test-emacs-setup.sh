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
echo "=== --list のバージョン抽出 ==="

FIXTURE_INDEX="$TEST_DIR/tests/fixtures/emacs-index.html"

assert_list_output() {
    local desc="$1" expected="$2" index_url="$3"
    local actual
    actual="$(EMACS_SETUP_INDEX_URL="$index_url" "$SCRIPT" --list 2>/dev/null)"
    if [ "$actual" = "$expected" ]; then
        record_pass "$desc"
    else
        record_fail "$desc — expected [$expected] got [$actual]"
    fi
}

# fixture には 21.4a / 23.2b / 23.3b（両ミラーで 404 になる幽霊バージョン）と
# emacs-lisp-intro-2.04（無関係なアーカイブ）を含めてある。
assert_list_output "list extracts versions from fixture" \
    "$(printf '23.4\n24.5\n28.1\n29.4\n30.2')" \
    "file://$FIXTURE_INDEX"

LIST_GHOSTS="$(EMACS_SETUP_INDEX_URL="file://$FIXTURE_INDEX" "$SCRIPT" --list 2>/dev/null \
    | grep -cE '^(21\.4|23\.2|23\.3)$')"
if [ "$LIST_GHOSTS" -eq 0 ]; then
    record_pass "list excludes ghost versions"
else
    record_fail "list excludes ghost versions — $LIST_GHOSTS 件が混入"
fi

EMPTY_INDEX="$(harness_mktemp)/empty.html"
: > "$EMPTY_INDEX"
LIST_EMPTY_ERR="$(EMACS_SETUP_INDEX_URL="file://$EMPTY_INDEX" "$SCRIPT" --list 2>&1 >/dev/null)"
EMACS_SETUP_INDEX_URL="file://$EMPTY_INDEX" "$SCRIPT" --list >/dev/null 2>&1
LIST_EMPTY_RC=$?
if [ "$LIST_EMPTY_RC" -ne 0 ] && echo "$LIST_EMPTY_ERR" | grep -q "抽出できませんでした"; then
    record_pass "list fails loudly on empty index"
else
    record_fail "list fails loudly on empty index (exit=$LIST_EMPTY_RC, err=$LIST_EMPTY_ERR)"
fi

# 既定の取得先が組み立てられることを検査する。fixture テストは URL を上書きするため、
# これが無いと既定経路の破損を検出できない。
LIST_STUB="$(harness_mktemp)"
make_stub_bin "$LIST_STUB" curl
PATH="$LIST_STUB:$PATH" "$SCRIPT" --list >/dev/null 2>&1
if grep -q 'ftp.jaist.ac.jp/pub/GNU/emacs/' "$LIST_STUB/calls.log" 2>/dev/null; then
    record_pass "list uses the default mirror index url"
else
    record_fail "list uses the default mirror index url — $(cat "$LIST_STUB/calls.log" 2>/dev/null)"
fi

echo ""
echo "=== ダウンロード元のフォールバック ==="

# wget スタブ。URL が WGET_FAIL_PATTERN に一致したら失敗する。
# -O が指定されていればその出力先を作る（原子的ダウンロードの検査用）。
make_wget_stub() {
    local dir="$1"
    mkdir -p "$dir" || return 1
    cat > "$dir/wget" <<'WGETSTUB'
#!/bin/bash
printf 'wget %s\n' "$*" >> "$(dirname "$0")/calls.log"
out=""
url=""
prev=""
for arg in "$@"; do
    case "$prev" in
        -O) out="$arg" ;;
    esac
    case "$arg" in
        -*) ;;
        *) url="$arg" ;;
    esac
    prev="$arg"
done
if [ -n "${WGET_FAIL_PATTERN:-}" ] && printf '%s' "$url" | grep -q "$WGET_FAIL_PATTERN"; then
    exit 8
fi
if [ -n "$out" ]; then
    printf 'dummy-tarball\n' > "$out"
else
    printf 'dummy-tarball\n' > "$(basename "$url")"
fi
exit 0
WGETSTUB
    chmod +x "$dir/wget" || return 1
}

# --install は download の後に tar/configure/make へ進んで失敗する。
# ここで検査するのは取得元の組み立てとフォールバックの有無なので、
# 終了コードではなく calls.log を見る。
run_install_download() {
    local stub_dir="$1" fail_pattern="$2"
    rm -rf "$HOME/.local/downloads"
    rm -f "$stub_dir/calls.log"
    PATH="$stub_dir:$PATH" WGET_FAIL_PATTERN="$fail_pattern" \
        "$SCRIPT" --install 30.2 >/dev/null 2>&1
    return 0
}

assert_download_urls() {
    local desc="$1" fail_pattern="$2" expect_mirror="$3" expect_upstream="$4"
    local stub_dir problems=""
    stub_dir="$(harness_mktemp)"
    make_wget_stub "$stub_dir"
    run_install_download "$stub_dir" "$fail_pattern"
    local log="$stub_dir/calls.log"
    if [ "$expect_mirror" = yes ]; then
        grep -q 'ftp.jaist.ac.jp' "$log" 2>/dev/null || problems="$problems ミラー未試行"
    fi
    if [ "$expect_upstream" = yes ]; then
        grep -q 'ftp.gnu.org' "$log" 2>/dev/null || problems="$problems upstream未試行"
    else
        grep -q 'ftp.gnu.org' "$log" 2>/dev/null && problems="$problems upstream不要試行"
    fi
    grep -q -- '--timeout' "$log" 2>/dev/null || problems="$problems タイムアウト未指定"
    if [ -z "$problems" ]; then
        record_pass "$desc"
    else
        record_fail "$desc — $problems"
    fi
}

assert_download_urls "download uses mirror first" "" yes no
assert_download_urls "download falls back to upstream" "jaist" yes yes

DL_BOTH_STUB="$(harness_mktemp)"
make_wget_stub "$DL_BOTH_STUB"
rm -rf "$HOME/.local/downloads"
DL_BOTH_ERR="$(PATH="$DL_BOTH_STUB:$PATH" WGET_FAIL_PATTERN="." \
    "$SCRIPT" --install 30.2 2>&1 >/dev/null)"
if echo "$DL_BOTH_ERR" | grep -q "ミラー・upstream 共に失敗"; then
    record_pass "download reports when both sources fail"
else
    record_fail "download reports when both sources fail — $DL_BOTH_ERR"
fi
rm -rf "$HOME/.local/downloads"

echo ""
echo "=== --uninstall の契約 ==="

# ダミーのソースツリーと prefix 成果物を用意し、make uninstall の成否ごとに
# 終了コード・ソースツリーの消滅・prefix の残存物を検査する。
setup_uninstall_fixture() {
    local uninstall_rc="$1"
    local src="$HOME/.local/downloads/emacs"
    rm -rf "$HOME/.local"
    mkdir -p "$src/info" "$HOME/.local/bin" "$HOME/.local/share/man/man1" \
             "$HOME/.local/share/info"
    printf 'uninstall:\n\t@exit %s\n' "$uninstall_rc" > "$src/Makefile"
    : > "$src/info/emacs.info"
    : > "$HOME/.local/bin/emacs"
    : > "$HOME/.local/bin/emacs-30.2"
    : > "$HOME/.local/bin/emacsclient"
    : > "$HOME/.local/bin/etags"
    : > "$HOME/.local/share/man/man1/emacsclient.1.gz"
    : > "$HOME/.local/share/info/emacs.info"
}

assert_uninstall() {
    local desc="$1" uninstall_rc="$2" expected_exit="$3" expect_binaries_gone="$4"
    setup_uninstall_fixture "$uninstall_rc"
    "$SCRIPT" --uninstall >/dev/null 2>&1
    local actual=$?
    local problems=""
    [ "$actual" -eq "$expected_exit" ] || problems="exit=$actual(期待 $expected_exit)"
    [ -d "$HOME/.local/downloads/emacs" ] && problems="$problems src残存"
    if [ "$expect_binaries_gone" = yes ]; then
        [ -e "$HOME/.local/bin/emacsclient" ] && problems="$problems emacsclient残存"
        [ -e "$HOME/.local/bin/emacs-30.2" ] && problems="$problems versioned残存"
        [ -e "$HOME/.local/share/man/man1/emacsclient.1.gz" ] && problems="$problems man残存"
        [ -e "$HOME/.local/share/info/emacs.info" ] && problems="$problems info残存"
    fi
    if [ -z "$problems" ]; then
        record_pass "$desc"
    else
        record_fail "$desc — $problems"
    fi
}

# 成功時: 終了コード 0、ソースツリーは消える
assert_uninstall "uninstall succeeds and removes source tree" 0 0 no
# 失敗時: フォールバック削除が走り、ソースツリーも消え、終了コードは非 0
assert_uninstall "uninstall failure still clears source tree and reports" 1 1 yes
rm -rf "$HOME/.local"

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
