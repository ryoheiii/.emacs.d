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

# スタブ生成の失敗を握り潰すと PATH が実コマンドへフォールバックし、
# 本物の sudo / apt-get / wget を起動しかねない。失敗したら即座に中止する。
harness_fatal() {
    printf 'FATAL: %s\n' "$1" >&2
    exit 1
}

# 一時ディレクトリは単一のルート配下へ作り、EXIT トラップでまとめて消す。
# 配列に貯める方式は使えない。呼び出しがすべて $(harness_mktemp) という
# コマンド置換（サブシェル）の中で行われるため、親シェルの配列へ反映されない。
HARNESS_ROOT="$(mktemp -d)" || harness_fatal "一時ディレクトリを作成できません。"
trap 'rm -rf "$HARNESS_ROOT"' EXIT

# 失敗したら何も出力せず非ゼロを返す。呼び出し側は必ず || harness_fatal を付ける。
# この関数自体で exit しても、呼び出しが $(...) のサブシェル内なので親は止まらない。
harness_mktemp() {
    local dir
    dir="$(mktemp -d "$HARNESS_ROOT/h-XXXXXX")" || return 1
    [ -n "$dir" ] || return 1
    printf '%s\n' "$dir"
}

# スタブ用ディレクトリとして使える実体があることを確認する。
# 空パスを PATH の先頭へ置くとカレントディレクトリ扱いになり、
# 実コマンドへフォールバックするため必ず fatal にする。
harness_require_dir() {
    [ -n "$1" ] || harness_fatal "一時ディレクトリのパスが空です。"
    [ -d "$1" ] || harness_fatal "一時ディレクトリ $1 が存在しません。"
}

# スタブが実際に生成され実行可能であることを確認する。
assert_stubs_executable() {
    local dir="$1"; shift
    local cmd
    for cmd in "$@"; do
        [ -x "$dir/$cmd" ] \
            || harness_fatal "スタブ $dir/$cmd を作成できませんでした。実コマンドへフォールバックする危険があるため中止します。"
    done
}

# make_stub_bin <dir> <cmd>...
# 呼び出し引数を <dir>/calls.log へ記録するスタブを作る。
# 終了コードは STUB_EXIT_<CMD> で制御する（既定 0、ハイフンは _ へ読み替え）。
make_stub_bin() {
    local dir="$1"; shift
    harness_require_dir "$dir"
    mkdir -p "$dir" || harness_fatal "$dir を作成できません。"
    local cmd upper
    for cmd in "$@"; do
        upper="$(printf '%s' "$cmd" | tr 'a-z-' 'A-Z_')"
        cat > "$dir/$cmd" <<STUB
#!/bin/bash
printf '%s %s\n' "$cmd" "\$*" >> "$dir/calls.log"
exit "\${STUB_EXIT_$upper:-0}"
STUB
        chmod +x "$dir/$cmd" || harness_fatal "$dir/$cmd を実行可能にできません。"
    done
    assert_stubs_executable "$dir" "$@"
}

# make_isolated_bin <dir> <name>...
# 実在するコマンドへの symlink だけを置く。ここに無いコマンドは PATH から消えるため、
# 「コマンドが存在しない」状況を再現できる（PATH 先頭への追加では実物が残り再現できない）。
make_isolated_bin() {
    local dir="$1"; shift
    harness_require_dir "$dir"
    mkdir -p "$dir" || harness_fatal "$dir を作成できません。"
    local name src
    for name in "$@"; do
        src="$(command -v "$name" 2>/dev/null)" || continue
        ln -sf "$src" "$dir/$name" || harness_fatal "$dir/$name を作成できません。"
    done
}

# make_isolated_bin で必須のコマンドが揃ったことを確認する。
assert_isolated_bin() {
    local dir="$1"; shift
    assert_stubs_executable "$dir" "$@"
}

# make_fake_home <dir> — 使い捨ての HOME を組み立てる。
make_fake_home() {
    local home="$1"
    harness_require_dir "$home"
    mkdir -p "$home/.emacs.d/loads/straight/repos" \
             "$home/.emacs.d/loads/straight/versions" \
             "$home/.emacs.d/var/hist" \
             "$home/.emacs.d/var/backup" \
             "$home/.emacs.d/var/package" || harness_fatal "$home を初期化できません。"
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
GUARD_SANDBOX="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
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
GUARD_BIN_DSCL="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
make_isolated_bin "$GUARD_BIN_DSCL" bash id cut awk
cat > "$GUARD_BIN_DSCL/dscl" <<DSCL
#!/bin/bash
printf 'NFSHomeDirectory: %s\n' "$GUARD_REAL_HOME"
DSCL
chmod +x "$GUARD_BIN_DSCL/dscl"
assert_stubs_executable "$GUARD_BIN_DSCL" dscl bash id cut awk
assert_guard "guard falls back to dscl when getent is absent" 0 "$GUARD_BIN_DSCL" \
    "EMACS_SETUP_TEST_SANDBOX=1" "HOME=$GUARD_SANDBOX"

# 5. getent も dscl も不在 → 拒否（fail-closed）
GUARD_BIN_NONE="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
make_isolated_bin "$GUARD_BIN_NONE" bash id cut awk
assert_isolated_bin "$GUARD_BIN_NONE" bash id cut awk
assert_guard "guard rejects when home lookup is unavailable" 1 "$GUARD_BIN_NONE" \
    "EMACS_SETUP_TEST_SANDBOX=1" "HOME=$GUARD_SANDBOX"

# 6a. HOME を正規化できない → 拒否
assert_guard "guard rejects unresolvable HOME" 1 "" \
    "EMACS_SETUP_TEST_SANDBOX=1" "HOME=$GUARD_SANDBOX/does-not-exist"

# 6b. サンドボックスの .emacs.d が実ホーム配下を指す → 拒否
GUARD_SYMLINKED="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
ln -s "$GUARD_REAL_HOME/.emacs.d" "$GUARD_SYMLINKED/.emacs.d"
assert_guard "guard rejects .emacs.d symlinked into real home" 1 "" \
    "EMACS_SETUP_TEST_SANDBOX=1" "HOME=$GUARD_SYMLINKED"

# 6c. .local がサンドボックス外（実ホームでもない場所）を指す → 拒否
# ブラックリストではなくホワイトリストであることを固定する。
GUARD_OUTSIDE="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
GUARD_FOREIGN="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
mkdir -p "$GUARD_OUTSIDE/.emacs.d" "$GUARD_FOREIGN/important"
ln -s "$GUARD_FOREIGN/important" "$GUARD_OUTSIDE/.local"
assert_guard "guard rejects .local pointing outside the sandbox" 1 "" \
    "EMACS_SETUP_TEST_SANDBOX=1" "HOME=$GUARD_OUTSIDE"

# 6d. スタブ生成の失敗は fail-closed（実コマンドへ落ちない）
if ( harness_require_dir "" ) >/dev/null 2>&1; then
    record_fail "harness aborts on an empty stub dir"
else
    record_pass "harness aborts on an empty stub dir"
fi

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

EMPTY_INDEX="$(harness_mktemp)/empty.html" || harness_fatal "一時ディレクトリを作成できません。"
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
LIST_STUB="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
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
    harness_require_dir "$dir"
    mkdir -p "$dir" || harness_fatal "$dir を作成できません。"
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
    chmod +x "$dir/wget" || harness_fatal "$dir/wget を実行可能にできません。"
    assert_stubs_executable "$dir" wget
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
    stub_dir="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
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

DL_BOTH_STUB="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
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

# 原子性: .part を経由し、失敗時も残骸を残さない
DL_DIR_PATH="$HOME/.local/downloads"

assert_no_part_left() {
    local desc="$1" fail_pattern="$2" expect_tarball="$3"
    local stub_dir problems=""
    stub_dir="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
    make_wget_stub "$stub_dir"
    rm -rf "$DL_DIR_PATH"
    PATH="$stub_dir:$PATH" WGET_FAIL_PATTERN="$fail_pattern" \
        "$SCRIPT" --install 30.2 >/dev/null 2>&1
    if compgen -G "$DL_DIR_PATH/*.part" >/dev/null 2>&1; then
        problems="$problems .part残存"
    fi
    if [ "$expect_tarball" = yes ]; then
        [ -f "$DL_DIR_PATH/emacs-30.2.tar.gz" ] || problems="$problems 本体未確定"
    else
        [ -f "$DL_DIR_PATH/emacs-30.2.tar.gz" ] && problems="$problems 本体が誤って確定"
    fi
    if [ -z "$problems" ]; then
        record_pass "$desc"
    else
        record_fail "$desc — $problems"
    fi
    rm -rf "$DL_DIR_PATH"
}

assert_no_part_left "download leaves no .part on success" "" yes
assert_no_part_left "download cleans .part when all sources fail" "." no

# 壊れた .part が残っていても再利用されない
DL_STALE_STUB="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
make_wget_stub "$DL_STALE_STUB"
rm -rf "$DL_DIR_PATH"
mkdir -p "$DL_DIR_PATH"
printf 'CORRUPT' > "$DL_DIR_PATH/emacs-30.2.tar.gz.part"
PATH="$DL_STALE_STUB:$PATH" "$SCRIPT" --install 30.2 >/dev/null 2>&1
if [ -f "$DL_DIR_PATH/emacs-30.2.tar.gz" ] \
    && ! grep -q CORRUPT "$DL_DIR_PATH/emacs-30.2.tar.gz" 2>/dev/null; then
    record_pass "download does not reuse a stale .part"
else
    record_fail "download does not reuse a stale .part"
fi
rm -rf "$DL_DIR_PATH"

# mv の失敗は mv スタブで再現する。
# 確定先を既存ディレクトリにする方法は使えない（mv はその中へ移動して成功する）。
DL_MV_STUB="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
make_wget_stub "$DL_MV_STUB"
make_stub_bin "$DL_MV_STUB" mv
rm -rf "$DL_DIR_PATH"
DL_MV_ERR="$(PATH="$DL_MV_STUB:$PATH" STUB_EXIT_MV=1 \
    "$SCRIPT" --install 30.2 2>&1 >/dev/null)"
if echo "$DL_MV_ERR" | grep -q "確定に失敗" \
    && ! compgen -G "$DL_DIR_PATH/*.part" >/dev/null 2>&1; then
    record_pass "download cleans .part when mv fails"
else
    record_fail "download cleans .part when mv fails — $DL_MV_ERR"
fi
rm -rf "$DL_DIR_PATH"

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
echo "=== パッケージのアーカイブと展開 ==="

PKG_LIVE="$HOME/.emacs.d/loads/straight"
PKG_BAK="$HOME/.emacs.d/loads/straight.bak"
PKG_ARCHIVE="$HOME/.emacs.d/package.tar.gz"
PKG_VAR="$HOME/.emacs.d/var"
PKG_STUB=""

# emacs スタブを用意する。run_package_build が実 Emacs を起動しないようにする。
setup_pkg_stub() {
    PKG_STUB="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
    make_stub_bin "$PKG_STUB" emacs
}

# marker で中身を区別できるパッケージツリーを作る。
build_pkg_tree() {
    local marker="$1"
    rm -rf "$PKG_LIVE"
    mkdir -p "$PKG_LIVE/repos/$marker-pkg" "$PKG_LIVE/versions"
    printf '%s\n' "$marker" > "$PKG_LIVE/repos/$marker-pkg/file.el"
    printf '(%s)\n' "$marker" > "$PKG_LIVE/versions/default.el"
}

# ユーザーデータと eln-cache を配置する。
build_var_fixture() {
    rm -rf "$PKG_VAR"
    mkdir -p "$PKG_VAR/hist" "$PKG_VAR/backup" "$PKG_VAR/package/eln-cache"
    printf 'savehist-data\n' > "$PKG_VAR/hist/savehist"
    printf 'backup-data\n' > "$PKG_VAR/backup/bk"
    printf 'stale\n' > "$PKG_VAR/package/eln-cache/stale.eln"
}

# new マーカーのアーカイブを作り、live を old マーカーへ戻す。
prepare_archive_and_old_tree() {
    rm -f "$PKG_ARCHIVE"
    build_pkg_tree new
    PATH="$PKG_STUB:$PATH" "$SCRIPT" --packing-package >/dev/null 2>&1
    build_pkg_tree old
    build_var_fixture
    rm -rf "$PKG_BAK"
}

run_extract() {
    PATH="$PKG_STUB:$PATH" "$SCRIPT" --extract-package
}

pkg_marker() {
    # live ツリーがどちらのマーカーかを返す
    if [ -e "$PKG_LIVE/repos/new-pkg/file.el" ]; then
        printf 'new\n'
    elif [ -e "$PKG_LIVE/repos/old-pkg/file.el" ]; then
        printf 'old\n'
    else
        printf 'none\n'
    fi
}

setup_pkg_stub

# 1. 正常系: 内容が入れ替わり、ユーザーデータは保護され、eln-cache は消える
prepare_archive_and_old_tree
# 取得に失敗して空のまま進むと、下の再計算も空になり "" = "" が成立して
# 「ユーザーデータ保護」の検査が空虚に PASS する。fixture の前提条件として
# ここで止める。
HIST_SUM="$(md5sum < "$PKG_VAR/hist/savehist")" \
    || harness_fatal "fixture の savehist を読めません。"
BK_SUM="$(md5sum < "$PKG_VAR/backup/bk")" \
    || harness_fatal "fixture の backup/bk を読めません。"
run_extract >/dev/null 2>&1
EXTRACT_RC=$?
pkg_problems=""
[ "$EXTRACT_RC" -eq 0 ] || pkg_problems="$pkg_problems exit=$EXTRACT_RC"
[ "$(pkg_marker)" = new ] || pkg_problems="$pkg_problems 内容未更新($(pkg_marker))"
[ "$(md5sum < "$PKG_VAR/hist/savehist")" = "$HIST_SUM" ] || pkg_problems="$pkg_problems hist改変"
[ "$(md5sum < "$PKG_VAR/backup/bk")" = "$BK_SUM" ] || pkg_problems="$pkg_problems backup改変"
[ -e "$PKG_VAR/package/eln-cache/stale.eln" ] && pkg_problems="$pkg_problems eln残存"
[ -e "$PKG_BAK" ] && pkg_problems="$pkg_problems bak残存"
if [ -z "$pkg_problems" ]; then
    record_pass "extract replaces packages and preserves user data"
else
    record_fail "extract replaces packages and preserves user data —$pkg_problems"
fi

# 2. アーカイブ構造（pack が straight/ を含むこと）
prepare_archive_and_old_tree
if tar -tzf "$PKG_ARCHIVE" | grep -q '^straight/versions/default.el$'; then
    record_pass "archive keeps the straight/ prefix"
else
    record_fail "archive keeps the straight/ prefix"
fi

# 3. アーカイブ不在
rm -f "$PKG_ARCHIVE"
if run_extract >/dev/null 2>&1; then
    record_fail "extract fails without an archive"
else
    record_pass "extract fails without an archive"
fi

# 4. 破損アーカイブ → 既存ツリーは無変更
prepare_archive_and_old_tree
head -c 512 /dev/urandom > "$PKG_ARCHIVE"
run_extract >/dev/null 2>&1
EXTRACT_RC=$?
pkg_problems=""
[ "$EXTRACT_RC" -ne 0 ] || pkg_problems="$pkg_problems exit=0"
[ "$(pkg_marker)" = old ] || pkg_problems="$pkg_problems 既存ツリー改変($(pkg_marker))"
compgen -G "$HOME/.emacs.d/loads/.extract-*" >/dev/null 2>&1 && pkg_problems="$pkg_problems 一時ディレクトリ残存"
if [ -z "$pkg_problems" ]; then
    record_pass "corrupt archive leaves the existing tree intact"
else
    record_fail "corrupt archive leaves the existing tree intact —$pkg_problems"
fi

# 5. PACKAGE_TARGET を欠くアーカイブ → 既存ツリーは無変更
prepare_archive_and_old_tree
PKG_PARTIAL="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
mkdir -p "$PKG_PARTIAL/straight/repos/new-pkg"
printf 'new\n' > "$PKG_PARTIAL/straight/repos/new-pkg/file.el"
tar -czf "$PKG_ARCHIVE" -C "$PKG_PARTIAL" straight
run_extract >/dev/null 2>&1
EXTRACT_RC=$?
pkg_problems=""
[ "$EXTRACT_RC" -ne 0 ] || pkg_problems="$pkg_problems exit=0"
[ "$(pkg_marker)" = old ] || pkg_problems="$pkg_problems 既存ツリー改変($(pkg_marker))"
if [ -z "$pkg_problems" ]; then
    record_pass "incomplete archive is rejected"
else
    record_fail "incomplete archive is rejected —$pkg_problems"
fi

# 6. swap 失敗 → rollback で既存ツリーが復元される
prepare_archive_and_old_tree
PKG_MV_STUB="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
cat > "$PKG_MV_STUB/mv" <<'MVSTUB'
#!/bin/bash
# 全引数を走査する。mv_replace は -T を前置するため $1 だけを見ると判定が外れる。
for a in "$@"; do
    if printf '%s' "$a" | grep -q '\.extract-'; then
        exit 1
    fi
done
exec /bin/mv "$@"
MVSTUB
chmod +x "$PKG_MV_STUB/mv"
cp "$PKG_STUB/emacs" "$PKG_MV_STUB/emacs"
assert_stubs_executable "$PKG_MV_STUB" mv emacs
PATH="$PKG_MV_STUB:$PATH" "$SCRIPT" --extract-package >/dev/null 2>&1
EXTRACT_RC=$?
pkg_problems=""
[ "$EXTRACT_RC" -ne 0 ] || pkg_problems="$pkg_problems exit=0"
[ "$(pkg_marker)" = old ] || pkg_problems="$pkg_problems 復元されていない($(pkg_marker))"
[ -e "$PKG_BAK" ] && pkg_problems="$pkg_problems bak残存"
if [ -z "$pkg_problems" ]; then
    record_pass "swap failure rolls back to the existing tree"
else
    record_fail "swap failure rolls back to the existing tree —$pkg_problems"
fi

# 7. ビルド失敗 → 新ツリー配置済み・.bak 保持・復旧手順が出る／その手順が実際に効く
prepare_archive_and_old_tree
BUILD_ERR="$(PATH="$PKG_STUB:$PATH" STUB_EXIT_EMACS=1 \
    "$SCRIPT" --extract-package 2>&1 >/dev/null)"
pkg_problems=""
[ "$(pkg_marker)" = new ] || pkg_problems="$pkg_problems 新ツリー未配置"
[ -d "$PKG_BAK" ] || pkg_problems="$pkg_problems bak不在"
echo "$BUILD_ERR" | grep -q "mv '$PKG_BAK' '$PKG_LIVE'" || pkg_problems="$pkg_problems 復旧手順なし"
# 出力された手順を実際に実行して復元できることを確かめる
rm -rf "$PKG_LIVE"
mv "$PKG_BAK" "$PKG_LIVE"
[ "$(pkg_marker)" = old ] || pkg_problems="$pkg_problems 手順で復元できない"
if [ -z "$pkg_problems" ]; then
    record_pass "build failure keeps a working restore path"
else
    record_fail "build failure keeps a working restore path —$pkg_problems"
fi

# 8. 初回復元（既存ツリーなし）
prepare_archive_and_old_tree
rm -rf "$PKG_LIVE"
run_extract >/dev/null 2>&1
EXTRACT_RC=$?
pkg_problems=""
[ "$EXTRACT_RC" -eq 0 ] || pkg_problems="$pkg_problems exit=$EXTRACT_RC"
[ "$(pkg_marker)" = new ] || pkg_problems="$pkg_problems 展開されていない"
[ -e "$PKG_BAK" ] && pkg_problems="$pkg_problems bak作成"
if [ -z "$pkg_problems" ]; then
    record_pass "first restore works without a backup"
else
    record_fail "first restore works without a backup —$pkg_problems"
fi

# 9. .bak 残骸がある状態 → 上書きせず停止
prepare_archive_and_old_tree
mkdir -p "$PKG_BAK"
run_extract >/dev/null 2>&1
EXTRACT_RC=$?
if [ "$EXTRACT_RC" -ne 0 ] && [ "$(pkg_marker)" = old ]; then
    record_pass "leftover .bak stops the restore"
else
    record_fail "leftover .bak stops the restore (exit=$EXTRACT_RC, marker=$(pkg_marker))"
fi
rm -rf "$PKG_BAK"

# 10. emacs 不在 → 既存ツリーを消す前に中止
prepare_archive_and_old_tree
PKG_NO_EMACS="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
make_isolated_bin "$PKG_NO_EMACS" bash tar mktemp mkdir rm mv grep basename dirname
assert_isolated_bin "$PKG_NO_EMACS" bash tar mktemp mv
PATH="$PKG_NO_EMACS" "$SCRIPT" --extract-package >/dev/null 2>&1
EXTRACT_RC=$?
if [ "$EXTRACT_RC" -ne 0 ] && [ "$(pkg_marker)" = old ]; then
    record_pass "missing emacs aborts before touching packages"
else
    record_fail "missing emacs aborts before touching packages (exit=$EXTRACT_RC, marker=$(pkg_marker))"
fi

# 11b. tar が失敗しても一時ファイルを残さない
build_pkg_tree new
rm -f "$PKG_ARCHIVE"
PKG_TAR_STUB="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
make_stub_bin "$PKG_TAR_STUB" tar
# find を wc -l へ直接繋ぐと、find が失敗しても wc が 0 を出すため
# 前後の件数がともに 0 になり、残骸があっても一致して PASS してしまう。
# 取得と計数を分け、find の失敗はここで止める。
count_files_under() {
    local dir="$1" listing
    listing="$(find "$dir" -type f)" || return 1
    if [ -z "$listing" ]; then
        printf '0\n'
    else
        printf '%s\n' "$listing" | wc -l
    fi
}

PKG_TMPDIR="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
TMP_BEFORE="$(count_files_under "$PKG_TMPDIR")" \
    || harness_fatal "一時ディレクトリ $PKG_TMPDIR を走査できません。"
PATH="$PKG_TAR_STUB:$PATH" TMPDIR="$PKG_TMPDIR" STUB_EXIT_TAR=1 \
    "$SCRIPT" --packing-package >/dev/null 2>&1
TMP_AFTER="$(count_files_under "$PKG_TMPDIR")" \
    || harness_fatal "一時ディレクトリ $PKG_TMPDIR を走査できません。"
if [ "$TMP_BEFORE" -eq "$TMP_AFTER" ]; then
    record_pass "packing leaves no temp file when tar fails"
else
    record_fail "packing leaves no temp file when tar fails ($TMP_BEFORE -> $TMP_AFTER)"
fi

# 11. pack は PACKAGE_TARGET が欠けていたら作らない
build_pkg_tree new
rm -f "$PKG_LIVE/versions/default.el"
rm -f "$PKG_ARCHIVE"
PATH="$PKG_STUB:$PATH" "$SCRIPT" --packing-package >/dev/null 2>&1
PACK_RC=$?
if [ "$PACK_RC" -ne 0 ] && [ ! -f "$PKG_ARCHIVE" ]; then
    record_pass "packing rejects an incomplete tree"
else
    record_fail "packing rejects an incomplete tree (exit=$PACK_RC)"
fi

rm -rf "$PKG_LIVE" "$PKG_BAK" "$PKG_VAR" "$PKG_ARCHIVE"

echo ""
echo "=== --setup の依存パッケージ ==="

# --setup は実 sudo / apt-get を起動してはならない。必ずスタブ経由で検査し、
# 終了コードだけを見るテストは書かない（実 sudo を起動する危険があるため）。
make_setup_stubs() {
    local dir="$1" gccjit_available="$2"
    make_stub_bin "$dir" sudo apt-get
    cat > "$dir/gcc" <<'GCCSTUB'
#!/bin/bash
printf '13\n'
GCCSTUB
    chmod +x "$dir/gcc"
    if [ "$gccjit_available" = yes ]; then
        cat > "$dir/apt-cache" <<'APTCACHE'
#!/bin/bash
printf 'apt-cache %s\n' "$*" >> "$(dirname "$0")/calls.log"
case "$1" in
    policy) printf 'libgccjit-13-dev:\n  候補: 13.3.0\n' ;;
    search) printf 'libgccjit-13-dev - GCC just-in-time compilation\n' ;;
esac
APTCACHE
    else
        cat > "$dir/apt-cache" <<'APTCACHE'
#!/bin/bash
printf 'apt-cache %s\n' "$*" >> "$(dirname "$0")/calls.log"
case "$1" in
    policy) printf 'N: Unable to locate package\n' ;;
    search) printf 'libgccjit-14-dev - GCC just-in-time compilation\n' ;;
esac
APTCACHE
    fi
    chmod +x "$dir/apt-cache"
    # --setup の末尾は tree-sitter 文法の導入を試みる。スタブが無いと実 Emacs が
    # 起動して実際に文法をビルドしてしまうため、必ずスタブを置く。
    make_stub_bin "$dir" emacs
    assert_stubs_executable "$dir" sudo apt-get gcc apt-cache emacs
}

# libgccjit あり: 正しい cairo パッケージが渡り、誤記のものは渡らない
SETUP_OK_STUB="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
make_setup_stubs "$SETUP_OK_STUB" yes
PATH="$SETUP_OK_STUB:$PATH" "$SCRIPT" --setup >/dev/null 2>&1
setup_problems=""
grep -q 'libcairo2-dev' "$SETUP_OK_STUB/calls.log" 2>/dev/null || setup_problems="$setup_problems libcairo2-dev未指定"
grep -q 'libcairo-5c-dev' "$SETUP_OK_STUB/calls.log" 2>/dev/null && setup_problems="$setup_problems libcairo-5c-dev混入"
grep -q 'libgccjit-13-dev' "$SETUP_OK_STUB/calls.log" 2>/dev/null || setup_problems="$setup_problems libgccjit未指定"
if [ -z "$setup_problems" ]; then
    record_pass "setup installs the correct cairo and libgccjit packages"
else
    record_fail "setup installs the correct cairo and libgccjit packages —$setup_problems"
fi

# この設定が前提としている外部コマンドが --setup で入る (issue #11)。
# clangd は clang とは別パッケージ、mozc-server は straight が入れる mozc.el の
# helper、ripgrep は consult と xref の検索経路であり、いずれも欠けると縮退する。
setup_problems=""
for pkg in clangd mozc-server emacs-mozc-bin ripgrep; do
    grep -q "$pkg" "$SETUP_OK_STUB/calls.log" 2>/dev/null || setup_problems="$setup_problems $pkg未指定"
done
if [ -z "$setup_problems" ]; then
    record_pass "setup installs the external tools this config depends on"
else
    record_fail "setup installs the external tools this config depends on —$setup_problems"
fi

# libgccjit なし: apt-get install へ進まずに停止する
SETUP_NG_STUB="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
make_setup_stubs "$SETUP_NG_STUB" no
SETUP_NG_ERR="$(PATH="$SETUP_NG_STUB:$PATH" "$SCRIPT" --setup 2>&1 >/dev/null)"
setup_problems=""
echo "$SETUP_NG_ERR" | grep -q 'libgccjit-13-dev が見つかりません' || setup_problems="$setup_problems エラー文言なし"
grep -q 'apt-get install -y pkg-config' "$SETUP_NG_STUB/calls.log" 2>/dev/null && setup_problems="$setup_problems 続行してしまった"
if [ -z "$setup_problems" ]; then
    record_pass "setup stops when libgccjit is unavailable"
else
    record_fail "setup stops when libgccjit is unavailable —$setup_problems"
fi

echo ""
echo "=== --setup --gui ==="

# --setup の失敗も calls.log の不在も、握り潰すと呼び出し側が空文字を受け取る。
# 空文字どうしの比較は下の gtk3 検査で成立してしまうため、どちらも非ゼロで返す。
setup_calls_for_gui() {
    local dir="$1"; shift
    make_setup_stubs "$dir" yes
    if ! PATH="$dir:$PATH" "$SCRIPT" --setup "$@" >/dev/null 2>&1; then
        return 1
    fi
    cat "$dir/calls.log"
}

# 一時ディレクトリはコマンド置換の外で受ける。$(setup_calls_for_gui "$(harness_mktemp)")
# の形では harness_mktemp の失敗を検査できず、harness_fatal の exit も
# コマンド置換のサブシェルしか止められない。
SETUP_DEFAULT_DIR="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
SETUP_NOGUI_DIR="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
SETUP_GTK3_DIR="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"

SETUP_DEFAULT_LOG="$(setup_calls_for_gui "$SETUP_DEFAULT_DIR")" \
    || harness_fatal "--setup（既定）のスタブ実行に失敗しました。"
SETUP_NOGUI_LOG="$(setup_calls_for_gui "$SETUP_NOGUI_DIR" --gui no)" \
    || harness_fatal "--setup --gui no のスタブ実行に失敗しました。"
SETUP_GTK3_LOG="$(setup_calls_for_gui "$SETUP_GTK3_DIR" --gui gtk3)" \
    || harness_fatal "--setup --gui gtk3 のスタブ実行に失敗しました。"

gui_problems=""
echo "$SETUP_DEFAULT_LOG" | grep -q 'xorg-dev' || gui_problems="$gui_problems 既定でGUI未導入"
echo "$SETUP_NOGUI_LOG" | grep -q 'xorg-dev' && gui_problems="$gui_problems gui=noでGUI導入"
echo "$SETUP_NOGUI_LOG" | grep -q 'libncurses-dev' || gui_problems="$gui_problems gui=noでTUI未導入"
for pkg in pandoc clangd mozc-server emacs-mozc-bin ripgrep; do
    echo "$SETUP_NOGUI_LOG" | grep -q "$pkg" || gui_problems="$gui_problems gui=noで${pkg}未導入"
done
if [ -z "$gui_problems" ]; then
    record_pass "setup --gui no excludes only GUI packages"
else
    record_fail "setup --gui no excludes only GUI packages —$gui_problems"
fi

# tty 退行の防止。libgnutls28-dev は GUI 非依存なので --gui no でも入らなければならない。
if echo "$SETUP_NOGUI_LOG" | grep -q 'libgnutls28-dev'; then
    record_pass "setup --gui no keeps TLS support"
else
    record_fail "setup --gui no keeps TLS support"
fi

# 非空を先に検査する。両辺が空でも = は成立するため、これが無いと
# ログを取れなかった場合に「一致した」として PASS してしまう。
if [ -n "$SETUP_DEFAULT_LOG" ] && [ "$SETUP_DEFAULT_LOG" = "$SETUP_GTK3_LOG" ]; then
    record_pass "setup --gui gtk3 matches the default"
else
    record_fail "setup --gui gtk3 matches the default"
fi

assert_exit "setup rejects a missing gui value" 1 --setup --gui
assert_exit "setup rejects an invalid gui value" 1 --setup --gui bogus
assert_exit "setup rejects unknown options" 1 --setup --unknown

# 不正な GUI 値は副作用より前に弾く。後段で弾くとソースツリーが残り、
# 次回の正しい install まで「already installed」で拒否されてしまう。
GUI_EARLY_STUB="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
make_wget_stub "$GUI_EARLY_STUB"
rm -rf "$HOME/.local"
PATH="$GUI_EARLY_STUB:$PATH" "$SCRIPT" --install 30.2 --gui bogus >/dev/null 2>&1
gui_early_problems=""
[ -s "$GUI_EARLY_STUB/calls.log" ] && gui_early_problems="$gui_early_problems wget呼び出し"
[ -d "$HOME/.local/downloads/emacs" ] && gui_early_problems="$gui_early_problems ソースツリー残存"
if [ -z "$gui_early_problems" ]; then
    record_pass "invalid install gui value is rejected before any side effect"
else
    record_fail "invalid install gui value is rejected before any side effect —$gui_early_problems"
fi
rm -rf "$HOME/.local"

echo ""
echo "=== --setup-treesit ==="

# 文法のレシピと導入先は Elisp 側 (loads/site-elisp/my-treesit.el) が正本である。
# シェル側の契約は「そのライブラリを early-init.el 付きでロードし、
# my/treesit-install-c-grammars を呼ぶ」ことだけなので、そこだけを固定する。
make_treesit_home() {
    local home="$1" with_lib="$2"
    make_fake_home "$home"
    : > "$home/.emacs.d/early-init.el" || harness_fatal "early-init.el を作成できません。"
    if [ "$with_lib" = yes ]; then
        mkdir -p "$home/.emacs.d/loads/site-elisp" || harness_fatal "site-elisp を作成できません。"
        : > "$home/.emacs.d/loads/site-elisp/my-treesit.el" \
            || harness_fatal "my-treesit.el を作成できません。"
    fi
}

TS_HOME="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
TS_STUB="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
make_treesit_home "$TS_HOME" yes
make_stub_bin "$TS_STUB" emacs
ts_problems=""
HOME="$TS_HOME" PATH="$TS_STUB:$PATH" "$SCRIPT" --setup-treesit >/dev/null 2>&1 \
    || ts_problems="$ts_problems 非ゼロ終了"
grep -q 'early-init.el' "$TS_STUB/calls.log" 2>/dev/null \
    || ts_problems="$ts_problems early-init未ロード"
grep -q 'site-elisp/my-treesit.el' "$TS_STUB/calls.log" 2>/dev/null \
    || ts_problems="$ts_problems ライブラリ未ロード"
grep -q 'my/treesit-install-c-grammars' "$TS_STUB/calls.log" 2>/dev/null \
    || ts_problems="$ts_problems 導入コマンド未実行"
if [ -z "$ts_problems" ]; then
    record_pass "setup-treesit installs grammars through the shared elisp library"
else
    record_fail "setup-treesit installs grammars through the shared elisp library —$ts_problems"
fi

# ライブラリが無い場合、明示指定の --setup-treesit は fail-closed で止まる。
TS_NOLIB_HOME="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
TS_NOLIB_STUB="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
make_treesit_home "$TS_NOLIB_HOME" no
make_stub_bin "$TS_NOLIB_STUB" emacs
ts_problems=""
HOME="$TS_NOLIB_HOME" PATH="$TS_NOLIB_STUB:$PATH" "$SCRIPT" --setup-treesit >/dev/null 2>&1 \
    && ts_problems="$ts_problems 成功扱い"
[ -s "$TS_NOLIB_STUB/calls.log" ] && ts_problems="$ts_problems emacs起動"
if [ -z "$ts_problems" ]; then
    record_pass "setup-treesit fails when the elisp library is missing"
else
    record_fail "setup-treesit fails when the elisp library is missing —$ts_problems"
fi

# --setup の末尾でも文法を導入する（前提が揃っている場合）。
TS_SETUP_HOME="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
TS_SETUP_STUB="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
make_treesit_home "$TS_SETUP_HOME" yes
make_setup_stubs "$TS_SETUP_STUB" yes
ts_problems=""
HOME="$TS_SETUP_HOME" PATH="$TS_SETUP_STUB:$PATH" "$SCRIPT" --setup >/dev/null 2>&1 \
    || ts_problems="$ts_problems 非ゼロ終了"
grep -q 'my/treesit-install-c-grammars' "$TS_SETUP_STUB/calls.log" 2>/dev/null \
    || ts_problems="$ts_problems 文法未導入"
if [ -z "$ts_problems" ]; then
    record_pass "setup installs grammars when the prerequisites are met"
else
    record_fail "setup installs grammars when the prerequisites are met —$ts_problems"
fi

# tree-sitter 無効な Emacs では、--setup は文法を諦めて案内を出し、成功で終わる。
# 新規マシンでは --setup の時点で Emacs 自体が未ビルドなのが普通のため。
TS_SKIP_HOME="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
TS_SKIP_STUB="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
make_treesit_home "$TS_SKIP_HOME" yes
make_setup_stubs "$TS_SKIP_STUB" yes
ts_problems=""
TS_SKIP_OUT="$(HOME="$TS_SKIP_HOME" PATH="$TS_SKIP_STUB:$PATH" STUB_EXIT_EMACS=1 \
    "$SCRIPT" --setup 2>&1)" || ts_problems="$ts_problems 非ゼロ終了"
echo "$TS_SKIP_OUT" | grep -q -- '--setup-treesit' || ts_problems="$ts_problems 案内なし"
echo "$TS_SKIP_OUT" | grep -q 'setup-env complete' || ts_problems="$ts_problems 後続処理が止まった"
grep -q 'my/treesit-install-c-grammars' "$TS_SKIP_STUB/calls.log" 2>/dev/null \
    && ts_problems="$ts_problems 導入を試みた"
if [ -z "$ts_problems" ]; then
    record_pass "setup skips grammars and keeps going when tree-sitter is unavailable"
else
    record_fail "setup skips grammars and keeps going when tree-sitter is unavailable —$ts_problems"
fi

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

# early-init.el を読まない emacs --batch が作る迷子の eln-cache を回収すること。
# 本設定での正規の保存先は var/package/eln-cache/ で、直下のものは重複でしかない。
assert_clean_removes_stray_eln() {
    local stray="$HOME/.emacs.d/eln-cache"
    rm -rf "$stray"
    if ! mkdir -p "$stray/30.2-stub" || ! : > "$stray/30.2-stub/dummy.eln"; then
        record_fail "clean removes stray eln-cache — スタブを作れなかった"
        return
    fi
    if "$SCRIPT" --clean >/dev/null 2>&1 && [ ! -e "$stray" ]; then
        record_pass "clean removes stray eln-cache"
    else
        record_fail "clean removes stray eln-cache — $stray が残ったか --clean が失敗した"
    fi
}
assert_clean_removes_stray_eln

# symlink は利用者が意図して張ったものとみなし、リンクも参照先も残すこと。
# rm -rf は参照先を辿らずリンク自体を消すため、黙って消すと復旧できない。
assert_clean_keeps_stray_eln_symlink() {
    local stray="$HOME/.emacs.d/eln-cache"
    local target="$HOME/eln-target"
    rm -rf "$stray" "$target"
    if ! mkdir -p "$target" || ! : > "$target/dummy.eln" \
        || ! mkdir -p "$(dirname "$stray")" || ! ln -s "$target" "$stray"; then
        record_fail "clean keeps stray eln-cache symlink — スタブを作れなかった"
        return
    fi
    if "$SCRIPT" --clean >/dev/null 2>&1 \
        && [ -L "$stray" ] && [ -f "$target/dummy.eln" ]; then
        record_pass "clean keeps stray eln-cache symlink"
    else
        record_fail "clean keeps stray eln-cache symlink — リンクか参照先が失われた"
    fi
    rm -rf "$stray" "$target"
}
assert_clean_keeps_stray_eln_symlink

echo ""
echo "=== 出力先の分離 ==="

assert_stderr_only() {
    local desc="$1"; shift
    local out err
    out="$("$SCRIPT" "$@" 2>/dev/null)"
    err="$("$SCRIPT" "$@" 2>&1 >/dev/null)"
    if [ -z "$out" ] && [ -n "$err" ]; then
        record_pass "$desc"
    else
        record_fail "$desc — stdout=[$out]"
    fi
}

assert_stderr_only "invalid version writes only to stderr"  --install abc
assert_stderr_only "unknown option writes only to stderr"   --invalid
assert_stderr_only "missing archive writes only to stderr"  --extract-package

# --help は正常系なので stdout に出す（退行させない）
HELP_OUT="$("$SCRIPT" --help 2>/dev/null)"
HELP_RC=$?
if [ "$HELP_RC" -eq 0 ] && echo "$HELP_OUT" | grep -q 'Usage:'; then
    record_pass "help writes to stdout with exit 0"
else
    record_fail "help writes to stdout with exit 0 (exit=$HELP_RC)"
fi

echo ""
echo "=== インストール後の検証 ==="

# 検証対象は "$EMACS_INSTALL_PREFIX/bin/emacs" という絶対パスで呼ばれるため、
# PATH スタブでは横取りできない。HOME を差し替えて prefix ごと隔離し、
# make スタブがその絶対パスへ実行可能なスタブを生成する形にする。
make_install_stubs() {
    local dir="$1"
    make_wget_stub "$dir"
    cat > "$dir/tar" <<'TARSTUB'
#!/bin/bash
mkdir -p emacs
printf '#!/bin/bash\nexit 0\n' > emacs/autogen.sh
printf '#!/bin/bash\nexit 0\n' > emacs/configure
chmod +x emacs/autogen.sh emacs/configure
exit 0
TARSTUB
    cat > "$dir/make" <<'MAKESTUB'
#!/bin/bash
if [ "$1" = install ]; then
    mkdir -p "$HOME/.local/bin"
    cat > "$HOME/.local/bin/emacs" <<'INNER'
#!/bin/bash
case "${FAKE_EMACS_MODE:-ok}" in
    version-fail) exit 1 ;;
    native-nil)
        for a in "$@"; do
            [ "$a" = --version ] && exit 0
        done
        exit 1
        ;;
    *) exit 0 ;;
esac
INNER
    chmod +x "$HOME/.local/bin/emacs"
fi
exit 0
MAKESTUB
    chmod +x "$dir/tar" "$dir/make"
    assert_stubs_executable "$dir" wget tar make
}

assert_install_verify() {
    local desc="$1" mode="$2" expected_exit="$3" expect_msg="$4"
    local dir err rc problems=""
    dir="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
    make_install_stubs "$dir"
    rm -rf "$HOME/.local"
    err="$(PATH="$dir:$PATH" FAKE_EMACS_MODE="$mode" \
        "$SCRIPT" --install 30.2 2>&1 >/dev/null)"
    rc=$?
    [ "$rc" -eq "$expected_exit" ] || problems="$problems exit=$rc(期待 $expected_exit)"
    if [ -n "$expect_msg" ]; then
        echo "$err" | grep -q "$expect_msg" || problems="$problems 文言なし"
        [ -d "$HOME/.local/downloads/emacs" ] || problems="$problems ソース未保持"
        echo "$err" | grep -q -- '--uninstall' || problems="$problems 復旧手順なし"
    fi
    if [ -z "$problems" ]; then
        record_pass "$desc"
    else
        record_fail "$desc —$problems"
    fi
    rm -rf "$HOME/.local"
}

assert_install_verify "install verifies a working emacs" ok 0 ""
assert_install_verify "install fails when emacs cannot start" version-fail 1 "起動できません"
assert_install_verify "install fails when native-comp is missing" native-nil 1 "native-comp"

echo ""
echo "=== 実行前チェックとヘルプ ==="

# macOS を模擬しても、コマンドを必要としないアクションは動き続ける
MACOS_STUB="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
cat > "$MACOS_STUB/uname" <<'UNAMESTUB'
#!/bin/bash
printf 'Darwin\n'
UNAMESTUB
chmod +x "$MACOS_STUB/uname"
assert_stubs_executable "$MACOS_STUB" uname

macos_problems=""
PATH="$MACOS_STUB:$PATH" "$SCRIPT" --setup >/dev/null 2>&1 \
    && macos_problems="$macos_problems setupが通った"
PATH="$MACOS_STUB:$PATH" "$SCRIPT" --install 30.2 >/dev/null 2>&1 \
    && macos_problems="$macos_problems installが通った"
PATH="$MACOS_STUB:$PATH" "$SCRIPT" --help >/dev/null 2>&1 \
    || macos_problems="$macos_problems helpが落ちた"
PATH="$MACOS_STUB:$PATH" "$SCRIPT" --clean >/dev/null 2>&1 \
    || macos_problems="$macos_problems cleanが落ちた"
if [ -z "$macos_problems" ]; then
    record_pass "non-Linux blocks only apt/build actions"
else
    record_fail "non-Linux blocks only apt/build actions —$macos_problems"
fi

HELP_TEXT="$("$SCRIPT" --help 2>/dev/null)"
help_problems=""
echo "$HELP_TEXT" | grep -q -- '-g|--gui' || help_problems="$help_problems -g未記載"
echo "$HELP_TEXT" | grep -q -- '--setup \[-g' || help_problems="$help_problems setup--gui未記載"
echo "$HELP_TEXT" | grep -qi 'undo' || help_problems="$help_problems clean説明が不十分"
echo "$HELP_TEXT" | grep -q 'EMACS_SETUP_MIRROR_URL' || help_problems="$help_problems 環境変数未記載"
echo "$HELP_TEXT" | grep -q -- '--setup-node' || help_problems="$help_problems setup-node未記載"
echo "$HELP_TEXT" | grep -q -- '--uninstall-node' || help_problems="$help_problems uninstall-node未記載"
if [ -z "$help_problems" ]; then
    record_pass "help documents the actual interface"
else
    record_fail "help documents the actual interface —$help_problems"
fi

echo ""
echo "=== Node.js (--setup-node / --uninstall-node) ==="

# オフライン導入を実 tarball で通しで検査する。
# エントリ数を多くするのは意図的である。実装が tarball のトップレベル名を
# head や sed q のような早期終了で読むと、set -o pipefail 下で tar が SIGPIPE
# により 141 で落ち、「小さな tarball では通るが実物では失敗する」状態になる。
NODE_ARCH=""
case "$(uname -m)" in
    x86_64)  NODE_ARCH=x64 ;;
    aarch64) NODE_ARCH=arm64 ;;
    armv7l)  NODE_ARCH=armv7l ;;
esac

if [ -z "$NODE_ARCH" ]; then
    echo "SKIP: オフライン導入非対応アーキテクチャのため検査を省略"
elif ! command -v xz >/dev/null 2>&1; then
    echo "SKIP: xz が無いため tarball 検査を省略"
else
    NODE_HOME="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
    make_fake_home "$NODE_HOME"
    NODE_STAGE="$NODE_HOME/stage/node-v22.0.0-linux-$NODE_ARCH"
    mkdir -p "$NODE_STAGE/bin" "$NODE_HOME/.local/downloads/node" \
        || harness_fatal "tarball 用ディレクトリを作成できません。"
    # node / npm は実行可能でなければ実装の検証に引っかかる
    printf '#!/bin/sh\necho v22.0.0\n' > "$NODE_STAGE/bin/node"
    printf '#!/bin/sh\necho 10.0.0\n'  > "$NODE_STAGE/bin/npm"
    chmod +x "$NODE_STAGE/bin/node" "$NODE_STAGE/bin/npm"
    # SIGPIPE を再現させるための水増し（tar の出力を 1 行で打ち切らせない）
    for i in $(seq 1 3000); do
        printf 'x\n' > "$NODE_STAGE/bin/pad$i"
    done
    if tar -cJf "$NODE_HOME/.local/downloads/node/node-v22.0.0-linux-$NODE_ARCH.tar.xz" \
        -C "$NODE_HOME/stage" "node-v22.0.0-linux-$NODE_ARCH" 2>/dev/null; then
        NODE_OUT="$(HOME="$NODE_HOME" "$SCRIPT" --setup-node 2>&1)"
        NODE_RC=$?
        node_problems=""
        [ "$NODE_RC" -eq 0 ] || node_problems="$node_problems exit=$NODE_RC"
        echo "$NODE_OUT" | grep -qi "fnm" \
            && node_problems="$node_problems オフライン tarball があるのに fnm へ落ちた"
        [ -x "$NODE_HOME/.local/node/bin/node" ] \
            || node_problems="$node_problems アクティブリンクが張られていない"
        [ -d "$NODE_HOME/.local/share/nodejs/node-v22.0.0-linux-$NODE_ARCH" ] \
            || node_problems="$node_problems 展開先が作られていない"
        if [ -z "$node_problems" ]; then
            record_pass "setup-node installs from an offline tarball"
        else
            record_fail "setup-node installs from an offline tarball —$node_problems"
        fi

        # 同じ tarball で再実行しても壊れない（既存ディレクトリの入れ替え経路）
        if HOME="$NODE_HOME" "$SCRIPT" --setup-node >/dev/null 2>&1 \
            && [ -x "$NODE_HOME/.local/node/bin/node" ]; then
            record_pass "setup-node is idempotent for the same tarball"
        else
            record_fail "setup-node is idempotent for the same tarball"
        fi

        # 導入したものは --uninstall-node で消える
        HOME="$NODE_HOME" "$SCRIPT" --uninstall-node >/dev/null 2>&1
        if [ ! -e "$NODE_HOME/.local/node" ] \
            && [ ! -d "$NODE_HOME/.local/share/nodejs/node-v22.0.0-linux-$NODE_ARCH" ]; then
            record_pass "uninstall-node removes the offline install"
        else
            record_fail "uninstall-node removes the offline install"
        fi
    else
        echo "SKIP: xz 圧縮の tar を作成できないため tarball 検査を省略"
    fi
fi

# --uninstall-node は、このスクリプトが導入していない fnm を消してはならない
# （利用者の全 Node バージョンと設定が失われる）。印の有無で挙動が変わることを検査する。
assert_uninstall_node_fnm() {
    local desc="$1" marker="$2" expect_removed="$3"
    local home
    home="$(harness_mktemp)" || harness_fatal "一時ディレクトリを作成できません。"
    make_fake_home "$home"
    mkdir -p "$home/.local/share/fnm/node-versions/v20" \
        || harness_fatal "fnm ディレクトリを作成できません。"
    printf '#!/bin/sh\n' > "$home/.local/share/fnm/fnm"
    chmod +x "$home/.local/share/fnm/fnm"
    [ "$marker" = yes ] && : > "$home/.local/share/fnm/.installed-by-emacs-setup"

    HOME="$home" "$SCRIPT" --uninstall-node >/dev/null 2>&1

    if [ "$expect_removed" = yes ]; then
        if [ -d "$home/.local/share/fnm" ]; then
            record_fail "$desc — fnm ディレクトリが残っている"
        else
            record_pass "$desc"
        fi
    else
        if [ -d "$home/.local/share/fnm/node-versions/v20" ]; then
            record_pass "$desc"
        else
            record_fail "$desc — 既存 fnm の資産を削除した"
        fi
    fi
}

assert_uninstall_node_fnm "uninstall-node removes fnm it installed"     yes yes
assert_uninstall_node_fnm "uninstall-node keeps a pre-existing fnm"     no  no

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
