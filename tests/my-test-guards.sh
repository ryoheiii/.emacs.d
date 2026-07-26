#!/bin/bash
# my-test-guards.sh --- テスト基盤と lint 基盤の fail-closed ガードを故障注入で検査する
#
# test-emacs-setup.sh のガードと Makefile の lint の環境分離、および
# tests/my-bench-run.sh の run_trial は、いずれも「外しても通常実行は全件 PASS のまま
# 通る」性質を持つ。ここでは故障を注入し、ガードが無ければ偽 PASS へ戻る箇所で
# 実際に停止することを固定する。
#
# 設計上の要点:
#
# - 注入はすべて外部から行う。検査対象のファイルへ注入フックを埋め込まない
#   （ガードを検査するために、そのガードのあるファイルへ分岐を足すのは本末転倒）。
# - 各注入は「対象ガードを外したときにだけ偽の成功へ戻る」ものでなければならない。
#   単に「非ゼロになる」だけの注入は、別のガードが受けても通るため判別力がない。
#   そのためスタブは有効な値を出しつつ非ゼロ終了する形にし、同種のガードが直列に
#   並ぶ箇所では呼び出し回数で挙動を変える。
# - このスクリプトは test-emacs-setup.sh と違い set -e を使ってよい。
#   カウンタを PASS=$((PASS + 1)) で増やすため、((PASS++)) が PASS=0 のとき
#   終了ステータス 1 を返す問題が起きない。
# - 期待どおり非ゼロで終わる呼び出しは if out="$(…)"; then rc=0; else rc=$?; fi の
#   形で受ける。裸の代入で書くと最初の期待失敗でこのスクリプト自身が終了する。

set -Eeuo pipefail

# 正の対照が外部環境で変質するのを防ぐ。必要なケースだけ明示的に設定する。
unset SHELLCHECK_OPTS

GUARD_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$GUARD_DIR/.." && pwd)"
cd "$REPO_ROOT"

# shellcheck source=tests/my-test-sandbox-guard.sh
. "$GUARD_DIR/my-test-sandbox-guard.sh"
require_test_sandbox || exit 1

# 検査対象は差し替え可能にする（手動 mutation 検証で使う）。
# test-emacs-setup.sh の SCRIPT は「被テストの emacs-setup.sh」を指す別物なので流用しない。
MY_GUARD_HARNESS="${MY_GUARD_HARNESS:-./test-emacs-setup.sh}"
MY_GUARD_BENCH="${MY_GUARD_BENCH:-tests/my-bench-run.sh}"
MAKE_CMD="${MAKE:-make}"

PASS=0
FAIL=0
SKIP=0

guard_fatal() {
    printf 'FATAL: %s\n' "$1" >&2
    exit 1
}

record_pass() {
    printf 'PASS: %s\n' "$1"
    PASS=$((PASS + 1))
}

record_fail() {
    printf 'FAIL: %s\n' "$1"
    FAIL=$((FAIL + 1))
}

record_skip() {
    printf 'SKIP: %s\n' "$1"
    SKIP=$((SKIP + 1))
}

#### 一時領域 ####
# 単一ルートへ集約し、EXIT トラップでまとめて消す。終了コードだけの検査では、
# 空出力で成功した場合に子パスが /case-… へ展開され rm -rf "" を仕掛けることになる。
GUARD_ROOT="$(mktemp -d)" || guard_fatal "一時ディレクトリを作成できません。"
[ -n "$GUARD_ROOT" ] || guard_fatal "一時ディレクトリのパスが空です。"
[ -d "$GUARD_ROOT" ] || guard_fatal "一時ディレクトリ $GUARD_ROOT が存在しません。"
trap 'rm -rf "$GUARD_ROOT"' EXIT

# ケースごとに完全に独立した領域を作る。状態付きスタブのカウンタを共有すると、
# 呼び出し回数を狙った注入が別の呼び出しへずれる。
new_case_dir() {
    local name="$1" dir
    dir="$GUARD_ROOT/case-$name"
    [ -e "$dir" ] && return 1
    mkdir -p "$dir/bin" "$dir/home" || return 1
    printf '%s\n' "$dir"
}

# スタブから実体へ委譲するための絶対パス。解決できなければ実コマンドへ
# フォールバックする危険があるので中止する。
resolve_real() {
    local name="$1" path
    path="$(command -v "$name")" || return 1
    [ -n "$path" ] || return 1
    [ -x "$path" ] || return 1
    printf '%s\n' "$path"
}

REAL_CAT="$(resolve_real cat)" || guard_fatal "cat の実体を解決できません。"
REAL_FIND="$(resolve_real find)" || guard_fatal "find の実体を解決できません。"
REAL_MKTEMP="$(resolve_real mktemp)" || guard_fatal "mktemp の実体を解決できません。"
REAL_MD5SUM="$(resolve_real md5sum)" || guard_fatal "md5sum の実体を解決できません。"
REAL_AWK="$(resolve_real awk)" || guard_fatal "awk の実体を解決できません。"
REAL_DATE="$(resolve_real date)" || guard_fatal "date の実体を解決できません。"
REAL_GREP="$(resolve_real grep)" || guard_fatal "grep の実体を解決できません。"

assert_stub() {
    local path="$1"
    [ -x "$path" ] || guard_fatal "スタブ $path を作成できませんでした。実コマンドへフォールバックする危険があるため中止します。"
}

#### 注入スタブ ####

# md5sum: N 回目の呼び出しだけ失敗する。常時失敗させると必ず最初のガードで止まり、
# 後続のガードを外しても検出できない。
make_md5sum_stub() {
    local dir="$1" fail_at="$2"
    cat > "$dir/bin/md5sum" <<STUB
#!/bin/bash
n=0
if [ -f "$dir/md5sum.count" ]; then read -r n < "$dir/md5sum.count"; fi
n=\$((n + 1))
printf '%s\n' "\$n" > "$dir/md5sum.count"
if [ "\$n" -eq $fail_at ]; then
    exit 1
fi
exec $REAL_MD5SUM "\$@"
STUB
    chmod +x "$dir/bin/md5sum" || guard_fatal "$dir/bin/md5sum を実行可能にできません。"
    assert_stub "$dir/bin/md5sum"
}

# find: "-type f" を含む呼び出しの N 回目だけ失敗する。
# emacs-setup.sh の find は -maxdepth 1 -name しか使わないため巻き込まない。
make_find_stub() {
    local dir="$1" fail_at="$2"
    cat > "$dir/bin/find" <<STUB
#!/bin/bash
is_target=0
prev=""
for a in "\$@"; do
    if [ "\$prev" = "-type" ] && [ "\$a" = "f" ]; then is_target=1; fi
    prev="\$a"
done
if [ "\$is_target" -eq 1 ]; then
    n=0
    if [ -f "$dir/find.count" ]; then read -r n < "$dir/find.count"; fi
    n=\$((n + 1))
    printf '%s\n' "\$n" > "$dir/find.count"
    if [ "\$n" -eq $fail_at ]; then
        exit 1
    fi
fi
exec $REAL_FIND "\$@"
STUB
    chmod +x "$dir/bin/find" || guard_fatal "$dir/bin/find を実行可能にできません。"
    assert_stub "$dir/bin/find"
}

# cat: 引数で calls.log を読む呼び出しにだけ介入する。
# 引数なしのヒアドキュメント（cat > file <<EOF）は素通しする。
#   fail  … 非ゼロ終了する
#   empty … 何も出さずに正常終了する
make_cat_stub() {
    local dir="$1" mode="$2"
    cat > "$dir/bin/cat" <<STUB
#!/bin/bash
for a in "\$@"; do
    case "\$a" in
        */calls.log)
            if [ "$mode" = empty ]; then
                exit 0
            fi
            exit 1
            ;;
    esac
done
exec $REAL_CAT "\$@"
STUB
    chmod +x "$dir/bin/cat" || guard_fatal "$dir/bin/cat を実行可能にできません。"
    assert_stub "$dir/bin/cat"
}

# mktemp: harness_mktemp のテンプレートにだけ介入する。
#   rc    … 実体で正しくディレクトリを作って出力してから非ゼロ終了する
#           （終了コード検査だけを識別するため。無出力にすると直後の非空検査でも
#            同じ結果になり、どちらのガードを見ているか判別できない）
#   empty … 無出力で正常終了する（非空検査だけを識別する）
make_mktemp_stub() {
    local dir="$1" mode="$2"
    cat > "$dir/bin/mktemp" <<STUB
#!/bin/bash
for a in "\$@"; do
    case "\$a" in
        */h-XXXXXX)
            if [ "$mode" = empty ]; then
                exit 0
            fi
            d="\$($REAL_MKTEMP "\$@")" || exit 1
            printf '%s\n' "\$d"
            exit 1
            ;;
    esac
done
exec $REAL_MKTEMP "\$@"
STUB
    chmod +x "$dir/bin/mktemp" || guard_fatal "$dir/bin/mktemp を実行可能にできません。"
    assert_stub "$dir/bin/mktemp"
}

# 呼び出し回数ごとに「出力|終了コード」を与えるスタブ。計画に無い回は実体へ委譲する。
# 出力に @EMPTY@ を指定すると何も出さない。
make_plan_stub() {
    local dir="$1" cmd="$2" real="$3"; shift 3
    local plan="$dir/$cmd.plan" entry
    : > "$plan" || guard_fatal "$plan を作成できません。"
    for entry in "$@"; do
        printf '%s\n' "$entry" >> "$plan" || guard_fatal "$plan へ書き込めません。"
    done
    cat > "$dir/bin/$cmd" <<STUB
#!/bin/bash
n=0
if [ -f "$dir/$cmd.count" ]; then read -r n < "$dir/$cmd.count"; fi
n=\$((n + 1))
printf '%s\n' "\$n" > "$dir/$cmd.count"
i=0
found=0
out=""
rc=0
while IFS='|' read -r o r; do
    i=\$((i + 1))
    if [ "\$i" -eq "\$n" ]; then
        out="\$o"
        rc="\$r"
        found=1
        break
    fi
done < "$plan"
if [ "\$found" -eq 0 ]; then
    exec $real "\$@"
fi
if [ "\$out" != "@EMPTY@" ]; then
    printf '%s\n' "\$out"
fi
exit "\$rc"
STUB
    chmod +x "$dir/bin/$cmd" || guard_fatal "$dir/bin/$cmd を実行可能にできません。"
    assert_stub "$dir/bin/$cmd"
}

# timeout: ベンチ本体を起動せずに probe 行だけを出して成功する。
# これが無いと BENCH_ROOT 不在で必ず失敗し、「ガードが無ければ有効試行になる」状態を
# 作れず、検査が判別力を失う。
make_timeout_stub() {
    local dir="$1"
    cat > "$dir/bin/timeout" <<'STUB'
#!/bin/bash
printf 'MY_BENCH t1_window_setup=0.100000\nMY_BENCH end\n'
exit 0
STUB
    chmod +x "$dir/bin/timeout" || guard_fatal "$dir/bin/timeout を実行可能にできません。"
    assert_stub "$dir/bin/timeout"
}

# grep: probe 検査を実体へ委譲したうえで、"MY_BENCH end" の検査を終えた直後に
# ログファイルをディレクトリへ置き換える。以後の追記が失敗する。
# 1 回目の直後に壊すと 2 回目の grep が読めずに probe-incomplete で先に返るため、
# 追記のガードへ到達しない。chmod ではなく置換にするのは root 実行でも効かせるため。
make_grep_stub() {
    local dir="$1" target="$2"
    cat > "$dir/bin/grep" <<STUB
#!/bin/bash
pat=""
for a in "\$@"; do
    case "\$a" in
        -*) ;;
        *) if [ -z "\$pat" ]; then pat="\$a"; fi ;;
    esac
done
$REAL_GREP "\$@"
rc=\$?
if [ "\$pat" = "MY_BENCH end" ]; then
    rm -rf "$target"
    mkdir -p "$target"
fi
exit "\$rc"
STUB
    chmod +x "$dir/bin/grep" || guard_fatal "$dir/bin/grep を実行可能にできません。"
    assert_stub "$dir/bin/grep"
}

#### 子ハーネスの実行 ####
# env のオプションは代入より前に置く。env FOO=1 -u BAR cmd は -u をコマンド名として
# 扱い rc=127 になる。
HARNESS_OUT=""
HARNESS_RC=0
run_harness() {
    local dir="$1"; shift
    HARNESS_OUT=""
    HARNESS_RC=0
    if HARNESS_OUT="$(env -u SCRIPT PATH="$dir/bin:$PATH" \
        EMACS_SETUP_TEST_SANDBOX=1 HOME="$dir/home" "$@" \
        "$MY_GUARD_HARNESS" 2>&1)"; then
        HARNESS_RC=0
    else
        HARNESS_RC=$?
    fi
}

# ガードが発火して途中で止まったことを検査する。
# Results: が出ていたら完走しており、ガードは効いていない。
expect_harness_abort() {
    local desc="$1"; shift
    local problems="" pat
    [ "$HARNESS_RC" -ne 0 ] || problems="$problems rc=0"
    case "$HARNESS_OUT" in
        *"Results:"*) problems="$problems 完走した" ;;
    esac
    for pat in "$@"; do
        case "$HARNESS_OUT" in
            *"$pat"*) ;;
            *) problems="$problems 文言なし($pat)" ;;
        esac
    done
    if [ -z "$problems" ]; then
        record_pass "$desc"
    else
        record_fail "$desc —$problems"
    fi
}

echo "=== A: test-emacs-setup.sh のガード ==="

# A1. 注入なしの対照。ここが落ちるなら以降の検査は意味を持たない。
A1_DIR="$(new_case_dir A1)" || guard_fatal "ケース領域を作成できません。"
run_harness "$A1_DIR"
a1_problems=""
[ "$HARNESS_RC" -eq 0 ] || a1_problems="$a1_problems rc=$HARNESS_RC"
case "$HARNESS_OUT" in
    *"0 failed"*) ;;
    *) a1_problems="$a1_problems 全件 PASS していない" ;;
esac
if [ -z "$a1_problems" ]; then
    record_pass "baseline harness passes without injection"
else
    record_fail "baseline harness passes without injection —$a1_problems"
fi

# A2a/A2b. md5sum の失敗を握り潰すと、fixture の md5 が空のまま
# "" = "" が成立して「ユーザーデータ保護」の検査が空虚に PASS する。
A2A_DIR="$(new_case_dir A2a)" || guard_fatal "ケース領域を作成できません。"
make_md5sum_stub "$A2A_DIR" 1
run_harness "$A2A_DIR"
expect_harness_abort "md5sum failure on savehist aborts the harness" \
    "FATAL: fixture の savehist を読めません。"

A2B_DIR="$(new_case_dir A2b)" || guard_fatal "ケース領域を作成できません。"
make_md5sum_stub "$A2B_DIR" 2
run_harness "$A2B_DIR"
expect_harness_abort "md5sum failure on backup aborts the harness" \
    "FATAL: fixture の backup/bk を読めません。"

# A3a/A3b. find の失敗を握り潰すと前後の件数がともに 0 になり、
# 一時ファイルが残っていても一致して PASS する。
A3A_DIR="$(new_case_dir A3a)" || guard_fatal "ケース領域を作成できません。"
make_find_stub "$A3A_DIR" 1
run_harness "$A3A_DIR"
expect_harness_abort "find failure before packing aborts the harness" \
    "FATAL: 一時ディレクトリ" "を走査できません。"

A3B_DIR="$(new_case_dir A3b)" || guard_fatal "ケース領域を作成できません。"
make_find_stub "$A3B_DIR" 2
run_harness "$A3B_DIR"
expect_harness_abort "find failure after packing aborts the harness" \
    "FATAL: 一時ディレクトリ" "を走査できません。"

# A4. --setup が失敗したまま続けると、空のログどうしを比較して gtk3 の検査が
# 空虚に PASS する。
A4_DIR="$(new_case_dir A4)" || guard_fatal "ケース領域を作成できません。"
run_harness "$A4_DIR" STUB_EXIT_SUDO=1
expect_harness_abort "failing --setup aborts before comparing gui logs" \
    "FATAL: --setup（既定）のスタブ実行に失敗しました。"

# A5. calls.log を読めなかった場合も同じく空文字が渡る。
A5_DIR="$(new_case_dir A5)" || guard_fatal "ケース領域を作成できません。"
make_cat_stub "$A5_DIR" fail
run_harness "$A5_DIR"
expect_harness_abort "unreadable calls.log aborts before comparing gui logs" \
    "FATAL: --setup（既定）のスタブ実行に失敗しました。"

# A6. cat が成功して空を返すと前段の 2 ガードは通過する。
# 比較前の非空検査が無ければ "" = "" が成立して PASS になる。
A6_DIR="$(new_case_dir A6)" || guard_fatal "ケース領域を作成できません。"
make_cat_stub "$A6_DIR" empty
run_harness "$A6_DIR"
a6_problems=""
case "$HARNESS_OUT" in
    *"FAIL: setup --gui gtk3 matches the default"*) ;;
    *) a6_problems="$a6_problems FAIL 行なし" ;;
esac
case "$HARNESS_OUT" in
    *"PASS: setup --gui gtk3 matches the default"*) a6_problems="$a6_problems 空ログで PASS した" ;;
esac
if [ -z "$a6_problems" ]; then
    record_pass "empty gui logs are rejected instead of matching"
else
    record_fail "empty gui logs are rejected instead of matching —$a6_problems"
fi

# A7a/A7b. harness_mktemp は終了コード検査の直後に非空検査を持つ。
# 片方だけを識別するには、それぞれ別の壊し方をする必要がある。
A7A_DIR="$(new_case_dir A7a)" || guard_fatal "ケース領域を作成できません。"
make_mktemp_stub "$A7A_DIR" rc
run_harness "$A7A_DIR"
expect_harness_abort "harness_mktemp propagates a nonzero mktemp" \
    "FATAL: 一時ディレクトリを作成できません。"

A7B_DIR="$(new_case_dir A7b)" || guard_fatal "ケース領域を作成できません。"
make_mktemp_stub "$A7B_DIR" empty
run_harness "$A7B_DIR"
expect_harness_abort "harness_mktemp rejects an empty path" \
    "FATAL: 一時ディレクトリを作成できません。"

# A8/A9. 呼び出し側のガードは注入では 1 つ目にしか到達できないため、
# ソースを走査して全呼び出しが受けられていることを検査する。
# 行継続を連結し、コメント行は除外する。
# 呼び出しの目印はリテラルで渡し、awk 側では index() で照合する。
# 正規表現として渡すと -v の文字列エスケープでバックスラッシュが失われ、
# $( が不正な正規表現になる。
assert_guarded_callsites() {
    local desc="$1" needle="$2" file="$3"
    local report total bad
    if ! report="$(awk -v needle="$needle" '
        /\\$/ { acc = acc substr($0, 1, length($0) - 1); next }
        {
            line = acc $0
            acc = ""
            if (line ~ /^[ \t]*#/) next
            if (index(line, needle) > 0) {
                total++
                if (index(line, "harness_fatal") == 0) bad++
            }
        }
        END { printf "%d %d\n", total + 0, bad + 0 }
    ' "$file")"; then
        record_fail "$desc — ソースを走査できません"
        return
    fi
    total="${report% *}"
    bad="${report#* }"
    if [ -z "$total" ] || [ -z "$bad" ]; then
        record_fail "$desc — 走査結果を解釈できません [$report]"
    elif [ "$total" -lt 1 ]; then
        record_fail "$desc — 呼び出しが 1 件も見つかりません（検査が空振りしている）"
    elif [ "$bad" -ne 0 ]; then
        record_fail "$desc — $bad/$total 件が harness_fatal で受けられていません"
    else
        record_pass "$desc ($total 件)"
    fi
}

# shellcheck disable=SC2016  # 検索する目印はリテラル。展開させてはならない
assert_guarded_callsites "every harness_mktemp callsite is guarded" \
    '$(harness_mktemp)' "$MY_GUARD_HARNESS"
# shellcheck disable=SC2016  # 同上
assert_guarded_callsites "every setup_calls_for_gui callsite is guarded" \
    '$(setup_calls_for_gui' "$MY_GUARD_HARNESS"

echo ""
echo "=== B: make lint の環境分離 ==="

mapfile -t SH_SOURCES < <(git ls-files -- '*.sh')
[ "${#SH_SOURCES[@]}" -gt 0 ] || guard_fatal "検査対象のシェルスクリプトが見つかりません。"

# rc ファイルの探索は検査対象ファイルのディレクトリから上へ辿り、見つからないときだけ
# $HOME へ落ちる。祖先に 1 つでもあると B3/B4 が $HOME のレバーを検証していないことになる。
# 祖先探索はドット無しの shellcheckrc も読む（$HOME フォールバックは .shellcheckrc のみ）。
RC_INSIDE=""
RC_OUTSIDE=""
scan_rc_files() {
    local src dir name
    for src in "${SH_SOURCES[@]}"; do
        dir="$(cd "$(dirname "$src")" && pwd)" || return 1
        while :; do
            for name in .shellcheckrc shellcheckrc; do
                if [ -e "$dir/$name" ]; then
                    case "$dir/" in
                        "$REPO_ROOT"/*) RC_INSIDE="$RC_INSIDE $dir/$name" ;;
                        *) RC_OUTSIDE="$RC_OUTSIDE $dir/$name" ;;
                    esac
                fi
            done
            [ "$dir" = / ] && break
            dir="$(dirname "$dir")" || return 1
        done
    done
}
if ! scan_rc_files; then
    guard_fatal "shellcheckrc の祖先走査に失敗しました。"
fi

if [ -n "$RC_INSIDE" ]; then
    record_fail "no shellcheckrc is committed under the repository —$RC_INSIDE"
else
    record_pass "no shellcheckrc is committed under the repository"
fi

LINT_OUT=""
LINT_RC=0
# 再帰 make の jobserver 警告を避けるため MAKEFLAGS 系を落とす。
run_make_lint_sh() {
    LINT_OUT=""
    LINT_RC=0
    if LINT_OUT="$(env -u MAKEFLAGS -u MFLAGS -u MAKELEVEL "$@" \
        "$MAKE_CMD" lint-sh 2>&1)"; then
        LINT_RC=0
    else
        LINT_RC=$?
    fi
}

SHELLCHECK_OUT=""
SHELLCHECK_RC=0
run_shellcheck() {
    SHELLCHECK_OUT=""
    SHELLCHECK_RC=0
    if SHELLCHECK_OUT="$("$@" 2>&1)"; then
        SHELLCHECK_RC=0
    else
        SHELLCHECK_RC=$?
    fi
}

# 正の対照。レバーが実在することを確かめないと、B2/B4 が空虚に PASS しうる。
# 終了コードは診断があると 1、実行エラーだと 2 なので 1 ちょうどを要求し、
# optional チェックの署名（SC2250）が出ていることも確かめる。
# 行頭を "# shellcheck" で始めるとディレクティブとして解釈されるため避けている。
expect_shellcheck_lever() {
    local desc="$1"
    local problems=""
    [ "$SHELLCHECK_RC" -eq 1 ] || problems="$problems rc=$SHELLCHECK_RC(期待 1)"
    case "$SHELLCHECK_OUT" in
        *SC2250*) ;;
        *) problems="$problems SC2250 が出ていない" ;;
    esac
    if [ -z "$problems" ]; then
        record_pass "$desc"
    else
        record_fail "$desc —$problems"
    fi
}

run_shellcheck env -u SHELLCHECK_OPTS shellcheck --norc -x --enable=all "${SH_SOURCES[@]}"
expect_shellcheck_lever "control: --enable=all does report optional findings"

run_make_lint_sh SHELLCHECK_OPTS=--enable=all
if [ "$LINT_RC" -eq 0 ]; then
    record_pass "lint-sh ignores SHELLCHECK_OPTS"
else
    record_fail "lint-sh ignores SHELLCHECK_OPTS (rc=$LINT_RC) — env -u SHELLCHECK_OPTS が外れている可能性がある: $LINT_OUT"
fi

RC_HOME="$GUARD_ROOT/rc-home"
mkdir -p "$RC_HOME" || guard_fatal "$RC_HOME を作成できません。"
printf 'enable=all\n' > "$RC_HOME/.shellcheckrc" \
    || guard_fatal "$RC_HOME/.shellcheckrc を作成できません。"

if [ -n "$RC_OUTSIDE" ]; then
    # 利用者の正当な設定である。make lint はまさにそれから独立するために --norc を
    # 付けているので、ここで make test 全体を落とすのは筋が違う。
    record_skip "control: \$HOME/.shellcheckrc is read — リポジトリ外の祖先に設定がある:$RC_OUTSIDE"
    record_skip "lint-sh ignores \$HOME/.shellcheckrc — 同上（この環境では --norc のレバーを検証できない）"
else
    run_shellcheck env -u SHELLCHECK_OPTS HOME="$RC_HOME" shellcheck -x "${SH_SOURCES[@]}"
    expect_shellcheck_lever "control: \$HOME/.shellcheckrc is read"

    run_make_lint_sh HOME="$RC_HOME"
    if [ "$LINT_RC" -eq 0 ]; then
        record_pass "lint-sh ignores \$HOME/.shellcheckrc"
    else
        record_fail "lint-sh ignores \$HOME/.shellcheckrc (rc=$LINT_RC) — --norc が外れている可能性がある: $LINT_OUT"
    fi
fi

echo ""
echo "=== C: my-bench-run.sh の run_trial ==="

BENCH_SKIP=""
if [ "$(uname)" != Linux ]; then
    BENCH_SKIP="Linux ではない"
else
    for cmd in script timeout stty; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            BENCH_SKIP="$cmd が無い"
            break
        fi
    done
fi

TRIAL_OUT=""
TRIAL_RC=0
# run_trial を library モードで直接呼ぶ。BENCH_ROOT は実在しない場所を指し、
# 実ベンチが起動しないことを二重に担保する（timeout スタブが先に受ける）。
# 第 3 引数を closed にすると stdout を閉じて呼び、最終出力の失敗を再現する。
run_bench_trial() {
    local dir="$1" maxv="${2:-999}" stdout_mode="${3:-open}"
    TRIAL_OUT=""
    TRIAL_RC=0
    if TRIAL_OUT="$(
        # shellcheck disable=SC1090  # 検査対象を差し替えられるよう変数で source する
        MY_BENCH_LIB_ONLY=1 . "$MY_GUARD_BENCH" now 1 "$dir/out" >/dev/null 2>&1
        # shellcheck disable=SC2030,SC2031  # スタブをこのサブシェル内だけへ効かせるのが目的
        PATH="$dir/bin:$PATH"
        # shellcheck disable=SC2034  # source した run_trial が参照する
        BENCH_ROOT="$dir/absent"
        # shellcheck disable=SC2034  # 同上
        LOADAVG_MAX="$maxv"
        # stderr は判定に使わないが、握り潰さずケース領域へ残す
        # （C10 のログ破壊で出る診断がサマリへ混ざるのを防ぐ）。
        if [ "$stdout_mode" = closed ]; then
            run_trial 1 >&- 2>"$dir/trial.err"
        else
            run_trial 1 2>"$dir/trial.err"
        fi
    )"; then
        TRIAL_RC=0
    else
        TRIAL_RC=$?
    fi
}

expect_trial_invalid() {
    local desc="$1"
    if [ "$TRIAL_RC" -ne 0 ]; then
        record_pass "$desc"
    else
        record_fail "$desc — 有効試行として rc=0 を返した (出力=[$TRIAL_OUT])"
    fi
}

# awk 注入のケース。1 回目が loadavg の取得、2 回目が閾値判定にあたる。
bench_case_awk() {
    local desc="$1" name="$2" first="$3" second="$4"
    local dir
    dir="$(new_case_dir "$name")" || guard_fatal "ケース領域を作成できません。"
    make_timeout_stub "$dir"
    make_plan_stub "$dir" awk "$REAL_AWK" "$first" "$second"
    run_bench_trial "$dir"
    expect_trial_invalid "$desc"
}

# date 注入のケース。1 回目が start、2 回目が end にあたる。
bench_case_date() {
    local desc="$1" name="$2" first="$3" second="$4"
    local dir
    dir="$(new_case_dir "$name")" || guard_fatal "ケース領域を作成できません。"
    make_timeout_stub "$dir"
    make_plan_stub "$dir" date "$REAL_DATE" "$first" "$second"
    run_bench_trial "$dir"
    expect_trial_invalid "$desc"
}

if [ -n "$BENCH_SKIP" ]; then
    for desc in \
        "run_trial rejects a failing loadavg awk" \
        "run_trial rejects an empty loadavg" \
        "run_trial rejects a non-numeric LOADAVG_MAX" \
        "run_trial rejects a dot-only loadavg" \
        "run_trial rejects a failing verdict awk" \
        "run_trial rejects a failing start date" \
        "run_trial rejects a failing end date" \
        "run_trial rejects a non-numeric start date" \
        "run_trial rejects a non-numeric end date" \
        "run_trial rejects a decreasing clock" \
        "run_trial rejects an unwritable trial log" \
        "control: run_trial accepts a healthy trial" \
        "run_trial rejects a closed stdout" \
        "run_trial rejects a high load verdict" \
        "run_trial rejects an unknown verdict"
    do
        record_skip "$desc — $BENCH_SKIP"
    done
else
    bench_case_awk "run_trial rejects a failing loadavg awk" C1 '0.1|1' 'ok|0'
    bench_case_awk "run_trial rejects an empty loadavg" C2 '@EMPTY@|0' 'ok|0'

    C3_DIR="$(new_case_dir C3)" || guard_fatal "ケース領域を作成できません。"
    make_timeout_stub "$C3_DIR"
    run_bench_trial "$C3_DIR" abc
    expect_trial_invalid "run_trial rejects a non-numeric LOADAVG_MAX"

    bench_case_awk "run_trial rejects a dot-only loadavg" C4 '.|0' 'ok|0'
    bench_case_awk "run_trial rejects a failing verdict awk" C5 '0.1|0' 'ok|1'

    bench_case_date "run_trial rejects a failing start date" C6 '1000000000000|1' '2000000000000|0'
    bench_case_date "run_trial rejects a failing end date" C7 '1000000000000|0' '2000000000000|1'
    # +N は正規表現が拒否する一方で [ -ge ] も $(( )) も受理するため、
    # 数値検査を外すと最後まで通って rc=0 になる（判別力がある）。
    bench_case_date "run_trial rejects a non-numeric start date" C8a '+1000000000|0' '2000000000000|0'
    bench_case_date "run_trial rejects a non-numeric end date" C8b '1000000000000|0' '+2000000000000|0'
    bench_case_date "run_trial rejects a decreasing clock" C9 '2000000000000|0' '1000000000000|0'

    C10_DIR="$(new_case_dir C10)" || guard_fatal "ケース領域を作成できません。"
    make_timeout_stub "$C10_DIR"
    make_grep_stub "$C10_DIR" "$C10_DIR/out/raw/now-1.log"
    run_bench_trial "$C10_DIR"
    expect_trial_invalid "run_trial rejects an unwritable trial log"

    C11_DIR="$(new_case_dir C11)" || guard_fatal "ケース領域を作成できません。"
    make_timeout_stub "$C11_DIR"
    run_bench_trial "$C11_DIR"
    c11_problems=""
    [ "$TRIAL_RC" -eq 0 ] || c11_problems="$c11_problems rc=$TRIAL_RC"
    case "$TRIAL_OUT" in
        ''|*[!0-9]*) c11_problems="$c11_problems 経過ミリ秒が数値でない([$TRIAL_OUT])" ;;
    esac
    if [ -z "$c11_problems" ]; then
        record_pass "control: run_trial accepts a healthy trial"
    else
        record_fail "control: run_trial accepts a healthy trial —$c11_problems"
    fi

    # 最終出力の失敗。現状は最終コマンドの終了ステータスが伝播するため mutation では
    # 区別できない契約テストである。printf の後ろへ行が足されたときの退行を固定する。
    C12_DIR="$(new_case_dir C12)" || guard_fatal "ケース領域を作成できません。"
    make_timeout_stub "$C12_DIR"
    run_bench_trial "$C12_DIR" 999 closed
    expect_trial_invalid "run_trial rejects a closed stdout"

    bench_case_awk "run_trial rejects a high load verdict" C13 '0.1|0' 'high|0'
    bench_case_awk "run_trial rejects an unknown verdict" C14 '0.1|0' 'weird|0'
fi

echo ""
echo "================================"
printf 'Results: %d passed, %d failed, %d skipped\n' "$PASS" "$FAIL" "$SKIP"
[ "$FAIL" -eq 0 ] || exit 1
exit 0
