# my-test-sandbox-guard.sh — テスト用サンドボックス判定
# shellcheck shell=bash
#
# emacs-setup.sh の --clean / --clean-all / --extract-package は
# $HOME/.emacs.d 配下を破壊的に操作する。テストからこれらを呼ぶ前に、
# 現在の $HOME が使い捨てのサンドボックスであることを確認する。
#
# このファイルは source して使う。副作用は持たせない。
#
# 【重要】呼び出し元（test-emacs-setup.sh）は set -e を使えないため
# （((PASS++)) が PASS=0 のとき終了ステータス 1 を返す）、
# ここでは各コマンドの終了ステータスと結果の非空を個別に検査する。
# 判定できない場合は必ず拒否側（fail-closed）へ倒す。

my_test_guard__reject() {
    printf 'test-emacs-setup.sh: %s\n' "$1" >&2
    return 1
}

# 現在のユーザーの登録済みホームディレクトリを stdout へ出す。
# 取得できなければ何も出さずに非ゼロを返す。
my_test_guard__real_home() {
    local uid uname_ passwd_line dscl_out home_dir

    uid="$(id -u 2>/dev/null)" || return 1
    [ -n "$uid" ] || return 1

    # 取得と解析は必ず分ける。パイプで繋ぐと pipefail の無い呼び出し元では
    # 前段が非ゼロ終了しても後段の成功で全体が成功扱いになる。
    if command -v getent >/dev/null 2>&1; then
        passwd_line="$(getent passwd "$uid" 2>/dev/null)" || return 1
        [ -n "$passwd_line" ] || return 1
        home_dir="$(printf '%s\n' "$passwd_line" | cut -d: -f6)" || return 1
    elif command -v dscl >/dev/null 2>&1; then
        # macOS には getent が無い。出力は "NFSHomeDirectory: /Users/foo" 形式。
        uname_="$(id -un 2>/dev/null)" || return 1
        [ -n "$uname_" ] || return 1
        dscl_out="$(dscl . -read "/Users/$uname_" NFSHomeDirectory 2>/dev/null)" || return 1
        [ -n "$dscl_out" ] || return 1
        home_dir="$(printf '%s\n' "$dscl_out" | awk '/^NFSHomeDirectory:/ {print $2}')" || return 1
    else
        return 1
    fi

    [ -n "$home_dir" ] || return 1
    printf '%s\n' "$home_dir"
}

# パスを物理パスへ正規化して stdout へ出す。失敗したら非ゼロを返す。
my_test_guard__realpath() {
    local resolved
    resolved="$(cd "$1" 2>/dev/null && pwd -P)" || return 1
    [ -n "$resolved" ] || return 1
    printf '%s\n' "$resolved"
}

# サンドボックスでの実行でなければ非ゼロを返す。
# マーカーと実ホーム判定は独立した AND 条件であり、
# マーカーを立てても実ホーム上での実行は許可しない。
require_test_sandbox() {
    local real_home home_real real_home_real sub sub_real

    if [ "${EMACS_SETUP_TEST_SANDBOX:-}" != "1" ]; then
        my_test_guard__reject \
            "EMACS_SETUP_TEST_SANDBOX=1 が必要です。破壊的な操作を含むため make test-setup 経由で実行してください。"
        return 1
    fi

    if [ -z "${HOME:-}" ]; then
        my_test_guard__reject "HOME が未設定です。"
        return 1
    fi

    if ! real_home="$(my_test_guard__real_home)"; then
        my_test_guard__reject \
            "登録済みホームディレクトリを特定できないため、安全のため中止します。"
        return 1
    fi

    if ! home_real="$(my_test_guard__realpath "$HOME")"; then
        my_test_guard__reject "HOME ($HOME) を正規化できません。"
        return 1
    fi

    if ! real_home_real="$(my_test_guard__realpath "$real_home")"; then
        my_test_guard__reject "登録済みホーム ($real_home) を正規化できません。"
        return 1
    fi

    if [ "$home_real" = "$real_home_real" ]; then
        my_test_guard__reject \
            "HOME ($home_real) が登録済みホームと一致します。サンドボックスでの実行ではありません。"
        return 1
    fi

    # サンドボックス内のディレクトリが実ホーム配下を指していると、
    # HOME 差し替えを迂回して実データへ到達してしまう。
    # テストは .emacs.d だけでなく .local（$EMACS_INSTALL_PREFIX の親）も削除するため、
    # 両方を検査する。
    for sub in .emacs.d .local; do
        if [ -e "$HOME/$sub" ] || [ -L "$HOME/$sub" ]; then
            if ! sub_real="$(my_test_guard__realpath "$HOME/$sub")"; then
                my_test_guard__reject "$HOME/$sub を正規化できません。"
                return 1
            fi
            case "$sub_real" in
                "$real_home_real"|"$real_home_real"/*)
                    my_test_guard__reject \
                        "$HOME/$sub が実ホーム配下 ($sub_real) を指しています。"
                    return 1
                    ;;
            esac
        fi
    done

    return 0
}
