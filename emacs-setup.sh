#!/bin/bash

# -E は現時点では no-op だが、将来 ERR トラップを足したときに
# 関数・コマンド置換・サブシェル内で発火させるために必要になる。
set -Eeuo pipefail

##### 設定 #####
EMACS_DIR="$HOME/.emacs.d"
LOADS_DIR="$EMACS_DIR/loads"
PACKAGE_DIR="straight"
# install-emacs
DL_DIR="$HOME/.local/downloads"
EMACS_SRC_DIR="$DL_DIR/emacs"
EMACS_INSTALL_PREFIX="$HOME/.local"
# clean
VAR_DIR="$EMACS_DIR/var"
# packing/extract_package
PACKAGE_ARCHIVE="$EMACS_DIR/package.tar.gz"
PACKAGE_TARGET=("repos" "versions/default.el")
# 取得元。環境変数で上書きできる。
# 接頭辞は Emacs 本体の EMACS_UNIBYTE 等と衝突しないよう EMACS_SETUP_ で揃える。
EMACS_SETUP_MIRROR_URL="${EMACS_SETUP_MIRROR_URL:-https://ftp.jaist.ac.jp/pub/GNU/emacs}"
EMACS_SETUP_UPSTREAM_URL="${EMACS_SETUP_UPSTREAM_URL:-https://ftp.gnu.org/gnu/emacs}"
# 一覧の取得先はダウンロード元と別変数にする。curl は file:// でディレクトリの
# 中身を返さないため、テストからファイルを直接指定できる必要がある。
EMACS_SETUP_INDEX_URL="${EMACS_SETUP_INDEX_URL:-$EMACS_SETUP_MIRROR_URL/}"


##### バリデーション #####
validate_version() {
    local ver="$1"
    if [[ ! "$ver" =~ ^[0-9]+\.[0-9]+(\.[0-9]+)?$ ]]; then
        echo "Error: Invalid version format '$ver'. Expected format: NN.N or NN.N.N (e.g., 30.1, 29.4.1)"
        exit 1
    fi
}

##### ヘルプ #####
usage() {
    cat << EOF
Usage: $0 [options]...

Options:
  -s, --setup               Install required dependencies for Emacs.
  -l, --list                List available Emacs versions for installation.
  -i <ver>, --install <ver> [--gui <gtk3|lucid|pgtk|no>]
                            Install Emacs <ver> with optional GUI backend.
  -u, --uninstall           Uninstall the locally installed Emacs.
  -c, --clean               Remove Emacs auto generated files (excluding packages).
  -C, --clean-all           Remove all Emacs auto generated files (including packages).
  -p, --packing-package     Archive the package directory ($PACKAGE_DIR).
  -x, --extract-package     Extract the package archive to .emacs.d/loads/$PACKAGE_DIR.
  -h, --help                Show this help message.

Examples:
  $0 --setup
  $0 --list
  $0 --install 30.1              # Install Emacs version 30.1.
  $0 --uninstall
  $0 --clean
  $0 --clean-all
  $0 --packing-package
  $0 --extract-package
EOF
    exit "${1:-1}"
}

##### GUI オプションの検証 #####
# --install と --setup で同じ語彙・同じ既定値を使う。
validate_gui_toolkit() {
    case "$1" in
        gtk3|lucid|pgtk|no) return 0 ;;
        *)
            echo "Unsupported GUI type: $1" >&2
            echo "Supported GUI types: gtk3 (default), lucid, pgtk, no" >&2
            exit 1
            ;;
    esac
}

##### 関連パッケージインストール #####
# GUI_PACKAGES を入れるかどうかだけが分岐点になる。gtk3 / lucid / pgtk の間に
# パッケージの差は無いため、実質は no かそれ以外かの二値である。
setup_env() {
    local gui="${1:-gtk3}"
    validate_gui_toolkit "$gui"

    echo "Setting up Emacs environment..."

    export DEBIAN_FRONTEND=noninteractive # Set non-interactive mode for apt-get

    sudo apt-get update

    ##### Emacs インストール時に必要なパッケージのインストール

    ### 両方の環境 (GUI & TUI) で必要なもの
    ## 必須（build-essential を先にインストールして GCC を確保）
    sudo apt-get install -y build-essential autoconf automake texinfo git libtool

    ## GCC バージョン検出（libgccjit 用）
    # gcc は build-essential でここまでに入る。事前チェックには含めない。
    local GCC_VERSION
    GCC_VERSION=$(gcc -dumpversion | cut -d. -f1)
    echo "Detected GCC version: $GCC_VERSION"

    # libgccjit は gcc とバージョンを揃える必要がある。
    # 欠落したまま続行すると install_emacs の --with-native-compilation が
    # 無条件のため、setup の成功が後のビルド失敗に化ける。ここで止める。
    if ! apt-cache policy "libgccjit-${GCC_VERSION}-dev" 2>/dev/null | grep -q '^libgccjit-'; then
        echo "Error: libgccjit-${GCC_VERSION}-dev が見つかりません。" >&2
        echo "       --with-native-compilation には gcc と同じバージョンの libgccjit が必要です。" >&2
        echo "       利用可能な候補:" >&2
        apt-cache search '^libgccjit-[0-9]+-dev$' >&2 || true
        exit 1
    fi

    ## 推奨
    # libm17n-dev              — 多言語テキスト処理（必要に応じて有効化）
    # libsystemd-dev           — systemd の統合（必要に応じて有効化）
    local COMMON_PACKAGES=(
        pkg-config                      # C/C++ プロジェクトのライブラリ依存管理ツール
        "libgccjit-${GCC_VERSION}-dev"  # ネイティブコンパイル用 (Emacs29以降)
        libgnutls28-dev                 # TLS (HTTPS/SSL) サポート。GUI 非依存
        libsqlite3-dev                  # SQLite バックエンド (Org-roam など)
        libtree-sitter-dev              # Tree-sitter (シンタックスハイライト)
        libxml2-dev                     # XML パース (shr.el, EWW)
        libdbus-1-dev                   # DBus 通信用ライブラリ
        zlib1g zlib1g-dev               # 圧縮ライブラリ (gzip など)
        libacl1-dev                     # POSIX ACL（アクセス制御リスト)サポート
        libp11-kit-dev                  # GnuTLS が証明書ストアを扱うための共通ライブラリ
    )

    ### GUI (X11/GTK) で必要なもの
    local GUI_PACKAGES=(
        ## 必須
        libgtk-3-dev                    # GTK3 ベースの GUI サポート
        libfreetype6-dev                # フォントサポート
        libotf-dev                      # Opentype フォント処理のサポート
        adwaita-icon-theme              # Icon
        hicolor-icon-theme              # Icon
        gnome-icon-theme                # Icon
        # X11 関連
        libx11-dev                      # X Window System の基本ライブラリ
        libxmu-dev                      # X11 のユーティリティライブラリ
        xorg-dev                        # X11 開発パッケージ
        libxfixes-dev                   # X11 の細かい修正拡張
        libxft-dev                      # フォント描画サポート
        libxkbcommon-dev                # キーボード入力処理
        libxrandr-dev                   # 画面サイズ変更のサポート
        libxt-dev                       # X Toolkit サポート
        ## 推奨
        libjpeg-dev                     # JPEG 画像のサポート
        libgif-dev                      # GIF 画像のサポート
        libpng-dev                      # PNG 画像のサポート
        libtiff-dev                     # TIFF 画像のサポート
        librsvg2-dev                    # SVG 画像のサポート
        libxpm-dev                      # XPM 画像のサポート
        libxaw7-dev                     # Xaw3d 用 (GUI の一部)
        libharfbuzz-dev                 # 高品質なフォントレンダリング
        libxcomposite-dev               # GTK3 での合成描画
        libmagickwand-dev               # ImageMagick の C API
        libxi-dev                       # 入力拡張 (XInput) 用のライブラリ
        libcairo2-dev                   # 2D グラフィックス (cairo 描画)
        liblcms2-dev                    # カラーマネジメント
        libwebp-dev                     # WebP 画像サポート
        # PGTK 用
        libgtk-4-dev                    # PGTK 用
    )

    ### ターミナル (TUI) で必要なもの
    local TUI_PACKAGES=(
        ## 必須
        libncurses-dev                  # ターミナルでのテキスト UI 提供ライブラリ
        libgpm-dev                      # General Purpose Mouse による端末でのマウスサポート
        ## 推奨
        libjansson-dev                  # JSON パース (LSP・eglot 用)
    )

    ##### Emacs 利用時に必要なパッケージのインストール
    local EMACS_TOOL_PACKAGES=(
        clang libclang-dev              # Clang 用
        elpa-color-theme-modern         # カラーテーマ用
        fonts-ricty-diminished          # フォント用
        global                          # GTAGS (ソースコード検索ツール)
        cmigemo                         # Migemo (ローマ字で日本語検索)
        hunspell hunspell-en-us         # Hunspell (スペルチェック)
        aspell aspell-en                # Aspell (スペルチェック)
        cmake llvm                      # CMake and LLVM (irony-install-server 用)
        pandoc                          # Pandoc (Markdown 用)
    )

    sudo apt-get install -y "${COMMON_PACKAGES[@]}"
    if [ "$gui" = no ]; then
        echo "GUI パッケージをスキップします (--gui no)。"
    else
        sudo apt-get install -y "${GUI_PACKAGES[@]}"
    fi
    sudo apt-get install -y "${TUI_PACKAGES[@]}"
    sudo apt-get install -y "${EMACS_TOOL_PACKAGES[@]}"

    echo "Emacs setup-env complete."
}

##### インストール可能な Emacs バージョンを取得 #####
# 標準入力の HTML からバージョンを抽出する。ネットワークに依存しないため
# fixture を使ったオフラインテストができる。
#
# 正規表現の要点:
#   - \K と先読みを使う。grep -oP は capture group を出力へ反映しないため、
#     グループ化しても "30.2.tar.gz" のように余計な文字が残ってしまう。
#   - .tar.gz へ直結する形だけを許す。emacs-21.4a のような文字サフィックス版を
#     "21.4" と誤抽出すると、どのミラーにも存在しない幽霊バージョンが一覧に出る。
#     validate_version も英字付きを弾くため、一覧から除外するのが整合する。
parse_emacs_version_list() {
    local versions
    # grep が 0 件マッチで 1 を返すと pipefail によりパイプライン全体が失敗する。
    # 裸の代入は errexit の免除対象ではないため、|| true を付けないと
    # 下の空チェックへ到達せずスクリプトが無言で終了する。
    versions=$(grep -oP 'emacs-\K[0-9]+\.[0-9]+(?:\.[0-9]+)?(?=\.tar\.gz)' \
        | sort -V | uniq) || true
    if [ -z "$versions" ]; then
        return 1
    fi
    printf '%s\n' "$versions"
}

list_emacs_versions() {
    echo "Fetching available Emacs versions..." >&2
    local html
    if ! html=$(curl -sf --connect-timeout 10 --max-time 60 "$EMACS_SETUP_INDEX_URL"); then
        echo "Error: バージョン一覧の取得に失敗しました。" >&2
        exit 1
    fi
    if ! printf '%s\n' "$html" | parse_emacs_version_list; then
        echo "Error: バージョン情報を抽出できませんでした（ページ形式が変わった可能性があります）。" >&2
        exit 1
    fi
}

##### tarball の取得 #####
# ミラーで失敗したら upstream へ 1 回だけフォールバックする。
# 404 は「そのホストに存在しない」ことを意味するため同一ホストへは再試行しない。
#
# ダウンロードは .part へ書いてから mv で確定させる。中断すると途中まで
# 書かれたファイルが残り、次回以降の [ -f "$tar_file" ] がそれを完全な
# アーカイブとみなして展開に失敗し続けるため。
#
# wget と mv はいずれも if で明示的に分岐させる。A && B 形式にすると
# A の失敗が errexit の免除対象になり、失敗が素通りしてしまう。
download_emacs_tarball() {
    local tar_file="$1"
    local part="$tar_file.part"

    if [ -f "$tar_file" ]; then
        return 0
    fi

    rm -f "$part"
    if ! wget --timeout=30 --tries=2 -O "$part" "$EMACS_SETUP_MIRROR_URL/$tar_file"; then
        echo "Warning: ミラーからの取得に失敗しました。upstream へフォールバックします。" >&2
        rm -f "$part"
        if ! wget --timeout=30 --tries=2 -O "$part" "$EMACS_SETUP_UPSTREAM_URL/$tar_file"; then
            rm -f "$part"
            echo "Error: ダウンロードに失敗しました（ミラー・upstream 共に失敗）。" >&2
            exit 1
        fi
    fi

    if ! mv "$part" "$tar_file"; then
        rm -f "$part"
        echo "Error: ダウンロード結果の確定に失敗しました。" >&2
        exit 1
    fi
}

##### Emacs インストール #####
# 参考: https://myemacs.readthedocs.io/ja/latest/build.html
install_emacs() {
    local VERSION="$1"
    local GUI="${2:-gtk3}"  # GUIオプション、デフォルトは gtk3
    [ -z "$VERSION" ] && { echo "Error: No Emacs version specified."; usage; }
    [ -d "$EMACS_SRC_DIR" ] && { echo "Emacs is already installed. Run 'uninstall' first."; exit 1; }

    echo "Installing Emacs $VERSION ..."

    # ソースコード取得
    mkdir -p "$DL_DIR"
    cd "$DL_DIR"
    TAR_FILE="emacs-$VERSION.tar.gz"
    download_emacs_tarball "$TAR_FILE"

    tar xvf "$TAR_FILE" --transform="s/^emacs-$VERSION/emacs/"

    # ビルドとインストール
    cd emacs
    ./autogen.sh

    local -a CONFIG_OPTS=(
        "--prefix=$EMACS_INSTALL_PREFIX"
        "--with-native-compilation"
        "--with-json"
        "--with-tree-sitter"
        "--with-modules"
        "--without-pop"
    )
    case "$GUI" in
        gtk3)
            CONFIG_OPTS+=("--with-x" "--with-x-toolkit=gtk3")
            ;;
        lucid)
            CONFIG_OPTS+=("--with-x" "--with-x-toolkit=lucid")
            ;;
        pgtk)
            CONFIG_OPTS+=("--with-pgtk")
            ;;
        no)
            CONFIG_OPTS+=("--without-x")
            ;;
        *)
            validate_gui_toolkit "$GUI"  # ここへは来ないが、値の集合を一箇所に保つ
            ;;
    esac
    ./configure "${CONFIG_OPTS[@]}"
    make -j"$(nproc)"
    make install

    echo "Emacs $VERSION installation complete."
}

##### Emacs アンインストール #####
# make uninstall が失敗したときに削除する既知のインストール成果物。
# ディレクトリ削除だけでは emacsclient / etags / ctags / ebrowse や
# man・info が残るため、名前を列挙して掃除する。
uninstall_fallback_cleanup() {
    local prefix="$EMACS_INSTALL_PREFIX"
    local name info_name

    for name in emacs emacsclient etags ctags ebrowse; do
        rm -f "$prefix/bin/$name"
        rm -f "$prefix/share/man/man1/$name.1" "$prefix/share/man/man1/$name.1.gz"
    done
    # emacs-30.2 のようなバージョン付きバイナリ
    rm -f "$prefix"/bin/emacs-[0-9]*

    # info は他のソフトウェアの成果物と同居しうるため、
    # ソースツリーが持つファイル名だけを対象にする。
    if [ -d "$EMACS_SRC_DIR/info" ]; then
        for info_name in "$EMACS_SRC_DIR"/info/*; do
            [ -e "$info_name" ] || continue
            info_name="$(basename "$info_name")"
            rm -f "$prefix/share/info/$info_name" "$prefix/share/info/$info_name.gz"
        done
    fi
}

uninstall_emacs() {
    echo "Uninstalling Emacs..."
    [ ! -d "$EMACS_SRC_DIR" ] && { echo "No Emacs installation found."; exit 0; }

    # make uninstall の失敗で処理を止めない。止めるとソースツリーが残り、
    # install_emacs の [ -d "$EMACS_SRC_DIR" ] に阻まれて install も uninstall も
    # 通らない状態になる。失敗は終了コードで伝える。
    local rc=0
    ( cd "$EMACS_SRC_DIR" && make uninstall ) || rc=$?

    if [ "$rc" -ne 0 ]; then
        echo "Warning: make uninstall が失敗しました (exit=$rc)。既知の成果物を削除します。" >&2
        uninstall_fallback_cleanup
    fi

    # make uninstall で削除されないディレクトリの手動削除
    rm -rf "${EMACS_INSTALL_PREFIX:?}"/{bin,share,libexec,lib,include}/emacs

    rm -rf "${EMACS_SRC_DIR:?}"

    if [ "$rc" -ne 0 ]; then
        echo "Error: アンインストールは不完全な可能性があります。$EMACS_INSTALL_PREFIX 配下を手動で確認してください。" >&2
        exit 1
    fi

    echo "Emacs uninstallation complete."
}

##### Emacs クリーンアップ（パッケージを除外） #####
clean() {
    echo "Cleaning Emacs auto created files (excluding packages)..."
    if [ -d "$VAR_DIR" ]; then
        echo "Removing $VAR_DIR ..."
        rm -rf "$VAR_DIR"
    fi
    echo "Emacs clean complete."
}

##### Emacs 完全クリーンアップ（パッケージ含む） #####
clean_all() {
    echo "Cleaning all Emacs-related files, including packages..."
    clean
    if [ -d "$LOADS_DIR/$PACKAGE_DIR" ]; then
        echo "Removing $LOADS_DIR/$PACKAGE_DIR ..."
        rm -rf "${LOADS_DIR:?}/${PACKAGE_DIR:?}"
    fi
    echo "Emacs full clean complete."
}

##### パッケージビルド #####
run_package_build() {
    emacs --batch \
        --eval "(setq user-emacs-directory \"$EMACS_DIR\")" \
        -l "$EMACS_DIR/early-init.el" \
        -l "$EMACS_DIR/init.el" \
        -f straight-rebuild-all
}

##### パッケージディレクトリの圧縮 #####
packing_package() {
    if [ -d "$LOADS_DIR/$PACKAGE_DIR" ]; then
        echo "Archiving package directory..."

        # 一時リストファイルの作成
        local TMP_LIST
        TMP_LIST=$(mktemp)

        # `PACKAGE_TARGET` はすべて揃っていることを要求する。
        # 一部だけのアーカイブを許すと、展開側が完全性を要求するため
        # 「自分で作ったアーカイブを復元できない」組み合わせが生まれる。
        for target in "${PACKAGE_TARGET[@]}"; do
            if [ ! -e "$LOADS_DIR/$PACKAGE_DIR/$target" ]; then
                rm -f "$TMP_LIST"
                echo "Error: $PACKAGE_DIR/$target がありません。アーカイブを作成しません。" >&2
                exit 1
            fi
            echo "$PACKAGE_DIR/$target" >> "$TMP_LIST"
        done

        # 圧縮
        if ! tar -czf "$PACKAGE_ARCHIVE" -C "$LOADS_DIR" -T "$TMP_LIST"; then
            rm -f "$TMP_LIST"
            echo "Error: Archive creation failed."
            exit 1
        fi
        rm -f "$TMP_LIST"
        echo "Package directory archived as $PACKAGE_ARCHIVE"
    else
        echo "Error: Package directory does not exist. Skipping archive."
        exit 1
    fi
}

##### パッケージディレクトリの展開 #####
# 展開はトランザクションとして扱う。旧実装は既存ツリーを消してから展開しており、
# アーカイブが壊れていると tar の失敗時点でパッケージツリーだけが失われ、
# 復元手段が無かった。
#
#   フェーズ A: 一時ディレクトリへ展開し、内容を検証する（既存ツリーは無変更）
#   フェーズ B: 退避 rename と本体 rename で入れ替える（失敗したら rollback）
#   フェーズ C: ビルドする（失敗しても自動 rollback はせず .bak を残す）
#
# 一時ディレクトリは $LOADS_DIR 配下へ作る。別ファイルシステムだと
# フェーズ B の rename が跨げないため。
extract_package() {
    local live="$LOADS_DIR/$PACKAGE_DIR"
    local backup="$LOADS_DIR/$PACKAGE_DIR.bak"
    local staging staged target had_live=no

    if [ ! -f "$PACKAGE_ARCHIVE" ]; then
        echo "Error: Archive file $PACKAGE_ARCHIVE not found." >&2
        exit 1
    fi

    # run_package_build へ到達できないのに既存ツリーを消してしまわないよう、
    # 削除を始める前に emacs の存在を確認する。
    if ! command -v emacs >/dev/null 2>&1; then
        echo "Error: emacs が見つかりません。展開前に中止します。" >&2
        exit 1
    fi

    if [ -e "$backup" ]; then
        echo "Error: $backup が残っています。前回の復元が完了していない可能性があります。" >&2
        echo "       内容を確認し、手動で退避または削除してから再実行してください。" >&2
        exit 1
    fi

    ##### フェーズ A: 展開と検証 #####
    echo "Extracting package directory..."
    mkdir -p "$LOADS_DIR"
    staging="$(mktemp -d "$LOADS_DIR/.extract-XXXXXX")"
    # shellcheck disable=SC2064
    # staging はここで確定させたいので展開を遅延させない。
    trap "rm -rf '$staging'" EXIT

    if ! tar -xzf "$PACKAGE_ARCHIVE" -C "$staging"; then
        echo "Error: アーカイブの展開に失敗しました。既存のパッケージは変更していません。" >&2
        exit 1
    fi

    staged="$staging/$PACKAGE_DIR"
    if [ ! -d "$staged" ]; then
        echo "Error: アーカイブに $PACKAGE_DIR が含まれていません。既存のパッケージは変更していません。" >&2
        exit 1
    fi
    for target in "${PACKAGE_TARGET[@]}"; do
        if [ ! -e "$staged/$target" ]; then
            echo "Error: アーカイブに $PACKAGE_DIR/$target が含まれていません。既存のパッケージは変更していません。" >&2
            exit 1
        fi
    done

    ##### フェーズ B: 入れ替え #####
    if [ -e "$live" ]; then
        had_live=yes
        if ! mv "$live" "$backup"; then
            echo "Error: 既存パッケージの退避に失敗しました。既存のパッケージは変更していません。" >&2
            exit 1
        fi
    fi

    if ! mv "$staged" "$live"; then
        echo "Error: パッケージの配置に失敗しました。" >&2
        if [ "$had_live" = yes ]; then
            if mv "$backup" "$live"; then
                echo "       既存のパッケージを復元しました。" >&2
            else
                echo "       復元にも失敗しました。$backup を手動で $live へ戻してください。" >&2
            fi
        fi
        exit 1
    fi
    echo "Package directory extracted to $live"

    # eln-cache は展開したパッケージと対応しないため破棄する。
    # var/hist と var/backup はユーザーデータなので残す。
    if [ -d "$VAR_DIR/package" ]; then
        echo "Removing $VAR_DIR/package ..."
        rm -rf "${VAR_DIR:?}/package"
    fi

    ##### フェーズ C: ビルド #####
    echo "Running package build..."
    if ! run_package_build; then
        echo "Error: パッケージのビルドに失敗しました。" >&2
        if [ "$had_live" = yes ]; then
            echo "       展開したパッケージは配置済みです。元へ戻すには次を実行してください。" >&2
            echo "         rm -rf '$live'" >&2
            echo "         mv '$backup' '$live'" >&2
        else
            echo "       復元対象の旧パッケージはありません。" >&2
        fi
        exit 1
    fi

    if [ "$had_live" = yes ]; then
        rm -rf "${backup:?}"
    fi
}

##### メイン処理 #####
[ $# -eq 0 ] && { echo "Error: No action specified."; usage; }
ACTION="$1"; shift
case "$ACTION" in
    -s|--setup)
        SETUP_GUI="gtk3"  # 既定は現状どおり GUI パッケージも入れる
        while [[ $# -gt 0 ]]; do
            case "$1" in
                -g|--gui)
                    if [[ $# -lt 2 || "$2" == -* ]]; then
                        echo "Error: --gui requires a value (gtk3, lucid, pgtk, no)." >&2
                        exit 1
                    fi
                    SETUP_GUI="$2"
                    shift 2
                    ;;
                *)
                    echo "Error: Unknown option '$1' for --setup." >&2
                    usage
                    ;;
            esac
        done
        setup_env "$SETUP_GUI"
        ;;
    -l|--list)            list_emacs_versions ;;
    -i|--install)
        EMACS_VERSION=""
        GUI_TOOLKIT="gtk3"  # デフォルト GUI

        while [[ $# -gt 0 ]]; do
            case "$1" in
                -g|--gui)
                    if [[ $# -lt 2 || "$2" == -* ]]; then
                        echo "Error: --gui requires a value (gtk3, lucid, pgtk, no)."
                        exit 1
                    fi
                    GUI_TOOLKIT="$2"
                    shift 2
                    ;;
                -*)
                    echo "Error: Unknown option '$1' for --install."
                    usage
                    ;;
                *)
                    if [[ -n "$EMACS_VERSION" ]]; then
                        echo "Error: Multiple version arguments specified ('$EMACS_VERSION' and '$1')."
                        exit 1
                    fi
                    EMACS_VERSION="$1"
                    shift
                    ;;
            esac
        done

        if [ -z "$EMACS_VERSION" ]; then
            echo "Error: No Emacs version specified for install."
            usage
        fi
        validate_version "$EMACS_VERSION"

        install_emacs "$EMACS_VERSION" "$GUI_TOOLKIT"
        ;;
    -u|--uninstall)       uninstall_emacs ;;
    -c|--clean)           clean ;;
    -C|--clean-all)       clean_all ;;
    -p|--packing-package) packing_package ;;
    -x|--extract-package) extract_package ;;
    -h|--help)            usage 0 ;;
    *) echo "Error: Invalid argument '$ACTION'"; usage ;;
esac
