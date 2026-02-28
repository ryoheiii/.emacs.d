#!/bin/bash

set -euo pipefail

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
  -n, --setup-node          Install Node.js 22 LTS via fnm (for Copilot etc.).
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
  $0 --setup-node
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

##### Node.js インストール（fnm 経由） #####
setup_node() {
    echo "Setting up Node.js via fnm..."

    local FNM_DIR="$HOME/.local/share/fnm"

    # fnm インストール
    if ! command -v fnm &>/dev/null && [ ! -x "$FNM_DIR/fnm" ]; then
        echo "Installing fnm to $FNM_DIR ..."
        curl -fsSL https://fnm.vercel.app/install | bash -s -- \
            --install-dir "$FNM_DIR" --skip-shell
        # インストール成功を検証（実行可能かチェック）
        if [ ! -x "$FNM_DIR/fnm" ]; then
            echo "Error: fnm installation failed." >&2
            exit 1
        fi
    fi

    export PATH="$FNM_DIR:$PATH"

    # fnm が実行可能か最終検証
    if ! command -v fnm &>/dev/null; then
        echo "Error: fnm is not executable after installation." >&2
        exit 1
    fi

    eval "$(fnm env --shell bash)"

    # Node.js 22 LTS をインストール・デフォルト化
    fnm install 22
    fnm default 22

    echo ""
    echo "Node.js $(node --version) installed via fnm."
    echo ""
    echo "=== シェル設定 ==="
    echo "以下を ~/.bashrc や ~/.zshrc に追加してください:"
    # 実際の fnm バイナリのディレクトリを案内（既存 fnm 流用時のパスずれを防止）
    local fnm_bin_dir
    fnm_bin_dir="$(dirname "$(command -v fnm)")"
    echo "  export PATH=\"${fnm_bin_dir/#"$HOME"/\$HOME}:\$PATH\""
    echo '  eval "$(fnm env)"'
}

##### 関連パッケージインストール #####
setup_env() {
    echo "Setting up Emacs environment..."

    export DEBIAN_FRONTEND=noninteractive # Set non-interactive mode for apt-get

    sudo apt-get update

    ##### Emacs インストール時に必要なパッケージのインストール

    ### 両方の環境 (GUI & TUI) で必要なもの
    ## 必須（build-essential を先にインストールして GCC を確保）
    sudo apt-get install -y build-essential autoconf automake texinfo git libtool

    ## GCC バージョン検出（libgccjit 用）
    GCC_VERSION=$(gcc -dumpversion | cut -d. -f1)
    echo "Detected GCC version: $GCC_VERSION"

    ## 推奨
    # libm17n-dev              — 多言語テキスト処理（必要に応じて有効化）
    # libsystemd-dev           — systemd の統合（必要に応じて有効化）
    local COMMON_PACKAGES=(
        pkg-config                      # C/C++ プロジェクトのライブラリ依存管理ツール
        "libgccjit-${GCC_VERSION}-dev"  # ネイティブコンパイル用 (Emacs29以降)
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
        libgnutls28-dev                 # TLS (HTTPS/SSL) サポート
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
        libcairo-5c-dev                 # 2Dグラフィックス
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
    sudo apt-get install -y "${GUI_PACKAGES[@]}"
    sudo apt-get install -y "${TUI_PACKAGES[@]}"
    sudo apt-get install -y "${EMACS_TOOL_PACKAGES[@]}"

    echo "Emacs setup-env complete."
}

##### インストール可能な Emacs バージョンを取得 #####
list_emacs_versions() {
    echo "Fetching available Emacs versions..."
    local html
    if ! html=$(curl -sf https://ftp.gnu.org/gnu/emacs/); then
        echo "Error: バージョン一覧の取得に失敗しました。" >&2
        exit 1
    fi
    echo "$html" | grep -oP 'emacs-\d+\.\d+(?:\.\d+)?' | sed 's/emacs-//' | sort -V | uniq
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
    if [ ! -f "$TAR_FILE" ]; then
        wget "https://ftp.jaist.ac.jp/pub/GNU/emacs/$TAR_FILE"
    fi

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
            echo "Unsupported GUI type: $GUI"
            echo "Supported GUI types: gtk3 (default), lucid, pgtk, no"
            exit 1
            ;;
    esac
    ./configure "${CONFIG_OPTS[@]}"
    make -j$(nproc)
    make install

    echo "Emacs $VERSION installation complete."
}

##### Emacs アンインストール #####
uninstall_emacs() {
    echo "Uninstalling Emacs..."
    [ ! -d "$EMACS_SRC_DIR" ] && { echo "No Emacs installation found."; exit 0; }

    cd "$EMACS_SRC_DIR"
    make uninstall
    cd ..
    rm -rf "$EMACS_SRC_DIR"

    # make uninstall で削除されないディレクトリの手動削除
    rm -rf "$EMACS_INSTALL_PREFIX"/{bin,share,libexec,lib,include}/emacs

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
        rm -rf "$LOADS_DIR/$PACKAGE_DIR"
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

        # `PACKAGE_TARGET` の各項目をリストに追加
        for target in "${PACKAGE_TARGET[@]}"; do
            if [ -e "$LOADS_DIR/$PACKAGE_DIR/$target" ]; then
                echo "$PACKAGE_DIR/$target" >> "$TMP_LIST"
            fi
        done

        # 圧縮
        if [ -s "$TMP_LIST" ]; then
            if ! tar -czf "$PACKAGE_ARCHIVE" -C "$LOADS_DIR" -T "$TMP_LIST"; then
                rm -f "$TMP_LIST"
                echo "Error: Archive creation failed."
                exit 1
            fi
            rm -f "$TMP_LIST"
            echo "Package directory archived as $PACKAGE_ARCHIVE"
        else
            rm -f "$TMP_LIST"
            echo "Error: No valid files/directories to archive."
            exit 1
        fi
    else
        echo "Error: Package directory does not exist. Skipping archive."
        exit 1
    fi
}

##### パッケージディレクトリの展開 #####
extract_package() {
    if [ -f "$PACKAGE_ARCHIVE" ]; then
        # 展開
        echo "Extracting package directory..."
        if [ -d "$LOADS_DIR/$PACKAGE_DIR" ]; then
            echo "Removing existing $LOADS_DIR/$PACKAGE_DIR..."
            rm -rf "$LOADS_DIR/$PACKAGE_DIR"
        fi
        tar -xzf "$PACKAGE_ARCHIVE" -C "$LOADS_DIR"
        echo "Package directory extracted to $LOADS_DIR/$PACKAGE_DIR"
        clean

        # ビルド
        echo "Running package build..."
        run_package_build
    else
        echo "Error: Archive file $PACKAGE_ARCHIVE not found."
        exit 1
    fi
}

##### メイン処理 #####
[ $# -eq 0 ] && { echo "Error: No action specified."; usage; }
ACTION="$1"; shift
case "$ACTION" in
    -s|--setup)           setup_env ;;
    -n|--setup-node)      setup_node ;;
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
