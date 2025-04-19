#!/bin/bash

set -e

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
PACKAGE_BUILD_CMD="emacs --batch --eval \"(setq user-emacs-directory \\\"$EMACS_DIR\\\")\" -l \"$EMACS_DIR/early-init.el\" -l \"$EMACS_DIR/init.el\" -f straight-rebuild-all"


##### ヘルプ #####
usage() {
    cat << EOF
Usage: $0 [options]...

Options:
  -s, --setup               Install required dependencies for Emacs.
  -l, --list                List available Emacs versions for installation.
  -i <ver>, --install <ver> Install Emacs <ver> (e.g., 30.1).
  -u, --uninstall           Uninstall the locally installed Emacs.
  -c, --clean               Remove Emacs auto generated files (excluding packages).
  -C, --clean-all           Remove all Emacs auto genarated files (including packages).
  -p, --packing-package     Archive the package directory ($PACKAGE_DIR).
  -x, --extract-package     Extract the package archive to .emacs.d/loads/$PACKAGE_DIR.
  -h, --help                Show this help message.

Examples:
  $0 --setup
  $0 --list
  $0 --install 30.1  Install Emacs version 30.1.
  $0 --uninstall
  $0 --clean
  $0 --clean-all
  $0 --packing-package
  $0 --extract-straight
EOF
    exit 1
}

##### 関連パッケージインストール #####
setup_env() {
    echo "Setting up Emacs environment..."

    export DEBIAN_FRONTEND=noninteractive # Set non-interactive mode for apt-get

    sudo apt-get update

    ##### Emacs インストール時に必要なパッケージのインストール
    ### 両方の環境 (GUI & TUI) で必要なもの
    GCC_VERSION=$(gcc -dumpversion | cut -d. -f1)
    echo "Detected GCC version: $GCC_VERSION"
    ## 必須
    sudo apt-get install -y build-essential autoconf automake texinfo git libtool
    ## 推奨
    sudo apt-get install -y pkg-config                   # C/C++ プロジェクトのライブラリ依存管理ツール
    sudo apt-get install -y libgccjit-${GCC_VERSION}-dev # ネイティブコンパイル用 (Emacs29以降)
    sudo apt-get install -y libsqlite3-dev               # SQLite バックエンド (Org-roam など)
    sudo apt-get install -y libtree-sitter-dev           # Tree-sitter (シンタックスハイライト)
    sudo apt-get install -y libxml2-dev                  # XML パース (shr.el, EWW)
    # sudo apt-get install -y libm17n-dev                  # 多言語テキスト処理
    sudo apt-get install -y libdbus-1-dev                # DBus 通信用ライブラリ
    sudo apt-get install -y zlib1g zlib1g-dev            # 圧縮ライブラリ (gzip など)
    sudo apt-get install -y libacl1-dev                  # POSIX ACL（アクセス制御リスト)サポート
    sudo apt-get install -y libp11-kit-dev               # GnuTLS が証明書ストアを扱うための共通ライブラリ
    # sudo apt-get install -y libsystemd-dev               # systemd の統合

    ### GUI (X11/GTK) で必要なもの
    ## 必須
    sudo apt-get install -y libgtk-3-dev                 # GTK3 ベースの GUI サポート
    sudo apt-get install -y libgnutls28-dev              # TLS (HTTPS/SSL) サポート
    sudo apt-get install -y libfreetype6-dev             # フォントサポート
    sudo apt-get install -y libotf-dev                   # Opentype フォント処理のサポート
    # X11 関連。GTK3 を使う場合は不要なものもあり
    sudo apt-get install -y libx11-dev                   # X Window System の基本ライブラリ
    sudo apt-get install -y libxmu-dev                   # X11 のユーティリティライブラリ
    sudo apt-get install -y xorg-dev                     # X11 開発パッケージ
    sudo apt-get install -y libxfixes-dev                # X11 の細かい修正拡張
    sudo apt-get install -y libxft-dev                   # フォント描画サポート
    sudo apt-get install -y libxkbcommon-dev             # キーボード入力処理
    sudo apt-get install -y libxrandr-dev                # 画面サイズ変更のサポート
    sudo apt-get install -y libxt-dev                    # X Toolkit サポート
    ## 推奨
    sudo apt-get install -y libjpeg-dev                  # JPEG 画像のサポート
    sudo apt-get install -y libgif-dev                   # GIF 画像のサポート
    sudo apt-get install -y libpng-dev                   # PNG 画像のサポート
    sudo apt-get install -y libtiff-dev                  # TIFF 画像のサポート
    sudo apt-get install -y librsvg2-dev                 # SVG 画像のサポート
    sudo apt-get install -y libxpm-dev                   # XPM 画像のサポート
    sudo apt-get install -y libxaw7-dev                  # Xaw3d 用 (GUI の一部)
    sudo apt-get install -y libharfbuzz-dev              # 高品質なフォントレンダリング (ligature.el など使う場合に必須)
    sudo apt-get install -y libxcomposite-dev            # GTX3 での合成描画
    sudo apt-get install -y libmagickwand-dev            # ImageMagick の C API
    sudo apt-get install -y libxi-dev                    # 入力拡張 (XInput) 用のライブラリ
    sudo apt-get install -y libcairo-5c-dev              # 2Dグラフィックス
    sudo apt-get install -y liblcms2-dev                 # カラーマネジメント
    sudo apt-get install -y libwebp-dev                  # WebP 画像サポート

    ### ターミナル (TUI) で必要なもの
    ## 必須
    sudo apt-get install -y libncurses-dev               # ターミナルでのテキスト UI 提供ライブラリ
    sudo apt-get install -y libgpm-dev                   # General Purpose Mouse による端末でのマウスサポート
    ## 推奨
    sudo apt-get install -y libjansson-dev               # JSON パース (LSP・eglot 用)

    ##### Emacs 利用時に必要なパッケージのインストール
    sudo apt-get install -y clang libclang-dev           # Clang 用
    sudo apt-get install -y elpa-color-theme-modern      # カラーテーマ用
    sudo apt-get install -y fonts-ricty-diminished       # フォント用
    sudo apt-get install -y global                       # GTAGS (ソースコード検索ツール)
    sudo apt-get install -y cmigemo                      # Migemo (ローマ字で日本語検索)
    sudo apt-get install -y hunspell hunspell-en-us      # Hunspell (スペルチェック)
    sudo apt-get install -y aspell aspell-en             # Aspell (スペルチェック)
    sudo apt-get install -y cmake llvm libclang-dev      # CMake and LLVM, Libclang (irony-install-server 用)
    sudo apt-get install -y pandoc                       # Pandoc (Markdown 用)
    echo "Emacs setup-env complete."
}

##### インストール可能な Emacs バージョンを取得 #####
list_emacs_versions() {
    echo "Fetching available Emacs versions..."
    curl -s http://ftp.gnu.org/gnu/emacs/ | grep -oP 'emacs-\d+\.\d+(?:\.\d+)?' | sed 's/emacs-//' | sort -V | uniq
}

##### Emacs インストール #####
# 参考: https://myemacs.readthedocs.io/ja/latest/build.html
install_emacs() {
    local VERSION="$1"
    [ -z "$VERSION" ] && { echo "Error: No Emacs version specified."; usage; }
    [ -d "$EMACS_SRC_DIR" ] && { echo "Emacs is already installed. Run 'uninstall' first."; exit 1; }

    echo "Installing Emacs $VERSION ..."

    # ソースコード取得
    mkdir -p "$DL_DIR"
    cd "$DL_DIR"
    TAR_FILE="emacs-$VERSION.tar.gz"
    [ ! -f "$TAR_FILE" ] && wget "http://ftp.jaist.ac.jp/pub/GNU/emacs/$TAR_FILE"
    tar xvf "$TAR_FILE" --transform="s/^emacs-$VERSION/emacs/"

    # ビルドとインストール
    cd emacs
    ./autogen.sh
    # ./configure --prefix="$EMACS_INSTALL_PREFIX" --with-native-compilation --with-pgtk --with-json --with-tree-sitter --with-modules --without-pop
    ./configure --prefix="$EMACS_INSTALL_PREFIX" --with-native-compilation --with-x-toolkit=gtk3 --with-x --with-json --with-tree-sitter --with-modules --without-pop
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
    [ -d "$VAR_DIR" ] && echo "Removing $VAR_DIR ..." && rm -rf "$VAR_DIR"
    echo "Emacs clean complete."
}

##### Emacs 完全クリーンアップ（パッケージ含む） #####
clean_all() {
    echo "Cleaning all Emacs-related files, including packages..."
    clean
    [ -d "$LOADS_DIR/$PACKAGE_DIR" ] && echo "Removing $LOADS_DIR/$PACKAGE_DIR ..." && rm -rf "$LOADS_DIR/$PACKAGE_DIR"
    echo "Emacs full clean complete."
}

##### パッケージディレクトリの圧縮 #####
packing_package() {
    if [ -d "$LOADS_DIR/$PACKAGE_DIR" ]; then
        echo "Archiving package directory..."

        # 一時リストファイルの作成
        TMP_LIST=$(mktemp)

        # `PACKAGE_TARGET` の各項目をリストに追加
        for target in "${PACKAGE_TARGET[@]}"; do
            if [ -e "$LOADS_DIR/$PACKAGE_DIR/$target" ]; then
                echo "$PACKAGE_DIR/$target" >> "$TMP_LIST"
            fi
        done

        # 圧縮
        if [ -s "$TMP_LIST" ]; then
            tar -czf "$PACKAGE_ARCHIVE" -C "$LOADS_DIR" -T "$TMP_LIST"
            echo "Package directory archived as $PACKAGE_ARCHIVE"
        else
            echo "Error: No valid files/directories to archive."
            exit 1
        fi

        # 一時リストファイルの削除
        rm -f "$TMP_LIST"
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
        [ -d "$LOADS_DIR/$PACKAGE_DIR" ] && echo "Removing existing $LOADS_DIR/$PACKAGE_DIR..." && rm -rf "$LOADS_DIR/$PACKAGE_DIR"
        tar -xzf "$PACKAGE_ARCHIVE" -C "$LOADS_DIR"
        echo "Package directory extracted to $LOADS_DIR/$PACKAGE_DIR"
        clean

        # ビルド
        if [ -n "$PACKAGE_BUILD_CMD" ]; then
            echo "Running package build command: $PACKAGE_BUILD_CMD"
            eval "$PACKAGE_BUILD_CMD"
        fi
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
    -l|--list)            list_emacs_versions ;;
    -i|--install)         install_emacs "$1" ;;
    -u|--uninstall)       uninstall_emacs ;;
    -c|--clean)           clean ;;
    -C|--clean-all)       clean_all ;;
    -p|--packing-package) packing_package ;;
    -x|--extract-package) extract_package ;;
    -h|--help)            usage ;;
    *) echo "Error: Invalid argument '$ACTION'"; usage ;;
esac
