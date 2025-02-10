#!/bin/bash

set -e

##### 設定 #####
EMACS_DIR="$HOME/.emacs.d"
# install-emacs
DL_DIR="$HOME/.local/downloads"
EMACS_SRC_DIR="$DL_DIR/emacs"
EMACS_INSTALL_PREFIX="$HOME/.local"
# clean
VAR_DIR="$EMACS_DIR/var"
ELISP_DIR="$EMACS_DIR/loads/elisp"

##### ヘルプ #####
usage() {
    cat << EOF
Usage: $0 [OPTION]...

Options:
  -s, --setup               Install required dependencies for Emacs.
  -l, --list                List available Emacs versions for installation.
  -i <ver>, --install <ver> Install Emacs <ver> (e.g., 29.4).
  -u, --uninstall           Uninstall the locally installed Emacs.
  -c, --clean               Remove Emacs temporary files.
  -h, --help                Show this help message and exit.

Examples:
  $0 --setup         Install dependencies.
  $0 --list          Show available Emacs versions.
  $0 --install 29.4  Install Emacs version 29.4.
  $0 --uninstall     Remove the installed Emacs.
  $0 --clean         Delete Emacs temporary files.
EOF
    exit 1
}

##### 関連パッケージインストール #####
setup_env() {
    echo "Setting up Emacs environment..."
    sudo apt-get update

    ## Emacs インストールに必要なパッケージのインストール
    GCC_VERSION=$(gcc -dumpversion | cut -d. -f1)
    echo "Detected GCC version: $GCC_VERSION"
    sudo apt-get install -y build-essential autoconf automake texinfo
    sudo apt-get install -y libgccjit-${GCC_VERSION}-dev # ネイティブコンパイル用 (Emacs29以降)
    sudo apt-get install -y libgnutls28-dev              # gnutls サポート
    sudo apt-get install -y libjansson-dev               # JSON サポート
    sudo apt-get install -y libjpeg-dev                  # JPEG 画像のサポート
    sudo apt-get install -y libgif-dev                   # GIF 画像のサポート
    sudo apt-get install -y libpng-dev                   # PNG 画像のサポート
    sudo apt-get install -y libtiff-dev                  # TIFF 画像のサポート
    sudo apt-get install -y librsvg2-dev                 # SVG 画像のサポート
    sudo apt-get install -y libxpm-dev                   # XPM 画像のサポート
    sudo apt-get install -y libncurses-dev               # 端末 UI 用 (TUI)
    sudo apt-get install -y libgtk-3-dev                 # GTK3 GUI サポート
    sudo apt-get install -y libxaw7-dev                  # Xaw3d 用 (GUI の一部)
    sudo apt-get install -y libxft-dev                   # フォント描画サポート
    sudo apt-get install -y libxkbcommon-dev             # キーボード入力処理
    sudo apt-get install -y libxrandr-dev                # 画面サイズ変更のサポート
    sudo apt-get install -y libxt-dev                    # X Toolkit サポート
    sudo apt-get install -y libharfbuzz-dev              # テキストレンダリング用
    sudo apt-get install -y libfreetype6-dev             # フォントサポート
    sudo apt-get install -y libsystemd-dev               # systemd の統合
    sudo apt-get install -y libsqlite3-dev               # SQLite バックエンド (Org-roam など)
    sudo apt-get install -y libtree-sitter-dev           # Tree-sitter (シンタックスハイライト)
    sudo apt-get install -y libxml2-dev                  # XML パース用
    sudo apt-get install -y zlib1g-dev                   # 圧縮機能
    sudo apt-get install -y liblcms2-dev                 # カラーマネジメント
    sudo apt-get install -y libwebp-dev                  # WebP 画像サポート
    sudo apt-get install -y libgpm-dev                   # 端末でのマウスサポート

    ## Emacs 利用に必要なパッケージのインストール
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
    ./configure --prefix="$EMACS_INSTALL_PREFIX" --with-native-compilation --with-tree-sitter
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

##### Emacs クリーンアップ #####
clean() {
    echo "Cleaning Emacs auto created files..."
    [ -d "$VAR_DIR" ] && echo "Removing $VAR_DIR ..." && rm -rf "$VAR_DIR"
    [ -d "$ELISP_DIR" ] && echo "Removing contents of $ELISP_DIR ..." && rm -rf "$ELISP_DIR"/*
    echo "Emacs clean complete."
}

##### メイン処理 #####
[ $# -eq 0 ] && { echo "Error: No action specified."; usage; }
ACTION="$1"; shift
case "$ACTION" in
    --setup-env|-s) setup_env ;;
    --list|-l) list_emacs_versions ;;
    --install-emacs|-i) install_emacs "$1" ;;
    --uninstall-emacs|-u) uninstall_emacs ;;
    --clean|-c) clean ;;
    --help|-h) usage ;;
    *) echo "Error: Invalid argument '$ACTION'"; usage ;;
esac
