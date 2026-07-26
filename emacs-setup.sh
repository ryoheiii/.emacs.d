#!/bin/bash

# -E は現時点では no-op だが、将来 ERR トラップを足したときに
# 関数・コマンド置換・サブシェル内で発火させるために必要になる。
set -Eeuo pipefail

##### 設定 #####
readonly EMACS_DIR="$HOME/.emacs.d"
readonly LOADS_DIR="$EMACS_DIR/loads"
readonly PACKAGE_DIR="straight"
# setup-treesit（文法の取得元・固定タグ・導入先は、このファイルが正本）
readonly TREESIT_LIB="$LOADS_DIR/site-elisp/my-treesit.el"
# install-emacs
readonly DL_DIR="$HOME/.local/downloads"
readonly EMACS_SRC_DIR="$DL_DIR/emacs"
readonly EMACS_INSTALL_PREFIX="$HOME/.local"
# setup-node (offline)
readonly NODE_DL_DIR="$DL_DIR/node"
readonly NODE_INSTALL_BASE="$HOME/.local/share/nodejs"
readonly NODE_ACTIVE_LINK="$HOME/.local/node"
# fnm をこのスクリプトが導入したことを示す印。--uninstall-node の削除判定に使う。
readonly FNM_OWNED_MARKER=".installed-by-emacs-setup"
# clean
readonly VAR_DIR="$EMACS_DIR/var"
# early-init.el を読まない emacs --batch が作る迷子の eln-cache。
# 本設定での正規の保存先は var/package/eln-cache/ で、こちらは読み込み経路からも外れている。
readonly STRAY_ELN_DIR="$EMACS_DIR/eln-cache"
# packing/extract_package
readonly PACKAGE_ARCHIVE="$EMACS_DIR/package.tar.gz"
readonly PACKAGE_TARGET=("repos" "versions/default.el")
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
        echo "Error: Invalid version format '$ver'. Expected format: NN.N or NN.N.N (e.g., 30.1, 29.4.1)" >&2
        exit 1
    fi
}

##### ヘルプ #####
# --help（正常系）は stdout、エラー経路からの呼び出しは stderr へ出す。
# エラーメッセージだけを stderr にしても、ヘルプ本文が stdout に出ていては
# 「エラー時に stdout が空」にならない。
usage() {
    local code="${1:-1}"
    local fd=1
    [ "$code" -eq 0 ] || fd=2
    cat >&"$fd" << EOF
Usage: $0 [options]...

Options:
  -s, --setup [-g|--gui <gtk3|lucid|pgtk|no>]
                            Install required dependencies for Emacs.
                            Only "no" changes the result: it skips the GUI
                            packages. gtk3 / lucid / pgtk are equivalent to
                            the default (install everything).
  -n, --setup-node          Install Node.js (offline tarball or fnm fallback).
                            Required only for GitHub Copilot.
  -t, --setup-treesit       Build the C/C++ tree-sitter grammars into
                            var/package/tree-sitter/. Included in --setup when a
                            tree-sitter enabled Emacs is already available; run
                            it after --install otherwise.
  --uninstall-node          Uninstall Node.js (offline install and/or fnm).
  -l, --list                List available Emacs versions for installation.
  -i <ver>, --install <ver> [-g|--gui <gtk3|lucid|pgtk|no>]
                            Install Emacs <ver> with optional GUI backend.
  -u, --uninstall           Uninstall the locally installed Emacs.
  -c, --clean               Remove generated files under var/ (packages kept).
                            This includes unrecoverable user data such as
                            minibuffer history, cursor positions and undo
                            history — not just caches.
                            Also removes a stray .emacs.d/eln-cache/ left by an
                            emacs --batch run that skipped early-init.el.
                            A symlink at that path is kept, not removed.
  -C, --clean-all           Same as --clean, and also removes the packages.
  -p, --packing-package     Archive the package directory ($PACKAGE_DIR).
  -x, --extract-package     Extract the package archive to .emacs.d/loads/$PACKAGE_DIR.
  -h, --help                Show this help message.

Environment:
  EMACS_SETUP_MIRROR_URL    Download mirror  (default: ftp.jaist.ac.jp)
  EMACS_SETUP_UPSTREAM_URL  Fallback source  (default: ftp.gnu.org)
  EMACS_SETUP_INDEX_URL     Index for --list (default: \$EMACS_SETUP_MIRROR_URL/)

Examples:
  $0 --setup
  $0 --setup --gui no            # Skip GUI dependencies (terminal only).
  $0 --setup-node                # Install Node.js for GitHub Copilot.
  $0 --setup-treesit             # Install the C/C++ tree-sitter grammars.
  $0 --uninstall-node
  $0 --list
  $0 --install 30.1              # Install Emacs version 30.1.
  $0 --uninstall
  $0 --clean
  $0 --clean-all
  $0 --packing-package
  $0 --extract-package
EOF
    exit "$code"
}

##### 実行前チェック #####
# 必要なコマンドはアクションごとに異なる。一律に確認すると、
# --help や --clean のように何も要らないアクションまで環境依存で失敗する。
require_commands() {
    local missing=() cmd
    for cmd in "$@"; do
        command -v "$cmd" >/dev/null 2>&1 || missing+=("$cmd")
    done
    if [ "${#missing[@]}" -gt 0 ]; then
        echo "Error: 次のコマンドが必要です: ${missing[*]}" >&2
        exit 1
    fi
}

# apt-get 前提の処理は Linux 専用である。スクリプト全体は拒否しない
# （--clean や --packing-package は他の環境でも動く）。
require_linux() {
    if [ "$(uname)" != Linux ]; then
        echo "Error: この操作は Linux (Debian/Ubuntu) 専用です。" >&2
        echo "       macOS では homebrew-emacs-plus などを利用してください。" >&2
        exit 1
    fi
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

##### Node.js アーキテクチャ検出 #####
# 成功時はアーキテクチャ名を stdout に出力し 0 を返す。
# 非対応アーキテクチャの場合は stderr にメッセージを出し 1 を返す。
detect_node_arch() {
    local machine
    machine="$(uname -m)"
    case "$machine" in
        x86_64)  echo "x64" ;;
        aarch64) echo "arm64" ;;
        armv7l)  echo "armv7l" ;;
        *)
            echo "Warning: Unsupported architecture for offline install: $machine" >&2
            return 1
            ;;
    esac
}

##### Node.js オフラインインストールが可能か判定 #####
# tarball が存在すれば 0 を返し、パスを stdout に出力する。
# 存在しなければ 1 を返す。
find_node_tarball() {
    local arch tarball
    arch="$(detect_node_arch)" || return 1

    tarball="$(find "$NODE_DL_DIR" -maxdepth 1 -name "node-v*-linux-${arch}.tar.xz" 2>/dev/null \
        | sort -V | tail -n 1)"

    if [ -z "$tarball" ]; then
        return 1
    fi
    echo "$tarball"
}

##### Node.js オフラインインストール #####
setup_node_offline() {
    local tarball="$1"

    echo "Installing Node.js from offline tarball: $(basename "$tarball") ..."

    mkdir -p "$NODE_INSTALL_BASE"

    # tarball 内のトップレベルディレクトリ名を取得・検証
    # awk は入力を最後まで読む。head や sed q のような早期終了を使うと、
    # tar が SIGPIPE で落ちて pipefail により exit 141 になる（実測）。
    local topdir
    topdir="$(tar -tf "$tarball" | awk -F/ 'NR==1 {print $1}')"
    if [ -z "$topdir" ]; then
        echo "Error: Failed to read tarball contents: $tarball" >&2
        exit 1
    fi

    # 既存の同バージョンディレクトリがあれば削除して再インストール
    # ${var:?} で空展開による広範囲削除を防ぐ（topdir は tar の中身由来のため必須）
    [ -d "$NODE_INSTALL_BASE/$topdir" ] && rm -rf "${NODE_INSTALL_BASE:?}/${topdir:?}"

    # 展開
    tar -xJf "$tarball" -C "$NODE_INSTALL_BASE"

    # アクティブシンボリックリンクの作成
    ln -sfn "$NODE_INSTALL_BASE/$topdir" "$NODE_ACTIVE_LINK"

    # インストール検証（絶対パスで実行し、システムの node を誤検出しない）
    local node_bin="$NODE_ACTIVE_LINK/bin/node"
    local npm_bin="$NODE_ACTIVE_LINK/bin/npm"
    if [ ! -x "$node_bin" ]; then
        echo "Error: node binary not found at $node_bin" >&2
        exit 1
    fi
    echo ""
    echo "node: $("$node_bin" -v)"
    echo "npm:  $("$npm_bin" -v)"
    echo "path: $node_bin"
    echo ""
    echo "Node.js installation complete."
    echo ""
    echo "=== シェル設定 ==="
    echo "以下を ~/.bashrc や ~/.zshrc に追加してください:"
    echo "  export PATH=\"\$HOME/.local/node/bin:\$PATH\""
}

##### Node.js インストール（fnm 経由） #####
setup_node_fnm() {
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
        # このスクリプトが導入したことを記録する。
        # --uninstall-node は、この印がある場合だけ fnm ディレクトリごと削除する
        # （既存 fnm を流用したときに、利用者の全 Node バージョンを消さないため）。
        : > "$FNM_DIR/$FNM_OWNED_MARKER"
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
    # シェル設定へ貼り付ける文字列そのものを出すため、ここは展開させない
    # shellcheck disable=SC2016
    echo '  eval "$(fnm env)"'
}

##### Node.js セットアップ（オフライン優先、fnm フォールバック） #####
setup_node() {
    local tarball
    # tarball 検索のみ if で判定（set -e が無効になっても安全）
    if tarball="$(find_node_tarball)"; then
        # 本体は直接呼び出し（set -e が有効な文脈で実行）
        setup_node_offline "$tarball"
        return
    fi
    echo "No offline tarball found in $NODE_DL_DIR. Falling back to fnm..."
    setup_node_fnm
}

##### Node.js アンインストール #####
uninstall_node() {
    echo "Uninstalling Node.js..."
    local found=false

    # オフラインインストールの削除
    if [ -L "$NODE_ACTIVE_LINK" ]; then
        local target
        target="$(readlink -f "$NODE_ACTIVE_LINK")"
        rm -f "$NODE_ACTIVE_LINK"
        echo "Removed symlink: $NODE_ACTIVE_LINK"

        # リンク先が NODE_INSTALL_BASE 配下の場合のみ削除（任意ディレクトリ削除を防止）
        local real_install_base
        real_install_base="$(readlink -f "$NODE_INSTALL_BASE" 2>/dev/null || echo "$NODE_INSTALL_BASE")"
        if [ -d "$target" ] && [[ "$target" == "$real_install_base"/* ]]; then
            rm -rf "$target"
            echo "Removed directory: $target"
        elif [ -d "$target" ]; then
            echo "Warning: Skipped deletion of $target (outside $NODE_INSTALL_BASE)" >&2
        fi

        # インストールベースディレクトリが空なら削除
        if [ -d "$NODE_INSTALL_BASE" ] && [ -z "$(ls -A "$NODE_INSTALL_BASE")" ]; then
            rmdir "$NODE_INSTALL_BASE"
        fi
        found=true
    fi

    # fnm インストールの削除
    # 既存 fnm を流用した場合は利用者の資産（全 Node バージョンと設定）であるため
    # 削除しない。このスクリプトが導入した印がある場合だけディレクトリごと消す。
    local FNM_DIR="$HOME/.local/share/fnm"
    if [ -d "$FNM_DIR" ]; then
        if [ -e "$FNM_DIR/$FNM_OWNED_MARKER" ]; then
            rm -rf "${FNM_DIR:?}"
            echo "Removed fnm directory: $FNM_DIR"
            found=true
        else
            echo "Skipped $FNM_DIR (not installed by this script)."
            echo "  このスクリプト導入分の Node だけ消すには: fnm uninstall 22"
        fi
    fi

    if [ "$found" = false ]; then
        echo "No Node.js installation found."
    else
        echo "Node.js uninstallation complete."
    fi
}

##### 関連パッケージインストール #####
# GUI_PACKAGES を入れるかどうかだけが分岐点になる。gtk3 / lucid / pgtk の間に
# パッケージの差は無いため、実質は no かそれ以外かの二値である。
setup_env() {
    local gui="${1:-gtk3}"
    validate_gui_toolkit "$gui"

    # gcc は build-essential で導入する対象そのものなので事前チェックに含めない。
    # 順序は macOS ガード → sudo/apt 系の確認 → build-essential → gcc 検出。
    require_linux
    require_commands sudo apt-get apt-cache

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
        clangd                          # C/C++ の LSP (eglot の第 1 段。clang とは別パッケージ)
        elpa-color-theme-modern         # カラーテーマ用
        fonts-ricty-diminished          # フォント用
        global                          # GTAGS (ソースコード検索ツール)
        cmigemo                         # Migemo (ローマ字で日本語検索)
        mozc-server emacs-mozc-bin      # Mozc (日本語入力。straight が入れるのは mozc.el だけ)
        ripgrep                         # ripgrep (consult-ripgrep と xref の検索経路)
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

    # C/C++ の tree-sitter 文法もここで導入する。ただし新規マシンでは --setup の
    # 時点で Emacs が未ビルドなのが普通なので、前提が揃うときだけ実行し、
    # 揃わなければ次の手順を案内して続行する（--setup 自体は失敗させない）。
    if treesit_prereqs_ready_p; then
        setup_treesit
    else
        echo "tree-sitter 文法の導入をスキップします (tree-sitter 有効な Emacs 30 以降と git が必要です)。"
        echo "       Emacs の導入後に ./emacs-setup.sh --setup-treesit を実行してください。"
    fi

    echo "Emacs setup-env complete."
}

##### tree-sitter 文法の導入 #####
# 文法の取得元・固定タグ・導入先は $TREESIT_LIB が正本であり、ここへ複製しない。
# use-package と straight に依存しないライブラリなので、パッケージを 1 つも
# 導入していない環境でも単体ロードできる。
# early-init.el を読むのはパスヘルパーと eln-cache のリダイレクトを通すためである
# （読まないと native-comp の生成物がリポジトリ直下の eln-cache/ へ落ちる）。
setup_treesit() {
    require_commands emacs git
    if [ ! -f "$TREESIT_LIB" ]; then
        echo "Error: $TREESIT_LIB が見つかりません。" >&2
        exit 1
    fi

    echo "Installing tree-sitter grammars (C/C++) ..."
    if ! emacs --batch \
        --eval "(setq user-emacs-directory \"$EMACS_DIR/\")" \
        -l "$EMACS_DIR/early-init.el" \
        -l "$TREESIT_LIB" \
        -f my/treesit-install-c-grammars; then
        echo "Error: tree-sitter 文法の導入に失敗しました。" >&2
        exit 1
    fi
}

# --setup から呼ぶ前提チェック。文法のビルドには git と C コンパイラが要り、
# 導入には tree-sitter 有効ビルドかつ Emacs 30 以降が要る。
# treesit-install-language-grammar が導入先 (OUT-DIR) を受け取るのは 30 以降で、
# 29 以前で呼ぶと引数エラーになる（Ubuntu 24.04 の既定は 29 系）。
# 判定は func-arity で行う。autoload のまま解決できるため treesit.el をロードせず、
# -Q と併せて Lisp を一切読まないので eln-cache も生成されない。
treesit_prereqs_ready_p() {
    [ -f "$TREESIT_LIB" ] || return 1
    command -v emacs >/dev/null 2>&1 || return 1
    command -v git >/dev/null 2>&1 || return 1
    emacs --batch -Q --eval \
        '(kill-emacs (if (and (fboundp (quote treesit-available-p))
                              (treesit-available-p)
                              (fboundp (quote treesit-install-language-grammar))
                              (>= (cdr (func-arity (quote treesit-install-language-grammar))) 2))
                         0 1))' \
        >/dev/null 2>&1 || return 1
    return 0
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
    require_commands curl grep sort
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
    require_linux
    require_commands wget tar make nproc

    local VERSION="$1"
    local GUI="${2:-gtk3}"  # GUIオプション、デフォルトは gtk3
    [ -z "$VERSION" ] && { echo "Error: No Emacs version specified." >&2; usage; }
    # ダウンロードや展開より前に検証する。後段で弾くと、ソースツリーが残った状態で
    # 失敗し、次回の正しい install まで「already installed」で拒否されてしまう。
    validate_gui_toolkit "$GUI"
    [ -d "$EMACS_SRC_DIR" ] && { echo "Emacs is already installed. Run 'uninstall' first." >&2; exit 1; }

    echo "Installing Emacs $VERSION ..."

    # ソースコード取得
    mkdir -p "$DL_DIR"
    cd "$DL_DIR"
    TAR_FILE="emacs-$VERSION.tar.gz"
    download_emacs_tarball "$TAR_FILE"

    tar xf "$TAR_FILE" --transform="s/^emacs-$VERSION/emacs/"

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
            # 【未検証】この経路の実ビルドは確認していない。回帰テストは
            # --setup の apt パッケージ選択をスタブで検査するだけで、
            # --install は実ビルドを伴うため CI でも make test-setup でも走らない。
            CONFIG_OPTS+=("--without-x")
            ;;
        *)
            validate_gui_toolkit "$GUI"  # ここへは来ないが、値の集合を一箇所に保つ
            ;;
    esac
    ./configure "${CONFIG_OPTS[@]}"
    make -j"$(nproc)"
    make install

    verify_installed_emacs "$VERSION"
}

##### インストール結果の検証 #####
# configure / make / make install が成功しても、生成物が起動しない、
# native-comp が無効、といった状態はあり得る。使えないインストールを
# 成功と報告しないため、検証に失敗したら非ゼロで終了する。
#
# ソースツリーは残す。調査と再ビルドができるようにするためだが、
# install_emacs は [ -d "$EMACS_SRC_DIR" ] で拒否するので、
# 再インストールには --uninstall が要る旨を案内する。
verify_installed_emacs() {
    local version="$1"
    local emacs_bin="$EMACS_INSTALL_PREFIX/bin/emacs"
    local failed=no

    if ! "$emacs_bin" --version >/dev/null 2>&1; then
        echo "Error: インストールした Emacs を起動できません ($emacs_bin)。" >&2
        failed=yes
    # (princ (native-comp-available-p)) は nil でも終了コード 0 になるため、
    # elisp 側で終了コードを立てる。
    elif ! "$emacs_bin" --batch \
            --eval '(kill-emacs (if (native-comp-available-p) 0 1))' >/dev/null 2>&1; then
        echo "Error: native-comp が有効になっていません。" >&2
        failed=yes
    fi

    if [ "$failed" = yes ]; then
        echo "       ビルドツリーは $EMACS_SRC_DIR に残してあります。" >&2
        echo "       再インストールするには先に $0 --uninstall を実行してください。" >&2
        exit 1
    fi

    echo "Emacs $version installation complete."
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
    # early-init.el を読まない emacs --batch は startup-redirect-eln-cache を
    # 通らないため、既定の $EMACS_DIR/eln-cache/ へ書く。本設定の正規の保存先は
    # var/package/eln-cache/ なので、置き場所の誤りとしてここで回収する。
    # symlink は利用者が意図して張ったものとみなし、削除せず残す
    # （rm -rf は参照先を辿らずリンク自体を消すため、黙って消すと復旧できない）。
    if [ -L "$STRAY_ELN_DIR" ]; then
        echo "Skipping $STRAY_ELN_DIR (symlink)." >&2
    elif [ -d "$STRAY_ELN_DIR" ]; then
        echo "Removing $STRAY_ELN_DIR ..."
        rm -rf "${STRAY_ELN_DIR:?}"
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
# straight-rebuild-all は変更検出を経由せず全パッケージを無条件に再ビルドし、
# ビルドキャッシュの mtime も更新する。このため --extract-package の後に
# 手動の straight-check-all は要らない。
# 【根拠の限界】これは straight.el の実装読解に基づく判断で、動的検証はしていない。
# 想定と違っていた場合は M-x straight-check-all の手動実行で回復できる。
run_package_build() {
    emacs --batch \
        --eval "(setq user-emacs-directory \"$EMACS_DIR\")" \
        -l "$EMACS_DIR/early-init.el" \
        -l "$EMACS_DIR/init.el" \
        -f straight-rebuild-all
}

##### パッケージディレクトリの圧縮 #####
# 一時ファイルの後始末は EXIT トラップで行う。RETURN トラップは exit や
# errexit による終了で発火せず、まさに漏れる経路を塞げない。
# トラップをサブシェルへ閉じ込め、他のアクションのトラップと干渉させない。
packing_package() {
    require_commands tar mktemp

    if [ ! -d "$LOADS_DIR/$PACKAGE_DIR" ]; then
        echo "Error: Package directory does not exist. Skipping archive." >&2
        exit 1
    fi

    (
        echo "Archiving package directory..."

        tmp_list=$(mktemp)
        trap 'rm -f "$tmp_list"' EXIT

        # `PACKAGE_TARGET` はすべて揃っていることを要求する。
        # 一部だけのアーカイブを許すと、展開側が完全性を要求するため
        # 「自分で作ったアーカイブを復元できない」組み合わせが生まれる。
        for target in "${PACKAGE_TARGET[@]}"; do
            if [ ! -e "$LOADS_DIR/$PACKAGE_DIR/$target" ]; then
                echo "Error: $PACKAGE_DIR/$target がありません。アーカイブを作成しません。" >&2
                exit 1
            fi
            echo "$PACKAGE_DIR/$target" >> "$tmp_list"
        done

        if ! tar -czf "$PACKAGE_ARCHIVE" -C "$LOADS_DIR" -T "$tmp_list"; then
            echo "Error: Archive creation failed." >&2
            exit 1
        fi
        echo "Package directory archived as $PACKAGE_ARCHIVE"
    )
}

##### 展開の補助 #####
# 宛先が既存ディレクトリのとき、その中へ移動せず失敗させる。
# GNU mv の -T が使えるならそれを、無ければ事前確認で代替する。
mv_replace() {
    if mv --version >/dev/null 2>&1; then
        mv -T "$1" "$2"
    else
        if [ -e "$2" ] || [ -L "$2" ]; then
            return 1
        fi
        mv "$1" "$2"
    fi
}

# 退避したツリーを元へ戻す。退避していなければ何もしない。
extract_rollback() {
    local backup="$1" live="$2" had_live="$3"
    [ "$had_live" = yes ] || return 0
    if mv_replace "$backup" "$live"; then
        echo "       既存のパッケージを復元しました。" >&2
    else
        echo "       復元にも失敗しました。$backup を手動で $live へ戻してください。" >&2
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
    require_commands tar mktemp

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

    # 壊れた symlink は -e が偽になるため -L も見る。
    if [ -e "$backup" ] || [ -L "$backup" ]; then
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
    if [ -e "$live" ] || [ -L "$live" ]; then
        had_live=yes
        if ! mv_replace "$live" "$backup"; then
            echo "Error: 既存パッケージの退避に失敗しました。既存のパッケージは変更していません。" >&2
            exit 1
        fi
    fi

    if ! mv_replace "$staged" "$live"; then
        echo "Error: パッケージの配置に失敗しました。" >&2
        extract_rollback "$backup" "$live" "$had_live"
        exit 1
    fi

    # mv は宛先が既存ディレクトリだとその中へ移動する。退避後に別プロセスが
    # $live を作り直していると入れ子になったまま「成功」してしまい、
    # 最後に .bak（元ツリー）を消してデータを失う。配置結果を検査する。
    if [ -e "$live/$PACKAGE_DIR" ]; then
        echo "Error: 配置結果が入れ子になっています（$live/$PACKAGE_DIR）。" >&2
        echo "       別プロセスが同時に $live を操作した可能性があります。" >&2
        rm -rf "${live:?}/${PACKAGE_DIR:?}"
        extract_rollback "$backup" "$live" "$had_live"
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
[ $# -eq 0 ] && { echo "Error: No action specified." >&2; usage; }
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
    -n|--setup-node)      setup_node ;;
    -t|--setup-treesit)   setup_treesit ;;
    --uninstall-node)     uninstall_node ;;
    -l|--list)            list_emacs_versions ;;
    -i|--install)
        EMACS_VERSION=""
        GUI_TOOLKIT="gtk3"  # デフォルト GUI

        while [[ $# -gt 0 ]]; do
            case "$1" in
                -g|--gui)
                    if [[ $# -lt 2 || "$2" == -* ]]; then
                        echo "Error: --gui requires a value (gtk3, lucid, pgtk, no)." >&2
                        exit 1
                    fi
                    GUI_TOOLKIT="$2"
                    shift 2
                    ;;
                -*)
                    echo "Error: Unknown option '$1' for --install." >&2
                    usage
                    ;;
                *)
                    if [[ -n "$EMACS_VERSION" ]]; then
                        echo "Error: Multiple version arguments specified ('$EMACS_VERSION' and '$1')." >&2
                        exit 1
                    fi
                    EMACS_VERSION="$1"
                    shift
                    ;;
            esac
        done

        if [ -z "$EMACS_VERSION" ]; then
            echo "Error: No Emacs version specified for install." >&2
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
    *) echo "Error: Invalid argument '$ACTION'" >&2; usage ;;
esac
