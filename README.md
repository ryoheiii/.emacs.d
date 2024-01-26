<!-- -*- gfm -*- -->
# **準備～初回起動後**
## Linux パッケージのインストール
- **clang**: `sudo apt install clang`
- **カラーテーマ**: `sudo apt install elpa-color-theme-modern`
- **フォント**: `sudo apt install fonts-ricty-diminished`
- **gtags**: `sudo apt install global`
- **migemo** (ローマ字で日本語検索): `sudo apt install cmigemo`
- **aspell** (スペルチェック): `sudo apt install aspell`, `~/.aspell.conf` に設定
- **cmake** (irony-install-server 用): `sudo apt install cmake`
- **libclang** (irony-install-server 用): `sudo apt-get install libclang-dev`
- **node.js** (github copilot 用): `curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash`; `export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")" [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm`; nvm ls-remote ;  `nvm install <version>`

## .emacs.d のパス設定
- **yasnippets**: ローカルスニペット置き場を`loads/inits/20-external-package.el`で設定
- **migemo**: 辞書のインストール場所を`loads/inits/20-external-package.el`で設定
- **ispell**: aspell の実行バイナリパスを`loads/inits/20-external-package.el`で設定
- **copilot**: node.js の実行バイナリパスを`loads/inits/20-external-package.el`で設定

## .emacs.d のパス設定（初回起動後）
- **yasnippets**: インストールした`loads/elisp/yasnippet-snippets-<version>`のパス名を`loads/inits/20-external-package.el`で設定
- **color-theme-modern**: インストールした`~/.emacs.d/loads/elisp/color-theme-modern-<version>`のパス名を`~/.emacs.d/loads/inits/20-external-package.el`で設定

## Irony サーバーの構築（初回起動後）
- Emacs を起動後 `M-x irony-install-server` を実行

# **パッケージの管理方法**
- `~/.emacs.d/loads/elisp`および`~/.emacs.d/loads/site-elisp`下の設定ファイルを読み込み

# **設定ファイルの命名規則**
(`~/.emacs.d/loads/inits/*.el`)
- 環境に依存しない設定ファイルには、ファイル名の最初に 2 桁の数字を付与
- この数字は読み込み順序の優先順位を表し、00 が最初、99 が最後に読み込まれる
- 同じ数字を付けることも可能
- 環境依存の設定ファイルには、以下のプレフィックスをファイル名の先頭に付ける：
  1. Meadow環境: `meadow-`
  2. Carbon Emacs環境: `carbon-emacs-`
  3. Cocoa Emacs環境: `cocoa-emacs-`
  4. emacs -nw環境: `nw-`
  5. Windows環境: `windows-`
  6. Linux環境: `linux-`
- 例: `00-keybind.el`, `cocoa-emacs-appearance.el`（形式: `prefix-機能名.el`）
- `package.el`関連の設定は 20 番台に記述するのが一般的

# **その他**
- ターミナルでの Emacs 使用時には "xterm-256color" を利用
