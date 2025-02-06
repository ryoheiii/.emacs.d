<!-- -*- gfm -*- -->
# Emacs 環境構築ガイド

## 1. Linux パッケージのインストール
以下のパッケージをインストールする

### 必須パッケージ
- **Clang**: `sudo apt install clang`
- **カラーテーマ**: `sudo apt install elpa-color-theme-modern`
- **フォント**: `sudo apt install fonts-ricty-diminished`
- **GTAGS (ソースコード検索ツール)**: `sudo apt install global`
- **Migemo (ローマ字で日本語検索)**: `sudo apt install cmigemo`
- **Hunspell (スペルチェック)**: `sudo apt install hunspell hunspell-en-us`
- **Aspell (スペルチェック)**: `sudo apt install aspell aspell-en`
- **CMake (irony-install-server 用)**: `sudo apt install cmake`
- **Libclang (irony-install-server 用)**: `sudo apt install libclang-dev`
- **Pandoc (Markdown 用)**: `sudo apt install pandoc`
- **Node.js (GitHub Copilot 用)**:
  `curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")" [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" nvm ls-remote nvm install <version>`

## 2. Emacs 設定ファイルのパス設定
以下のパスを設定する。

### 設定する項目
- **Yasnippets**: ローカルスニペットのパスを `loads/inits/20-external-package.el` で設定
- **Migemo**: 辞書のインストール場所を `loads/inits/20-external-package.el` で設定
- **Copilot**: Node.js の実行バイナリパスを `loads/inits/20-external-package.el` で設定

## 3. 初回起動後の追加設定
Emacs の初回起動後に、以下の設定を行う。

### 設定項目
- **Yasnippets**: インストール後のパス (`loads/elisp/yasnippet-snippets-<version>`) を `loads/inits/20-external-package.el` で指定
- **Color Theme Modern**: インストールされたパス (`~/.emacs.d/loads/elisp/color-theme-modern-<version>`) を `~/.emacs.d/loads/inits/20-external-package.el` で指定
- **Irony サーバーの構築**: Emacs 起動後、以下コマンドを実行
  ```sh
  M-x irony-install-server
  ```

## 4. パッケージ管理
Emacs のパッケージは以下のディレクトリで管理する。
- `~/.emacs.d/loads/elisp`
- `~/.emacs.d/loads/site-elisp`

## 5. 設定ファイルの命名規則
Emacs の設定ファイル (`~/.emacs.d/loads/inits/*.el`) には、以下の命名規則を適用する。

### 命名規則
- **環境に依存しない設定ファイル**
  - ファイル名の先頭に 2 桁の数字を付与 (例: `00-keybind.el`)
  - 数字は読み込み順を示し、`00` が最初、`99` が最後
  - 同じ数字を付けることも可能
- **環境依存の設定ファイル**
  - 環境ごとに以下のプレフィックスを使用:
    - Meadow: `meadow-`
    - Carbon Emacs: `carbon-emacs-`
    - Cocoa Emacs: `cocoa-emacs-`
    - Terminal (`emacs -nw`): `nw-`
    - Windows: `windows-`
    - Linux: `linux-`
  - 例: `cocoa-emacs-appearance.el` (`<環境>-<機能名>.el`)
- **package.el 関連の設定は 20 番台に記述**

## 6. その他の注意点
- ターミナルで Emacs を利用する際は、`xterm-256color` を設定
