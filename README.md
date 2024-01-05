<!-- -*- gfm -*- -->

# **準備**
## ローカル環境に合わせたパスの変更
### yasnippets
- `loads/inits/20-external-package.el`で変更
### auto-complete
- `loads/inits/20-external-package.el`で変更
### migemo
- `loads/inits/20-external-package.el`で変更

## パッケージのインストール
### clang
- `sudo apt install clang`
### カラーテーマ
- `sudo apt install elpa-color-theme-modern`
### フォント
- `sudo apt install fonts-ricty-diminished`
### gtags
- `sudo apt install global`
### migemo (ローマ字で日本語検索)
- `sudo apt install cmigemo`
### aspell (スペルチェック)
- `sudo apt install aspell`
- `~/.aspell.conf`に設定
### cmake (irony-install-server 用)
- `sudo apt install cmake`
### libclang (irony-install-server 用)
- `sudo apt-get install libclang-dev`

# **パッケージの管理方法**
* `~/.emacs.d/loads/elisp/site-lisp`と`elpa`下の全ての設定ファイルを読み込む

# **設定ファイルの命名規則 (`~/.emacs.d/loads/inits/*.el`)**
* 環境に依存しない設定ファイルは、ファイル名の最初に2桁の数字を付ける
* 数字は優先順位を表し、00 が最初、99 が最後に読み込まれる
* 同じ数字を付けることも可能
* 環境依存の設定は、以下のプレフィックスをファイル名の先頭に付ける
  1. Meadow: `meadow`
  2. Carbon Emacs: `carbon-emacs`
  3. Cocoa Emacs: `cocoa-emacs`
  4. emacs -nw: `nw`
  5. Windows: `windows`
  6. Linux: `linux`
* 例: `00-keybind.el`, `cocoa-emacs-appearance.el`（`prefix-機能名.el`の形式）
* `package.el`に関連する設定は20番台に書くことが多い

# **その他**
* "xterm-256color"を使用する
* Irony の初回実行時、サーバー構築のため`M-x irony-install-server`の手動実行が必要
