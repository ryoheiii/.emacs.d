<!-- -*- gfm -*- -->
# **Prepare**
## Modyfy path according to local environment
- yasnippets:    loads/inits/20-external-package.el
- auto-complete: loads/inits/20-external-package.el
- migemo:        loads/inits/20-external-package.el

## Package
### clang
- sudo apt install clang
### color theme
- sudo apt install elpa-color-theme-modern
### font
- sudo apt install fonts-ricty-diminished
### gtags
- sudo apt install global
### migemo （ローマ字で日本語検索）
- sudo apt install cmigemo
### aspell (スペルチェック)
- sudo apt install aspell
- ~/.aspell.conf

# **Package Management**
* ~/.emacs.d/loads/elisp/site-lisp, elpa 以下のものを全部読み込む

# **~/.emacs.d/inits 以下の命名規則**
* 環境に依存しない設定はファイル名の最初に 2 桁の数字
* 番号は優先度で 00 が最初に読み込まれて、99 が最後に読み込まれる
* 同じ数字を付けても良い
* 環境依存な設定はそれぞれの環境に対して以下のプレフィックスをファイル名の最初につける
0. 環境: prefix
1. Meadow: meadow
2. Carbon Emacs: carbon-emacs
3. Cocoa Emacs: cocoa-emacs
4. emacs -nw: nw
5. Windows: windows
6. Linux: linux
* 例： 00-keybind.el とか cocoa-emacs-appearance.el のように "prefix-機能名.el" とファイル名をつける
* package.el 周りの設定は 20 番台に書くことが多い

# **Others**
* xterm-256color を利用
