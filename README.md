<!-- -*- gfm -*- -->
## 必要なもの
# パス設定
- yasnippets: 設定ファイル
- auto-complete: 設定ファイル
- migemo: ./init/20-migemoへインストールディレクトリ内の辞書パス設定
# font
sudo apt-get install fonts-inconsolata
- #sudo aptitude install ttf-inconsolata ttf-takao # 過去
# gtag
# migemo （ローマ字で日本語検索するツール）
sudo apt-get install cmigemo
# aspell (スペルチェック)
sudo apt-get install aspell
- ~/.aspell.confも必要

## package管理
~/.emacs.d/elisp/site-lisp, elpa 以下のものを全部読み込む
# ~/.emacs.d/inits ディレクトリ以下のファイルは以下の設定に従って作成する
- 環境に依存しない設定はファイル名の最初に 2 桁の数字をつける
- 番号は優先度で 00 が最初に読み込まれて， 99 が最後に読み込まれる
- 同じ数字を付けても良い
- 環境依存な設定はそれぞれの環境に対して以下のプレフィックスをファイル名の最初につける
-- 環境: prefix
-- Meadow: meadow
-- Carbon Emacs: carbon-emacs
-- Cocoa Emacs: cocoa-emacs
-- emacs -nw: nw
-- Windows: windows
-- Linux: linux
- 例： 00-keybind.el とか cocoa-emacs-appearance.el のように "prefix-機能名.el" とファイル名をつける
- package.el 周りの設定は 20 番台に書くことが多い