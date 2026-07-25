# 自分用カスタムデータ置き場

## snippets (for yasnippet)

* 追加スニペット集の本体：`yasnippet-snippets` パッケージ（straight 管理）。
  パスはパッケージが公開する `yasnippet-snippets-dir` で参照する。
* 設定：`~/.emacs.d/loads/inits/31-editing.el`
* 個人スニペットはこのディレクトリに置く。次の 2 レイアウトに対応する。
  * `custom/snippets/<mode>/` にモードディレクトリを直接置く
  * `custom/snippets/snippets` を外部ディレクトリ（Dropbox 等）への symlink にする
* 両方を同時に使う構成は yasnippet の仕様上サポートしない（起動時に警告が出る）。
* 詳細は `.claude/rules/elisp-conventions.md` の
  「【不変条件】yasnippet のスニペットディレクトリ」を参照する。

## css / js (for markdown)

* `markdown-mode` のプレビュー表示に使う CSS / JS を置く。
* 設定：`~/.emacs.d/loads/site-elisp/my-markdown.el`
