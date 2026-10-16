<!-- -*- gfm -*- -->
# custom/ の内容

`custom.el`・個人スニペット（どちらも gitignored）と、Markdown プレビュー用の CSS / JS（Git 管理）を置く。

## custom.el

`M-x customize` の保存先。手で `setq` を書いてもよく、設定モジュールより先に読み込まれる。
使い方は README の [環境ごとの設定](../README.md#環境ごとの設定) を参照。

## snippets/

個人用の yasnippet スニペット。汎用のスニペットは `yasnippet-snippets` パッケージが供給し、挿入は `C-c y`。
次のどちらかのレイアウトで置く。

- `custom/snippets/<mode>/` にモードごとのディレクトリを直接置く
- `custom/snippets/snippets` を、直下に `<mode>/` を持つ外部ディレクトリ（Dropbox など）への symlink にする

両方を同時に置く構成は非対応。起動時に警告が出て、`custom/snippets/<mode>/` 側は読み込まれない。
レイアウトを新しく作ったり切り替えたときは Emacs を再起動する（既存のレイアウトへスニペットを足すだけなら `M-x yas-reload-all`）。

## css/ と js/

`C-c C-c p` で開く Markdown プレビュー（pandoc で生成する HTML）が読み込む CSS / JS。
本体は `css/markdown-style.css` と `js/markdown-script.js` の 2 ファイル。js は HTML 本文の末尾へ挿入する断片で、`<script>` タグを含めたまま書く。
