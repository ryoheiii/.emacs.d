---
description: Emacs Lisp の宣言、命名、配置と固定キーバインドを定義する
globs: ["**/*.el"]
---

# Emacs Lisp 規約

## `use-package`

- 外部パッケージは `use-package` で宣言する。`straight-use-package-by-default t` により `:straight t` は暗黙に適用される。
- 組み込みパッケージには必ず `:straight nil` を指定する。
- 設定値は `:custom`、フックは `:hook`、キーバインドは `:bind` にグループ化する。
- **`:hook` / `:bind` から参照する自前の関数は `:init` ではなく `:preface` で定義する。**
  これらの節の関数は `:commands` 経由で autoload 対象になり、バイトコンパイル時に
  `declare-function` が名前を先に登録する。`:init`（処理順 100）はその後に来るため
  「`defined multiple times`」警告になる。`:preface`（処理順 79）は `:commands`（98）より
  前に処理されるので警告が出ない。副作用を伴う式は `:preface`（`eval-and-compile`）へ
  置かず `:init` に残す。同じ節の他の定義を参照する場合はブロックごと移す
  （残すと「free variable」警告へ入れ替わる）。
- 外部パッケージ宣言は `loads/inits/` の 20〜35 番台へ機能別に配置する。
- `package.el` をパッケージ管理へ使用しない。

## tty (`emacs -nw`) 対応

主用途は `emacs -nw` である。tty を既定の対象として宣言を書く。

- GUI 限定パッケージは `:if (display-graphic-p)` で分離し、tty 側の代替宣言を残す。
- tty で成立しない前提（アイコンフォント、画像表示、ピクセル単位の指定、GUI 専用の
  キーイベントやマウス操作）を、分岐なしの共通設定へ書かない。
- 遅延パッケージの `:custom` で GUI 依存の値を扱う場合は `(display-graphic-p)` の
  評価結果を使い、GUI 固定値を直接書かない。
- 端末向けの代替（`corfu-terminal`、`xclip` など）を削除・無効化しない。
- GUI 限定宣言を追加・変更した場合は、`tests/my-test-tty.el` の
  `my-test-tty--gui-only-features` と `tests/my-test-tty-live.el` の対応する検査を更新する。

## C/C++ モードのフック parity

C/C++ は tree-sitter 文法が導入済みなら `c-ts-mode` / `c++-ts-mode`、無ければ
`c-mode` / `c++-mode` を使う（`loads/inits/19-language-modes.el`）。
両系統が並行して存在することを前提に宣言を書く。

- `c-ts-mode` / `c++-ts-mode` は `derived-mode-p` 上は `c-mode` / `c++-mode` の派生だが、
  **親モードのフックは実行されない**。C/C++ 用の `:hook` を追加・変更する場合は、
  cc-mode 系と ts 系の 4 モードすべてへ登録する。
- 併せて `tests/my-test-packages.el` の `my-test-packages--ts-mode-hook-entries` を更新する。
- `derived-mode-p` で C 系を判定して cc-mode の関数（`c-beginning-of-defun` など）を
  呼ぶ分岐は、ts モードを先に振り分けて汎用関数へ倒す。ts バッファでは
  `derived-mode-p` が真になる一方、cc-mode の内部状態が無いためエラーになる。
- `c-ts-mode` を起動経路で `require` しない。文法不在の環境で警告が出て
  `make test-startup` が失敗する。可用性判定には `treesit-language-available-p` を使い、
  `use-package c-ts-mode` には `:no-require t` を付けてバイトコンパイル時の先読みも止める。
- 文法が無い環境で cc-mode へフォールバックする経路を壊さない。

## 【不変条件】yasnippet のスニペットディレクトリ

`yas-snippet-dirs` の各要素は「直下がモード名サブディレクトリである top-level dir」で
なければならない（`yas-load-directory` の契約）。モード別のリーフディレクトリ
（`.../snippets/c-mode` など）を直接入れてもスニペットは 1 件も供給されない。

- 個人スニペットは 2 レイアウトを支える。どちらの環境も実在するため、片方だけを
  前提にした簡略化をしてはならない。
  - A: `custom/snippets/<mode>/` に実体のモードディレクトリを直接置く
  - B: `custom/snippets/snippets` を外部ディレクトリへの symlink にする
- `custom/snippets` を top-level dir として登録してよいのはレイアウト A のときだけ。
  B の構成で登録すると、直下の `snippets` がモード名として `intern` され、
  架空メジャーモード `'snippets` が作られて同じツリーを二重に走査する。
- A と B が同時に成立する構成は `yas-snippet-dirs` の契約では表現できないため
  **非対応**とする。混在時は B を優先し、`display-warning` で A 側がロードされない
  ことを通知する。
- 追加スニペット集はパッケージ公開のシンボル `yasnippet-snippets-dir` で参照し、
  straight のビルドパスを自前で組み立てない。このシンボルを `:init` で
  `yas-snippet-dirs` へ事前投入することで `yasnippet-snippets-initialize` が no-op に
  なり、起動時の全ディレクトリ走査が 2 回から 1 回に減る（実測 464ms → 206ms）。
- パスの解決失敗を無警告で握り潰さない。`file-directory-p` は壊れた symlink に対して
  nil を返すため、`file-symlink-p` による破損検出を別途行う。
- `yas-snippet-dirs` が空になると `yas--load-snippet-dirs` が対話的な
  `yas-load-directory` を呼び、起動が停止する。全滅時は `yas-global-mode` を
  有効化しない。
- 上記は `tests/my-test-deferred.el` の `:deferred` タグのテスト群が固定する。

## 命名と配置

- 設定ファイルは `NN-name.el` とし、2 桁の番号で読み込み順を制御する。
- 環境固有設定には `linux-`、`windows-`、`nw-`、`cocoa-emacs-` のプレフィックスを使う。
- セクションヘッダは `;;;;; [Group] セクション名 ;;;;;` の形式にする。
- 自動生成ファイルをリポジトリルート直下へ置かない。
- 履歴、バックアップ、データベースには定義済みのパスヘルパーを使う。

## 【不変条件】タグナビゲーション

次の C/C++ タグナビゲーション用キーバインドは固定であり、変更してはならない。

| キーバインド | 機能 |
|---|---|
| `C-t d` / `C-t C-d` | 関数の定義場所の検索 (`my/gtags-find-definition`) |
| `C-t u` / `C-t C-u` | 使用箇所の検索 (`my/gtags-find-references`) |
| `C-t v` / `C-t C-v` | 変数の使用箇所の検索 (`my/gtags-find-symbol`) |
| `C-t f` / `C-t C-f` | ファイル検索 (`my/gtags-find-file`) |
| `C-t p` / `C-t C-p` | `xref-go-back` |
| `C-t n` / `C-t C-n` | `xref-go-forward` |

注意事項:

- バックエンドを変更する場合も、上記キーバインドを維持する。
- `C-t` プレフィックスは C/C++ タグナビゲーション用に予約する。
- `update-gtags` 関数は手動タグ更新用として維持する。
