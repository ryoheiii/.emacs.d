---
description: Emacs Lisp の宣言、命名、配置と固定キーバインドを定義する
globs: ["**/*.el"]
---

# Emacs Lisp 規約

## `use-package`

- 外部パッケージは `use-package` で宣言する。`straight-use-package-by-default t` により `:straight t` は暗黙に適用される。
- 組み込みパッケージには必ず `:straight nil` を指定する。
- 設定値は `:custom`、フックは `:hook`、キーバインドは `:bind` にグループ化する。
- 外部パッケージ宣言は `loads/inits/` の 20〜34 番台へ機能別に配置する。
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
