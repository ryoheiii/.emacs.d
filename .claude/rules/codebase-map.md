---
description: Emacs 設定の起動順、ディレクトリ責務、パス構築規約を定義する
globs: ["early-init.el", "init.el", "loads/**/*.el"]
---

# コードベース構成

## 起動シーケンス

1. `early-init.el` — Emacs UI 描画前に実行する。OS 判定定数（`IS-MAC`、`IS-LINUX`、`IS-WINDOWS`）、全ディレクトリパス変数（`my-emacs-dir`、`my-loads-dir`、`my-var-dir` など）、パスヘルパーを定義する。さらに `package.el` の無効化、`straight.el` のブートストラップ、バックアップ／自動保存先の設定、不要な UI 要素の除去を行う。
2. `init.el` — 最小限のエントリーポイントである。`loads/site-elisp/` を `load-path` に加え、straight 経由で `use-package` をインストールし、`init-loader` で `loads/inits/` 内の設定を読み込む。
3. `loads/inits/` — `init-loader` が番号／アルファベット順に読み込むモジュール型設定ファイル群である。

## ディレクトリ構成

| パス | 用途 |
|---|---|
| `loads/inits/` | 番号付き設定モジュール。00=コア、01=UI、02=キーバインド、10=関数、18=組み込み、19=言語モード、20〜34=外部パッケージ（機能別）、99=プライベート |
| `loads/site-elisp/` | ユーザー作成の Elisp ライブラリ。`load-path` に含まれる |
| `loads/straight/` | straight.el のリポジトリとビルド成果物 |
| `custom/` | `custom.el`、YASnippet、Markdown 表示用 CSS／JS などのカスタムデータ |
| `var/hist/` | 履歴、ブックマーク、TRAMP 永続化、transient の実行時データ |
| `var/backup/` | 自動保存・バックアップファイルの実行時生成先 |
| `var/package/` | `eln-cache` などネイティブコンパイル成果物と `tree-sitter/`（文法ライブラリ）の実行時生成先 |
| `tests/` | ERT による回帰テストと起動コスト計測ハーネス。`Makefile` の各ターゲットが参照する |
| `docs/eval/` | 設計判断の根拠となる実測結果と生ログ |

## 番号プレフィックス

- ファイル名は `NN-name.el` とし、00 を最初、99 を最後に読み込む。
- 既存の番号帯の責務を変えず、関連する設定を同じモジュールへまとめる。
- 外部パッケージ宣言の配置規約は `elisp-conventions.md` を参照する。

## パスヘルパー関数

パスを文字列でハードコーディングしてはならない。新しい保存先やパッケージパスには次を使う。

| 関数 | 対応するパス |
|---|---|
| `(my-set-emacs "subdir/")` | `~/.emacs.d/subdir/` |
| `(my-set-loads "subdir/")` | `~/.emacs.d/loads/subdir/` |
| `(my-set-straight "subdir/")` | `~/.emacs.d/loads/straight/subdir/` |
| `(my-set-custom "subdir/")` | `~/.emacs.d/custom/subdir/` |
| `(my-set-history "filename")` | `~/.emacs.d/var/hist/filename` |
| `(my-set-backup "filename")` | `~/.emacs.d/var/backup/filename` |
| `(my-set-package "filename")` | `~/.emacs.d/var/package/filename` |
| `(my-set-db "filename")` | `~/.emacs.d/var/database/filename` |

これらの基底パスは `early-init.el` が定義する。呼び出し側でホームディレクトリやリポジトリ絶対パスを再構築しない。

`my-set-straight` で straight のビルド成果物を指す場合、ディレクトリ名を `"build/"` と
文字列で書かず、公開変数 `straight-build-dir` を経由する
（`straight-use-version-specific-build-dir` を有効にすると `"build-30.2"` などへ変わる）。
外部パッケージ同梱のデータファイルは、そもそもパッケージ自身が公開する変数
（例: `yasnippet-snippets-dir`）で参照し、ビルドレイアウトを自前で再現しない。
