<!-- -*- gfm -*- -->
# アーキテクチャ

設定の構造、読み込み順、ディレクトリの責務をまとめる。
使い方は [../README.md](../README.md)、キーバインド一覧は [keybindings.md](keybindings.md) を参照。

## 起動シーケンス

```text
early-init.el          OS 判定定数、パス変数、straight.el ブートストラップ、UI 抑制
    ↓
init.el                site-elisp/ を load-path に追加、use-package 設定、init-loader 起動
    ↓
loads/inits/*.el       init-loader が番号・アルファベット順でロード
```

| 段階 | 責務 |
|---|---|
| `early-init.el` | UI 描画前に実行する。OS 判定定数（`IS-MAC` / `IS-LINUX` / `IS-WINDOWS`）、全ディレクトリパス変数とパスヘルパー、`package.el` の無効化、straight.el のブートストラップ、バックアップと自動保存の保存先、フレーム生成前の UI 抑制 |
| `init.el` | 最小限のエントリーポイント。`loads/site-elisp/` を `load-path` へ追加し、straight 経由で `use-package` を導入して `init-loader` を起動する |
| `loads/inits/` | 機能別の設定モジュール群。`init-loader` が番号・アルファベット順に読み込む |

## ディレクトリ構成

| パス | 用途 |
|---|---|
| `loads/inits/` | 番号付き設定モジュール |
| `loads/site-elisp/` | ユーザー作成の Elisp ライブラリ（`load-path` に含まれる） |
| `loads/straight/` | straight.el のリポジトリとビルド成果物 |
| `custom/` | `custom.el`、YASnippet スニペット、Markdown 表示用 CSS/JS |
| `var/hist/` | 履歴、ブックマーク、TRAMP、transient、undo-fu-session |
| `var/backup/` | 自動保存・バックアップ |
| `var/package/` | eln-cache（ネイティブコンパイル）、`tree-sitter/`（文法ライブラリ）、`copilot/`（language server） |
| `tests/` | ERT による回帰テストと起動コスト計測ハーネス |
| `docs/` | 本書などの補足ドキュメントと、`docs/eval/` の実測ログ |

`var/` と `loads/straight/` は自動生成物であり、手で編集しない。
削除が必要な場合は `./emacs-setup.sh --clean` / `--clean-all` を使う。

## 設定モジュール

| ファイル | 説明 |
|---|---|
| `00-core.el` | コア設定（文字コード、バックアップ、GC 復元、基本動作） |
| `01-ui.el` | UI・フォント・フレーム設定 |
| `02-keybindings.el` | グローバルキーバインド |
| `10-functions.el` | カスタム関数（ウィンドウ操作、ファイル名コピー） |
| `18-built-in-package.el` | 組み込みパッケージ設定（eglot、treesit、tab-bar を含む。tree-sitter 文法の導入経路は `loads/site-elisp/my-treesit.el`） |
| `19-language-modes.el` | 言語モード設定（cc-mode / c-ts-mode の振り分けを含む） |
| `21-ime.el` | 日本語入力（tr-ime, mozc） |
| `22-theme.el` | テーマとモードライン（doom-themes, doom-modeline） |
| `23-visual.el` | 視覚効果・アイコン・ハイライト（pulsar, nerd-icons, rainbow-delimiters 等） |
| `24-org.el` | Org モード |
| `25-markdown.el` | Markdown（markdown-mode, markdown-toc, pandoc-mode） |
| `26-vertico.el` | ミニバッファ補完（vertico, marginalia） |
| `27-consult.el` | 検索・絞り込み（consult, embark, orderless） |
| `28-corfu.el` | バッファ内補完（corfu, corfu-terminal, cape） |
| `29-scoring.el` | スコアリング・履歴（prescient, flx） |
| `30-buffer-file.el` | バッファ・ファイル管理（recentf, dashboard, anzu, xclip） |
| `31-editing.el` | コード編集・タグナビゲーション（google-c-style, irony, ggtags, yasnippet 等） |
| `32-navigation.el` | ナビゲーション・スペルチェック（migemo, neotree, flyspell） |
| `33-vcs.el` | バージョン管理（magit, diff-hl, difftastic） |
| `34-misc.el` | ユーティリティ（which-key, vundo, undo-fu-session） |
| `35-copilot.el` | AI 補完（GitHub Copilot, Copilot Chat） |
| `99-private.el` | プライベート設定（gitignored。この環境だけの設定を置く） |

### site-elisp モジュール

| ファイル | 説明 |
|---|---|
| `my-gtags.el` | タグ検索関数群（eglot/xref 優先、`global` フォールバック） |
| `my-markdown.el` | Markdown カスタム関数群（pandoc コマンド構築、CSS 設定） |
| `my-treesit.el` | tree-sitter 文法の取得元・導入先・導入コマンド（`emacs-setup.sh --setup-treesit` からも単体ロードする） |

## 技術スタック

| カテゴリ | パッケージ |
|---|---|
| 補完 UI（ミニバッファ） | Vertico + Consult + Marginalia + Orderless + Prescient |
| 補完 UI（バッファ内） | Corfu + Cape（tty では corfu-terminal） |
| テーマ | doom-themes (doom-dracula) + doom-modeline |
| Git | Magit + diff-hl + difftastic |
| C/C++ | cc-mode + google-c-style / c-ts-mode、eglot + clangd、ggtags、irony |
| AI 補完 | GitHub Copilot（copilot.el + copilot-chat.el） |
| 日本語入力 | Mozc, Migemo, TR-ime (Windows) |
| Undo | vundo + undo-fu-session |

## パスヘルパー関数

`early-init.el` が定義する。パスは文字列でハードコーディングせず、次を使う。

| 関数 | 展開先 |
|---|---|
| `(my-set-emacs "sub/")` | `~/.emacs.d/sub/` |
| `(my-set-loads "sub/")` | `~/.emacs.d/loads/sub/` |
| `(my-set-straight "sub/")` | `~/.emacs.d/loads/straight/sub/` |
| `(my-set-custom "sub/")` | `~/.emacs.d/custom/sub/` |
| `(my-set-history "file")` | `~/.emacs.d/var/hist/file` |
| `(my-set-backup "file")` | `~/.emacs.d/var/backup/file` |
| `(my-set-package "file")` | `~/.emacs.d/var/package/file` |
| `(my-set-db "file")` | `~/.emacs.d/var/database/file` |

## 設定ファイルの命名規則

- **`NN-name.el`**: 2 桁の数字プレフィックスで読み込み順を制御する（`00` が最初、`99` が最後）。
- **番号帯**: 00=コア、01=UI、02=キーバインド、10=関数、18=組み込み、
  19=言語モード、20〜35=外部パッケージ（機能別）、99=プライベート。
- **環境プレフィックス**: プラットフォーム固有設定用に
  `linux-` / `windows-` / `nw-`（ターミナル）/ `cocoa-emacs-` を用意している。
  現在このプレフィックスを使うファイルは無く、新規に分離する場合に用いる。
- 自動生成ファイルをリポジトリルート直下へ置かない。

設定を書き足すときの `use-package` 規約と不変条件は
`.claude/rules/elisp-conventions.md` を参照する。
