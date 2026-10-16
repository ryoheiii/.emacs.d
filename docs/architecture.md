<!-- -*- gfm -*- -->
# ファイル構成

## 起動シーケンス

```text
early-init.el         OS 判定、パス変数、custom/custom.el の読み込み、straight.el のブートストラップ、UI 抑制
    ↓
init.el               loads/site-elisp/ を load-path へ追加、use-package の導入、init-loader の起動
    ↓
loads/inits/NN-*.el   init-loader が番号順に読み込む設定モジュール
```

読み込みエラーがあったときだけ `*init log*` バッファが表示される（手動で見るには `M-x init-loader-show-log`）。

## ディレクトリ構成

| パス | 用途 | Git |
|---|---|---|
| `loads/inits/` | 番号付き設定モジュール | 管理 |
| `loads/site-elisp/` | 自作の Elisp ライブラリ（`load-path` に含まれる） | 一部（`my-*.el` だけ管理） |
| `loads/straight/` | straight.el のリポジトリとビルド成果物（[パッケージ管理](packages.md)） | 一部（lockfile `versions/default.el` だけ管理） |
| `custom/` | `custom.el`、個人スニペット、Markdown プレビュー用の CSS / JS（[custom/README.md](../custom/README.md)） | 一部（`custom.el` と `snippets/` は gitignored） |
| `var/hist/` | 履歴、ブックマーク、undo 履歴などの実行時データと、irony-server の導入先（`irony/`） | gitignored |
| `var/backup/` | バックアップと自動保存 | gitignored |
| `var/package/` | ネイティブコンパイルのキャッシュ（`eln-cache/`）、tree-sitter 文法（`tree-sitter/`）、Copilot language server（`copilot/`） | gitignored |
| `tests/` | 回帰テストと起動コスト計測 | 管理 |
| `docs/` | ドキュメント（`docs/eval/` は実測ログ） | 管理 |

`var/` と `loads/straight/` は手で編集しない。削除は `./emacs-setup.sh --clean`（`var/`）または `--clean-all`（加えて `loads/straight/`）で行う。
復元できないデータや lockfile も消えるため、実行前に README の [クリーンアップ](../README.md#クリーンアップ) を確認する。

## 設定モジュール（`loads/inits/`）

init-loader は 2 桁の数字で始まるファイルを番号順に読み込み、その後に環境に応じて `linux-` / `nw-`（端末）/ `windows-` / `cocoa-emacs-`
で始まるファイルも読み込む（現在は無い）。00〜19 はコア・UI・キー・自作コマンド・組み込みパッケージ・言語モード、
20〜35 は外部パッケージ（機能別）、99 は環境固有の設定。

| ファイル | 内容 |
|---|---|
| `00-core.el` | 文字コード、バックアップ、保存時の処理、基本動作 |
| `01-ui.el` | UI、フォント、フレーム |
| `02-keybindings.el` | グローバルキーバインド |
| `10-functions.el` | 自作コマンド（ウィンドウ操作、ファイル名コピー） |
| `18-built-in-package.el` | 組み込みパッケージ（eglot、treesit、tab-bar、自動保存など） |
| `19-language-modes.el` | 言語モード（cc-mode / c-ts-mode の振り分け、折りたたみ） |
| `21-ime.el` | 日本語入力（mozc は Windows 以外、tr-ime は Windows の GUI） |
| `22-theme.el` | テーマとモードライン（doom-themes、doom-modeline） |
| `23-visual.el` | 視覚効果（pulsar、nerd-icons、rainbow-delimiters など） |
| `24-org.el` | Org（TODO キーワードと完了時刻の記録） |
| `25-markdown.el` | Markdown（markdown-mode、markdown-toc、pandoc-mode） |
| `26-vertico.el` | ミニバッファ補完（vertico、marginalia） |
| `27-consult.el` | 検索・絞り込み（consult、embark、orderless） |
| `28-corfu.el` | バッファ内補完（corfu、corfu-terminal、cape） |
| `29-scoring.el` | 候補の並び順と履歴（prescient、flx） |
| `30-buffer-file.el` | バッファ・ファイル（recentf、dashboard、anzu、xclip） |
| `31-editing.el` | 編集とタグナビゲーション（google-c-style、irony、ggtags、yasnippet、multiple-cursors など） |
| `32-navigation.el` | ナビゲーションとスペルチェック（migemo、neotree、breadcrumb、flyspell） |
| `33-vcs.el` | バージョン管理（magit、diff-hl、difftastic） |
| `34-misc.el` | ユーティリティ（which-key、vundo、undo-fu-session） |
| `35-copilot.el` | AI 補完（copilot、copilot-chat） |
| `99-private.el` | この環境だけの設定（gitignored） |

## 自作ライブラリ（`loads/site-elisp/`）

| ファイル | 内容 |
|---|---|
| `my-gtags.el` | `C-t` のタグナビゲーション。`C-t d` / `C-t u` は LSP 優先で GNU Global へフォールバック、ほかは GNU Global（[タグナビゲーション](cpp.md#タグナビゲーション)） |
| `my-lsp-nav.el` | `C-t c` / `e` / `t` / `o` / `s` の呼び出し階層・型階層・ヘッダ/ソース切替・横断シンボル検索（[コードリーディング補助](cpp.md#コードリーディング補助)） |
| `my-markdown.el` | Markdown の pandoc プレビューと CSS |
| `my-treesit.el` | tree-sitter 文法の取得元と導入コマンド |

設定を書き足すときの規約（番号帯、`use-package` の書き方、パスヘルパー、端末（`emacs -nw`）での動作を保つ条件）は
[AGENTS.md の「変更に応じて読む規約」](../AGENTS.md#変更に応じて読む規約) から該当する規約を開く。変更後の検証は [テスト](testing.md)。
