<!-- -*- gfm -*- -->
# ファイル構成

共通の設定を変更するときは下の設定モジュール一覧から対象を探す。この PC だけの設定を書く場所は
README の [環境ごとの設定](../README.md#環境ごとの設定) を参照。

## 起動シーケンス

```text
early-init.el         OS 判定、保存先の指定・旧配置の移行、custom/custom.el の読み込み、straight.el の準備、起動中の画面表示の抑制
    ↓
init.el               loads/site-elisp/ を load-path へ追加、use-package の導入、init-loader の起動
    ↓
loads/inits/NN-*.el   init-loader が番号順に読み込む設定モジュール
```

読み込みエラーがあったときだけ `*init log*` バッファが表示される（手動で見るには `M-x init-loader-show-log`）。
`load-path` は、Emacs が Elisp ライブラリを読み込むときに探すディレクトリの一覧。

## ディレクトリ構成

表の「gitignored」は Git の追跡対象外という意味で、他の環境へ clone してもコピーされない。

| パス | 用途 | Git |
|---|---|---|
| `loads/inits/` | 番号付き設定モジュール | 管理 |
| `loads/site-elisp/` | 自作の Elisp ライブラリ（`load-path` に含まれる） | 一部（`my-*.el` だけ管理） |
| `loads/straight/` | straight.el のリポジトリとビルド成果物（[パッケージ管理](packages.md)） | 一部（lockfile `versions/default.el` だけ管理） |
| `custom/` | `custom.el`、個人スニペット、Markdown プレビュー用の CSS / JS（[custom/README.md](../custom/README.md)） | 一部（`custom.el` と `snippets/` は gitignored） |
| `var/state/` | 保持する実行時データ。履歴、ブックマーク、undo、プロジェクト一覧、Transient 設定、URL Cookie、保存したチャット、`database/` | gitignored |
| `var/backup/` | バックアップと自動保存 | gitignored |
| `var/packages/` | tree-sitter 文法（`tree-sitter/`）、Copilot language server（`copilot/`）、irony-server（`irony/`） | gitignored |
| `var/cache/` | 再作成できるデータ。ネイティブコンパイル（`eln-cache/`）、URL、Copilot Chat のトークン・モデル一覧、ベンチ作業領域（`bench/`） | gitignored |
| `var/tmp/` | Emacs の `temporary-file-directory`。`make-temp-file` などが作る一時ファイル | gitignored |
| `tests/` | 回帰テストと起動コスト計測 | 管理 |
| `docs/` | ドキュメント（`docs/eval/` は過去の計測記録と生ログ） | 管理 |
| `.github/workflows/` | Linux の回帰テストと macOS / Windows の起動検証 | 管理 |
| `.claude/` | 共通の規約・スキルと Claude Code の設定 | 一部（ローカル設定・作業領域は gitignored） |
| `.codex/` | Codex の設定と共通スキルへのリンク | 管理 |

設定値・フック・キーバインドは `loads/inits/` に置き、まとまった処理は
`loads/site-elisp/` に置く。短い補助関数は設定のそばに残し、処理を追うためのファイル移動を増やさない。
`emacs-setup.sh` は単独で実行できる入口、`test-emacs-setup.sh` はその検証としてルートに置く。

## 実行時データと旧配置からの移行

`var` は実行中に変わるデータをまとめる名前として維持する。直下を `state`（保持するデータ）、
`cache`（再作成できるデータ）、`backup`（復旧用）、`packages`（導入物）、`tmp`（一時ファイル）に分ける。
既存のパスヘルパーは互換性のため維持し、`my-set-history` は `var/state/` を指す。

更新前に、同じ設定を使う Emacs をすべて終了する。次の起動時に `early-init.el` が `my-runtime.el` を読み、
履歴やパッケージのロードより前に次を移す。内容は書き換えず、移行済みの項目は再度移さない。

| 旧配置 | 移行先 |
|---|---|
| `var/hist/` | `var/state/`（中の `irony/` は `var/packages/irony/`） |
| `var/package/` | `var/packages/`（中の `eln-cache/` は `var/cache/eln-cache/`） |
| `var/database/` | `var/state/database/` |
| 直下の `projects.eld` / `projects`、`transient/`、`url/`、`request/`、`copilot-chat/` | `var/state/` 内の同名項目 |
| 旧 `url/cache/`（親の移行後は `var/state/url/cache/`） | `var/cache/url/`（Cookie と履歴は `var/state/url/` に保持） |
| 直下の `.cache/copilot/` | `var/packages/copilot/` |
| 直下の `eln-cache/` | `var/cache/legacy-eln-cache/`（現用キャッシュとは混ぜず保全） |
| 直下の `.bench/` | `var/cache/bench/` |

移行元と移行先に同名ファイルがある場合や、移行対象自体がリンクの場合は、上書きせず起動時にエラーを出す。
移動済みの項目はそのまま残る。表示されたパスの両方を保全し、衝突やリンクを手動で解決してから再起動する。
旧 `projects` だけがある環境では、そのファイルを引き続きプロジェクト一覧として使う。
`.cache/` 自体がリンクの場合は、その中身を含め変更しない。未知の項目が残っている場合も削除しない。空になった場合だけディレクトリを取り除く。

Copilot 本体の既定の導入先は `.cache/copilot/` で、`copilot-install-server` が作成する。
本設定では `var/packages/copilot/` を指定する。ネイティブコンパイルの既定は直下の `eln-cache/` で、
設定を読まないバッチや独自の起動方法では保存先変更が効かない。パッケージのビルド・復元・文法導入では
先に `early-init.el` を読み込む。手動のバッチ実行も [手動リビルド](packages.md#手動リビルド) と同じ順序にする。

外部ツールの `TMPDIR`、OS のソケット、GitHub の共有認証ファイル（`~/.config/github-copilot/`）は移さない。
Copilot Chat の旧 `~/.cache/` 内のキャッシュはそのまま残し、新しい保存先で再取得する。
`loads/straight/` は依存パッケージ基盤として従来の位置を維持する。

`var/` と `loads/straight/` は手で編集しない。キャッシュの削除は `./emacs-setup.sh --clean-cache` を使う。
`--clean` は `var/` 全体、`--clean-all` は加えて `loads/straight/` を消すため、
実行前に README の [クリーンアップ](../README.md#クリーンアップ) を確認する。

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
| `my-c-ts-edit.el` | C/C++ ts モードの自動改行、空白削除、入力途中のインデント。`19-language-modes.el` から読み込む |
| `my-gtags.el` | `C-t` のタグナビゲーション。`C-t d` / `C-t u` は LSP 優先で GNU Global へフォールバック、ほかは GNU Global（[タグナビゲーション](cpp.md#タグナビゲーション)） |
| `my-lsp-nav.el` | `C-t c` / `e` / `t` / `o` / `s` の呼び出し階層・型階層・ヘッダ/ソース切替・横断シンボル検索（[コードリーディング補助](cpp.md#コードリーディング補助)） |
| `my-markdown.el` | Markdown の編集補助と HTML 出力。mdh を優先し、無ければ Pandoc を使う |
| `my-node-path.el` | fnm、nvm、オフライン導入先からの Node.js 検出。`35-copilot.el` が可用性判定の前に呼ぶ |
| `my-runtime.el` | 旧保存先のデータを上書きせず用途別に移行する。`early-init.el` が絶対パスで読み込む |
| `my-snippets.el` | 個人スニペットの配置解決と初期化。`31-editing.el` から読み込む |
| `my-treesit.el` | tree-sitter 文法の取得元と導入コマンド |

`my-c-ts-edit.el`、`my-node-path.el`、`my-snippets.el` は補助処理の定義を持つ。
モードの有効化や PATH の更新は、設定モジュールから必要な関数を呼ぶ時点で行う。
これらを読み込んでも、c-ts-mode・treesit・yasnippet・Copilot 本体の遅延ロードは維持される。

設定を書き足すときの規約（番号帯、`use-package` の書き方、パスヘルパー、端末（`emacs -nw`）での動作を保つ条件）は
[AGENTS.md の「変更に応じて読む規約」](../AGENTS.md#変更に応じて読む規約) から該当する規約を開く。変更後の検証は [テスト](testing.md)。
