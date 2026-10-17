<!-- -*- gfm -*- -->
# テスト

設定変更で既存の動作が壊れていないかを確認する。「ターゲット」は `make` に渡す検証メニューの名前。
以下のコマンドは、検証したいチェックアウトのルートで実行する。

```sh
make test           # フルスイート。lint から test-platform までを順に実行し、最初の失敗で止まる
make test-startup   # 起動確認だけ
```

変更範囲ごとに選ぶターゲットは [検証手順](../.claude/rules/verification.md) を参照する。
表示・キー・補完・端末初期化に触れたときは `make test-tty` と `make test-tty-live` も実行する。

## 必要なツール

- `make lint`: `shellcheck`（CI と同じ 0.11.0）
- `make test-setup`: `python3`、`jq`、`unzip`、`sha256sum`、`flock`
- `make test-audit-shell`、`make check-lockfile`: `python3`
- `make test-tty-live`: Linux、`script`（util-linux）、`timeout`
- `make test-gui`: GUI 対応 Emacs、`timeout`、X11 の `DISPLAY`

`make test` を通しで実行できるのは Linux だけ。macOS では標準の bash 3.2 に `mapfile` が無く `lint` で止まるため、
`make SHELL=<bash 4 以上のパス> <ターゲット>` の形で `test-tty-live` を除いて個別に実行する。

## 実行環境

- Emacs を起動するターゲット（`test-audit-shell` を除く）は、Git 追跡ファイルを作業ツリーの内容のまま（未コミットの変更も含めて）
  一時ディレクトリへ複製し、そこで実行する。未追跡のファイルは複製されず、`lint-sh` も Git 追跡中のシェルスクリプトだけを検査する。
  そのため新規ファイルは `git add` するまでテストから見えず、`custom/custom.el`、`loads/inits/99-private.el`、`var/` 配下も読み込まれない。
- `custom/custom.el` を読まないため、`my/copilot-enabled` は既定の `auto` で検証される（Node.js があれば Copilot 関連を読み込む）。
- `var/` を複製しないため tree-sitter 文法は「未導入」として扱われ、ts モードの検査は skip される。文法ありで検証するときは
  `make install-test-grammars TEST_TREESIT_DIR=<文法ディレクトリ>` で文法を用意してから
  `make test-cpp-config TEST_TREESIT_EXPECT=with TEST_TREESIT_DIR=<文法ディレクトリ>` を実行する。
  `TEST_TREESIT_EXPECT` の既定は `auto` で、`with` / `without` は文法の有無を固定する。
- worktree にはパッケージの実体（`loads/straight/repos/` と `build/`）が無いため、
  `make test STRAIGHT_DIR=$HOME/.emacs.d/loads/straight` のように `STRAIGHT_DIR` で実体を指す。
  `STRAIGHT_DIR` は末尾が `straight` のディレクトリでなければならない。
- 別ビルドの Emacs で検証するときは `EMACS=<emacs のパス>` を渡す。
- `test-setup`、`test-guards`、`test-audit-shell` は隔離した HOME で動き、`sudo`・`apt`・通信をスタブに置き換えるため実環境を変更しない。

## ターゲット一覧

### `make test` が順に実行するターゲット

| ターゲット | 検証内容 |
|---|---|
| `make lint` | `lint-sh`（shellcheck）と `lint-el`（Elisp の構文と byte compile。警告も失敗） |
| `make test-unit` | `early-init.el` のパスヘルパー |
| `make test-startup` | フル起動、init-loader のエラーログ、起動時の警告 |
| `make test-keybinding` | `C-t` の固定キーと追加キー、折りたたみのキー、`C-z` のタブ操作、パッケージ固有のキーマップ |
| `make test-cpp-config` | C/C++ のスタイル、eglot 起動条件、検索経路、cc-mode と ts モードの切替、irony ゲート、読解補助。`clangd`、GNU Global、tree-sitter 文法が無い環境では、それを使う検査を skip する |
| `make test-deferred` | `:defer` 付きパッケージの遅延ロード後の設定と、グローバルモードの有効化 |
| `make test-invariants` | グローバルモードのフック登録、feature のロード状態、ts モードと cc-mode のフック登録の一致、組み込みパッケージの固定 |
| `make test-tty` | 対話画面を開かない batch 実行で、端末向け設定が読み込まれる条件を検証 |
| `make test-tty-live` | 擬似端末（pty）で実際に `emacs -nw` を起動し、モードライン、端末初期化、`C-t`、補完ポップアップを検証 |
| `make test-setup` | 隔離した HOME での `emacs-setup.sh`（引数、ダウンロードの原子性、パッケージ復元、clangd の導入と削除） |
| `make test-guards` | テスト基盤と lint 基盤の fail-closed ガードを故障注入で検査 |
| `make test-audit` | 編集・補完・ユーティリティの入力境界とデータ保全 |
| `make test-audit-shell` | セットアップ・ベンチ・lockfile 照合の故障注入 |
| `make test-platform` | 実 OS の入力メソッド分岐、生成物パス、PATH 区切り文字 |

### 個別に実行するターゲット

| ターゲット | 内容 |
|---|---|
| `make test-gui` | Linux の実 GUI 起動とクリップボード |
| `make check-lockfile` | 導入済み repo と lockfile の照合。照合先は `STRAIGHT_DIR`、導入しないことがある repo は `LOCK_ALLOW_MISSING` で指定する（[パッケージ管理](packages.md#更新と-lockfile)） |
| `make install-test-grammars` | `TEST_TREESIT_DIR` に固定タグの C/C++ 文法を導入 |
| `make clean-test` | `tests/` 配下の byte compile 生成物を削除 |
| `make straight-thaw` | CI 専用（`CI=true`）。lockfile のリビジョンを適用し、照合まで実行 |

## `test-tty-live` の注意

- straight のビルドキャッシュを日常使いの Emacs と共有するため、その Emacs を起動したまま実行しない。
- コールドキャッシュ（未ビルド）のときは、先に `make test-startup` などの batch 系ターゲットでビルドを済ませる。
  `test-tty-live` は 1 プロセスを 180 秒で打ち切るため、ビルドが間に合わないと失敗する。

## GUI・他 OS

- `make test-gui` はクリップボードへ固定文字列を書き込むため、日常のディスプレイと分離して実行する。
  専用の straight を `make test-startup STRAIGHT_DIR=<専用の straight>` で準備してから、
  `xvfb-run -a make test-gui STRAIGHT_DIR=<専用の straight>` を実行する（`xvfb-run` が `DISPLAY` を用意する）。
- macOS / Windows の batch 起動は `.github/workflows/platform.yml` が検証する。このワークフローは手動実行のほか、起動経路を変更する PR で動く。

## 起動コストの計測

```sh
tests/my-bench-run.sh now  15 .bench/out    # 現行設定を 15 有効試行（ウォームアップ 3 回は破棄）
tests/my-bench-run.sh bare 15 .bench/out    # emacs -Q -nw の下限
tests/my-bench-summarize.sh .bench/out      # 中央値と IQR で集計
```

計測は専用の worktree で行う。変更前後を比べるときは `BENCH_REPO_ROOT=<比較する checkout>` で対象を切り替え、
両方の計測で同じ `STRAIGHT_DIR` を使う。出力先の `.bench/` は gitignored。

## CI

GitHub Actions は push と PR で、Emacs 31.1 の安定版と開発途中の snapshot 版を別々に検証する。
snapshot 版は次の Emacs で起きそうな問題を早めに見つけるための検証で、失敗しても CI 全体を失敗にはしない。
実際の Mozc、クリップボード、irony-server、Copilot との連携は `make test` でも CI でも検証しない。clangd だけは、PATH にあれば
`make test-cpp-config` が実際に起動して検証する。各ツールの導入と無いときの挙動は README の [外部ツール](../README.md#外部ツール) を参照する。
