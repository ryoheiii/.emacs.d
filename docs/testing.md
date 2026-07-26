<!-- -*- gfm -*- -->
# 回帰テストと計測

Emacs 標準の ERT と Makefile で、設定のユニットテスト、起動検査、
キーバインド不変条件、tty 動作、セットアップスクリプトを検証する。

```sh
# lint からセットアップスクリプトまでを fail-fast で一括実行
make test
```

## ターゲット一覧

| ターゲット | 検証内容 |
|---|---|
| `make test` | 下記すべてを fail-fast で一括実行 |
| `make lint` | Git 追跡中のシェルスクリプトを shellcheck、設定ファイルを一時ディレクトリへ byte compile（byte compile の警告は表示、エラーは失敗） |
| `make test-unit` | `early-init.el` のパスヘルパー |
| `make test-startup` | フル起動・init-loader エラーログ・起動時警告（allowlist 外は失敗） |
| `make test-keybinding` | `C-t` タグナビゲーションの固定キーバインド |
| `make test-cpp-config` | C/C++ スタイル・eglot 起動条件・検索経路・起動時性能設定・tree-sitter 段階移行・irony ゲート |
| `make test-deferred` | `:defer N` 遅延パッケージの `:config` とグローバルモード有効化 |
| `make test-invariants` | グローバルモードのフック登録・feature ロード状態・ts モードのフック parity・Copilot のロードゲート |
| `make test-tty` | 非 GUI 分岐の tty ロード条件（corfu-terminal、GUI 限定宣言の eager 化カナリア） |
| `make test-tty-live` | 実 pty での `emacs -nw` 起動（モード活性化・モードライン・端末初期化・`C-t` 表） |
| `make test-setup` | 隔離した HOME で `test-emacs-setup.sh` を実行（引数パース、`--list` の抽出、ダウンロードの原子性、パッケージ復元のトランザクション、サンドボックスガード） |
| `make clean-test` | `tests/` 配下の byte compile 生成物を削除 |
| `make straight-thaw` | CI 専用。lockfile のリビジョンを適用して `straight-check-all` まで実行する（`CI=true` 以外では実行できない） |

`make lint` は [shellcheck](https://www.shellcheck.net/) を必要とする
（CI の ubuntu-latest には同梱されている）。

## 変更範囲ごとの最小検証

`make test` を通しで回さない場合は、変更範囲に対応するターゲットを実行する。

| 変更範囲 | 実行するターゲット |
|---|---|
| 設定ファイルの構文・byte compile 警告、シェルスクリプト | `make lint` |
| `early-init.el` のパスヘルパー、バックアップ先 | `make test-unit` |
| 起動経路の設定全般 | `make test-startup` |
| `C-t` タグナビゲーションのキーバインド | `make test-keybinding` |
| C/C++ 設定（スタイル、eglot 起動条件、検索経路、tree-sitter 切替、irony ゲート） | `make test-cpp-config` |
| `:defer` 付きパッケージ宣言 | `make test-deferred` |
| グローバルモードのフック登録、feature のロード状態、ts モードのフック parity | `make test-invariants` |
| 表示・モードライン・キーバインド・補完・クリップボード・端末初期化・GUI 分岐 | `make test-tty` と `make test-tty-live` |
| `emacs-setup.sh` | `make test-setup` |

## テストの実行環境

起動検査とキーバインド検査は、**Git 追跡ファイルだけを展開した一時ルート**で実行する
（未コミットの作業ツリー差分は適用される）。実行時データは一時ルートへ隔離され、
`custom/custom.el` や `loads/inits/99-private.el` のようなローカル専用の未追跡設定は
読み込まれない。

このため `var/` 配下は一時ルートへ展開されず、`make test` から見た
tree-sitter 文法は通常「未導入」になる（システムの共有ライブラリ検索パスに
文法が置かれている場合は導入済みとして扱われる）。C/C++ のスタイル検査は
文法の可用性で排他になっており、未導入なら cc-mode 側
（`my-test-cpp-config-google-style`）、導入済みなら ts 側
（`my-test-cpp-config-c-ts-indent-google-equivalent`）が走る。
入力途中（`ERROR` 状態）の桁を固定する `my-test-cpp-config-c-ts-error-indent`
などの ts 専用検査も文法が要るため、未導入環境では skip される。
ts 側を明示的に走らせる場合は、文法を置いたディレクトリを
`treesit-extra-load-path` へ加えた状態で ERT を実行する。

worktree で検証する場合は、パッケージ実体を共有するため
`make test STRAIGHT_DIR=$HOME/.emacs.d/loads/straight` のように指定する。

## `test-tty-live` の前提

`script`（util-linux）と `timeout` を使う Linux 前提のターゲットで、
実際の起動ライフサイクル（after-init → tty 端末初期化 → emacs-startup → window-setup）を
pty 上で再現して検証する。

- 非 Linux 環境では明示エラーで失敗する（`make test` のフル実行は Linux 前提。
  macOS では個別ターゲットで代替する）
- straight のビルドキャッシュを対話セッションと共有するため、対話 Emacs を
  起動したままの実行は避ける
- コールドキャッシュ時は先に `make test-startup` 等の batch 系ターゲットで
  ビルドを温めてから実行する（timeout 180 秒のため）

## 起動コストの計測

起動時間の内訳を実 pty で測るハーネスを `tests/` に置く。`make` ターゲットではなく
直接実行する（回帰テストではないため `make test` には含めない）。

```sh
# 現行設定を 15 有効試行（ウォームアップ 3 回は破棄）
tests/my-bench-run.sh now  15 .bench/out

# emacs -Q -nw の下限
tests/my-bench-run.sh bare 15 .bench/out

# 中央値と IQR で集計
tests/my-bench-summarize.sh .bench/out
```

`tests/my-bench-startup.el` が `window-setup-hook` 到達（t1）と遅延ロード完了（t3）の
経過時間、および `use-package` 宣言ごとのコストを外部／組み込みへ分類して出力する。
`use-package-compute-statistics` を使うため計測用パッケージの追加は不要である。

注意点:

- **worktree で計測しない。** パッケージキャッシュを実体からコピーすると実環境と
  異なる結果が出ることがある（`eval/7-elpaca-ceiling/CORRECTION.md`）
- 修正前後を比較する場合は `git checkout <rev> -- <file>` で作業ツリーを一時的に戻し、
  同一ハーネス・同一キャッシュで測る（ハーネスは作業ツリーの差分を取り込む）
- `emacs-init-time` は `after-init-hook` の直前で止まるため、この設定の主要コストを
  計測窓の外へ出す。判断には使わない
- 出力先の `.bench/` は gitignored

## CI

GitHub Actions は push と pull request で Emacs 30.2 の安定レーンと
snapshot のカナリアレーンを実行する。snapshot の失敗は non-blocking とする。

実測時間（2026-07 時点）: キャッシュミス時（全パッケージ clone）は
安定レーンで約 4 分 30 秒、キャッシュヒット時は約 1 分 20 秒。
Copilot 関連パッケージの追加により、キャッシュミス時はこれより伸びる。
