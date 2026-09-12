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
| `make test` | lint と test-* を fail-fast で一括実行（文法ありレーンは別途） |
| `make lint` | `lint-sh` と `lint-el` をまとめて実行 |
| `make lint-sh` | Git 追跡中のシェルスクリプトを shellcheck |
| `make lint-el` | 全管理対象 Elisp の構文と設定の byte compile（警告・エラーは失敗） |
| `make test-unit` | `early-init.el` のパスヘルパー |
| `make test-startup` | フル起動・init-loader エラーログ・起動時警告（allowlist 外は失敗） |
| `make test-keybinding` | `C-t` タグナビゲーションの固定キーバインド |
| `make test-cpp-config` | C/C++ スタイル・eglot 起動条件・検索経路・起動時性能設定・tree-sitter 段階移行・irony ゲート |
| `make test-deferred` | `:defer N` 遅延パッケージの `:config` とグローバルモード有効化 |
| `make test-invariants` | グローバルモードのフック登録・feature ロード状態・ts モードのフック parity・Copilot のロードゲート |
| `make test-tty` | 非 GUI 分岐の tty ロード条件（corfu-terminal、GUI 限定宣言の eager 化カナリア） |
| `make test-tty-live` | 実 pty での `emacs -nw` 起動（モード活性化・モードライン・端末初期化・`C-t` 表） |
| `make test-setup` | 隔離した HOME で `test-emacs-setup.sh` を実行（引数パース、`--list` の抽出、ダウンロードの原子性、パッケージ復元のトランザクション、サンドボックスガード） |
| `make test-guards` | テスト基盤と lint 基盤自身の fail-closed ガードを故障注入で検査（`test-emacs-setup.sh` のガード、`lint-sh` の環境分離、`run_trial` の失敗検査） |
| `make test-audit` | 編集・補完・ユーティリティの入力境界とデータ保全 |
| `make test-audit-shell` | セットアップ・レビュー・ベンチ・lock照合の故障注入 |
| `make test-platform` | 実 OS の入力メソッド分岐・生成物パス・PATH 区切り文字 |
| `make test-gui` | Linux の実 GUI 起動・モードライン・Corfu 子フレームと確定・clipboard（通常の `make test` とは別） |
| `make check-lockfile` | 導入済み repo の HEAD・変更・過不足と lockfile を照合 |
| `make install-test-grammars` | `TEST_TREESIT_DIR` に固定タグの C/C++ 文法を導入 |
| `make clean-test` | `tests/` 配下の byte compile 生成物を削除 |
| `make straight-thaw` | CI 専用。lockfile のリビジョンを適用して `straight-check-all` と lockfile 照合まで実行する（`CI=true` 以外では実行できない） |

`make lint` は [shellcheck](https://www.shellcheck.net/) を必要とする。
CI は runner 同梱版を使わず、`.github/workflows/test.yml` で **0.11.0 を明示導入**する。
同梱版はバージョンが固定されず、`ubuntu-latest` の実体が上がるとチェック集合が変わって
無関係な変更が落ちるためである。バージョンを上げるときは同ファイルの
`SHELLCHECK_VERSION` と `SHELLCHECK_SHA256` を両方更新し、ローカルの shellcheck も
同じ版へ揃えてから `make lint` が通ることを確認する。

`make lint-sh` は `--norc` と `env -u SHELLCHECK_OPTS` を付けて実行する。
`.shellcheckrc`（祖先探索ではドット無しの `shellcheckrc` も読まれる）と `SHELLCHECK_OPTS` は
どちらも検査内容を書き換えるため、個人環境によって結果が変わらないようにしている。

この 2 つはどちらが外れても通常の実行では気づけないため、`make test-guards` が
`SHELLCHECK_OPTS=--enable=all` と `$HOME/.shellcheckrc` を注入して `make lint-sh` が
影響を受けないことを検査する。注入が実際にレバーとして効くこと（optional チェックが
本当に増えること）も正の対照で確かめる。リポジトリ外の祖先に `.shellcheckrc` がある
環境では `$HOME` フォールバックが使われないため、該当の 2 件は `SKIP:` になる。

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
| テスト基盤・lint 基盤（`test-emacs-setup.sh`、`tests/my-bench-run.sh`、`Makefile` の `lint*` / `test-*`） | `make test-guards` |

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
ts 側を明示的に走らせる場合は `TEST_TREESIT_EXPECT=with` と
`TEST_TREESIT_DIR=/文法のディレクトリ` を指定する。文法は一時ルートへ複製され、
必須のテストが実行されない場合は失敗する。

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

## GUI・他 OS・非native ビルド

`make test-gui` は GUI 対応 Emacs、X11 の `DISPLAY`、`timeout` が必要。
クリップボードへ固定文字列を書き込むため、日常のディスプレイと分離した Xvfb で実行する。
パッケージは事前に専用の `STRAIGHT_DIR` へ準備する。

```sh
make test-startup STRAIGHT_DIR=/専用/straight
xvfb-run -a make test-gui STRAIGHT_DIR=/専用/straight
make test-tty test-tty-live STRAIGHT_DIR=/専用/straight
```

非native ビルドは、Emacs を `--without-native-compilation` でビルドした実行ファイルを
`EMACS=/専用/emacs/bin/emacs` で各ターゲットへ渡す。`make test-platform` の環境行が
`native-comp=nil` であることも確認する（JIT の停止だけでは代替しない）。

macOS / Windows のバッチ起動は `.github/workflows/platform.yml` で Emacs 30.2 と
lockfile の組合せを検証する。手動実行、または設定変更の PR で起動する。
macOS では実 BSD `mv` によるパッケージアーカイブの置換・復元も検査する。
Linux 固有のビルド・apt 導入は macOS / Windows の対象外。
Windows の検査はバッチ起動と IME の GUI ガード・パスに限定し、GUI の日本語入力は含まない。

Emacs 30 の tty 補完は `corfu-terminal`、標準の `tty-child-frames` がある Emacs 31 以降は
Corfu 本体を使う。snapshot の文法あり C/C++ 互換性は #19 で別途検証する。

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

- 専用 worktree で計測する。`STRAIGHT_DIR` は両側で同一の依存状態を持つ実体を指定する。
  日常利用の cache への書込みを避ける場合は、独立した複製を1回作成して比較の両側へ使う。
  build 内の絶対 symlink が複製元を向いていないことも確認する。
- 修正済みの同一ハーネスから `BENCH_REPO_ROOT=/比較対象のcheckout` で対象を選び、
  修正前後とも15有効試行を測る。main の作業ファイルを checkout で戻さない。
  過去の計測訂正（`eval/7-elpaca-ceiling/CORRECTION.md`）を踏まえ、
  異なる cache・ハーネス・環境の結果を同条件の比較として扱わない。
- raw ログは実行ごとのディレクトリへ保存し、完走時だけ manifest を公開する。
  集計は manifest に記録された有効試行だけを使う。manifest のない旧計測は再計測する。
- `emacs-init-time` は `after-init-hook` の直前で止まるため、この設定の主要コストを
  計測窓の外へ出す。判断には使わない
- 出力先の `.bench/` は gitignored

## CI

GitHub Actions は push と pull request で Emacs 30.2 の安定レーンと
snapshot のカナリアレーンを実行する。snapshot の失敗は non-blocking とする。

実測時間（2026-07 時点）: キャッシュミス時（全パッケージ clone）は
安定レーンで約 4 分 30 秒、キャッシュヒット時は約 1 分 20 秒。
Copilot 関連パッケージの追加により、キャッシュミス時はこれより伸びる。

## 監査で追加した検証

- `make test-audit`: シンボル置換の範囲・undo・失敗時保全、Global解析、補完、版選択、ユーティリティ、Markdown。
- `make test-audit-shell`: 一時 HOME とスタブで archive/Node/レビュー公開の失敗、ベンチ有効試行、lock照合を検証する。Python 3 が必要。
- `make lint-el`: 全管理対象 Elisp の構文を検査し、early-init/init/inits/site-elisp を一時出力先へコンパイルする。警告は未許可として失敗する。
- `make check-lockfile STRAIGHT_DIR=/隔離した/straight`: lockfile の再現性を変更なしで検査する。HEAD差分と未コミット変更、ロック外repo、未許可の欠落を拒否する。

文法の両レーンは次で再現できる。文法ありでは実編集5件、文法なしでは cc-mode の2件が必須となる。

```sh
make test-cpp-config TEST_TREESIT_EXPECT=without
mkdir -p /tmp/emacs-test-grammars
make install-test-grammars TEST_TREESIT_DIR=/tmp/emacs-test-grammars
make test-cpp-config TEST_TREESIT_EXPECT=with TEST_TREESIT_DIR=/tmp/emacs-test-grammars
```

`test-tty-live` は同じ隔離 HOME を使う2プロセスで実行し、圧縮ファイルの起動引数、
固定 CAPF 候補の表示・確定、保存後の undo 再読込、固定入力メソッドの状態遷移を確認する。
undo 検証だけは fixture 自体が `/tmp` 内にあるため、一時ファイルの除外を局所的に無効化する。

clipboard の ERT は転送コールバックに渡る文字列を固定fixtureで確認する。
実 Mozc・clipboard・clangd・irony・Copilot の再現手順と実測結果は
[tty の実サービス検証](integration.md) を参照する。これらは通常 CI の保証範囲外である。
