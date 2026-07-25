<!-- -*- gfm -*- -->
# Emacs 設定リポジトリ

モジュール構成の Emacs 設定リポジトリ。C/C++ 開発、ドキュメント作成（Markdown/Org）、日本語入力（Mozc/Migemo）対応の汎用編集環境。

## 動作環境

- Emacs 30.x 以上
- `make lint` に [shellcheck](https://www.shellcheck.net/) が必要（CI の ubuntu-latest には同梱）
- Linux (Debian/Ubuntu) / WSL2 / macOS (Emacs 設定のみ。emacs-setup.sh は未対応)
- パッケージ管理: [straight.el](https://github.com/radian-software/straight.el)（`package.el` は不使用）
- 主用途は端末上の `emacs -nw`（CLI モード）。GUI でも動作するが、tty での動作維持を
  優先する（変更時の規約は `AGENTS.md` の「【最優先】CLI (`emacs -nw`) 前提」を参照）

---

## 1. セットアップ

### 依存パッケージのインストール

``` sh
./emacs-setup.sh --setup

# GUI 依存を入れない（端末専用の環境向け）
./emacs-setup.sh --setup --gui no
```

`--gui` は `no` を指定したときだけ GUI パッケージ（X11 群、画像ライブラリなど）を
除外する。`gtk3` / `lucid` / `pgtk` を指定しても既定（すべて導入）と同じ結果になる。
TLS など GUI に依存しない依存は `--gui no` でも導入される。

### Emacs のインストール

``` sh
# インストール可能なバージョンを確認
./emacs-setup.sh --list

# ビルド・インストール（GUI バックエンド: gtk3, lucid, pgtk, no）
./emacs-setup.sh --install <バージョン> [--gui <バックエンド>]

# 例: Emacs 30.2 を pgtk でインストール
./emacs-setup.sh --install 30.2 --gui pgtk
```

### アンインストール

``` sh
./emacs-setup.sh --uninstall
```

### クリーンアップ

``` sh
# var/ 配下の生成物とユーザー操作履歴を削除（パッケージは保持）
./emacs-setup.sh --clean

# 上記に加えてパッケージも削除
./emacs-setup.sh --clean-all
```

**`--clean` は復元できないデータを消す。** 削除対象は再生成可能なものだけではない。

| 種別 | 例 |
|---|---|
| 再生成可能 | `var/package/` のネイティブコンパイルキャッシュ、`var/backup/` |
| **復元不可能** | ミニバッファ履歴（savehist）、カーソル位置（places）、最近使ったファイル、undo 履歴（undo-fu-session） |

---

## 2. 初回起動後の設定

いずれも任意である。実行しなくても設定は動作し、利用できない機能は自動的に
フォールバックする（「C/C++ の段階構成」を参照）。

| 項目 | 手順 |
|---|---|
| tree-sitter 文法（C/C++） | `M-x my/treesit-install-c-grammars`（`git` と C コンパイラが必要）。導入後の再起動で ts モードへ切り替わる |
| Irony サーバー（非 LSP 環境） | C/C++ ファイルを開いて `M-x irony-install-server`（`cmake` と libclang が必要） |
| Migemo 辞書 | `loads/inits/32-navigation.el` で辞書パスを確認 |
| Nerd-icons フォント | `M-x nerd-icons-install-fonts` |

---

## 3. アーキテクチャ

### 起動シーケンス

```
early-init.el          OS 判定定数、パス変数、straight.el ブートストラップ、UI 抑制
    ↓
init.el                site-elisp/ を load-path に追加、use-package 設定、init-loader 起動
    ↓
loads/inits/*.el       init-loader が番号・アルファベット順でロード
```

### ディレクトリ構成

| パス | 用途 |
|---|---|
| `loads/inits/` | 番号付き設定モジュール |
| `loads/site-elisp/` | ユーザー作成の Elisp ライブラリ（`load-path` に含まれる） |
| `loads/straight/` | straight.el のリポジトリとビルド成果物 |
| `custom/` | `custom.el`、YASnippet スニペット、Markdown 表示用 CSS/JS |
| `var/hist/` | 履歴、ブックマーク、TRAMP、transient |
| `var/backup/` | 自動保存・バックアップ |
| `var/package/` | eln-cache（ネイティブコンパイル）、`tree-sitter/`（文法ライブラリ） |
| `tests/` | ERT による回帰テストと起動コスト計測ハーネス |
| `docs/eval/` | 設計判断の根拠となる実測結果と生ログ |

### モジュール構成

| ファイル | 説明 |
|---|---|
| `00-core.el` | コア設定（GC 復元、基本動作） |
| `01-ui.el` | UI・フォント・フレーム設定 |
| `02-keybindings.el` | キーバインド |
| `10-functions.el` | カスタム関数 |
| `18-built-in-package.el` | 組み込みパッケージ設定（eglot を含む） |
| `19-language-modes.el` | 言語モード設定 |
| `21-ime.el` | 日本語入力（tr-ime, mozc） |
| `22-theme.el` | テーマとモードライン（doom-themes, doom-modeline） |
| `23-visual.el` | 視覚効果・アイコン・ハイライト（pulsar, nerd-icons, rainbow-delimiters 等） |
| `24-org.el` | Org モード |
| `25-markdown.el` | Markdown（markdown-mode, pandoc-mode） |
| `26-vertico.el` | ミニバッファ補完（vertico, marginalia） |
| `27-consult.el` | 検索・絞り込み（consult, embark, orderless） |
| `28-corfu.el` | バッファ内補完（corfu, cape） |
| `29-scoring.el` | スコアリング・履歴（prescient, flx） |
| `30-buffer-file.el` | バッファ・ファイル管理（recentf, dashboard, anzu） |
| `31-editing.el` | コード編集・タグナビゲーション（google-c-style, irony, ggtags 等） |
| `32-navigation.el` | ナビゲーション・スペルチェック（migemo, neotree, flyspell） |
| `33-vcs.el` | バージョン管理（magit, diff-hl, difftastic） |
| `34-misc.el` | ユーティリティ（which-key, vundo, undo-fu-session） |
| `99-private.el` | プライベート設定 |

#### site-elisp モジュール

| ファイル | 説明 |
|---|---|
| `my-gtags.el` | タグ検索関数群（eglot/xref 優先、global フォールバック） |
| `my-markdown.el` | Markdown カスタム関数群（pandoc コマンド構築、CSS 設定） |

### 技術スタック

| カテゴリ | パッケージ |
|---|---|
| 補完 UI | Vertico + Consult + Marginalia + Orderless + Prescient |
| バッファ内補完 | Corfu + Cape |
| テーマ | doom-themes (doom-dracula) + doom-modeline |
| Git | Magit + diff-hl + difftastic |
| C/C++ | cc-mode + google-c-style / c-ts-mode（文法導入時）、eglot + clangd（CDB/.clangd 検出時）、ggtags、irony（非 LSP 補完） |
| 日本語入力 | Mozc, Migemo, TR-ime (Windows) |
| Undo | vundo + undo-fu-session |

### C/C++ の段階構成

セットアップの自由度が低い環境（社内環境など）でも動くよう、C/C++ 関連は
**使える機構を自動選択し、使えなければ 1 段下へフォールバックする**構成にしている。
どの段でも `C-t` タグナビゲーションと補完 UI（Corfu）は同じように使える。

**メジャーモード**

| 条件 | 使われるモード |
|---|---|
| tree-sitter 文法（`c` / `cpp`）が導入済み | `c-ts-mode` / `c++-ts-mode` |
| 文法が無い、または `my/use-treesit-for-cc` が `nil` | `c-mode` / `c++-mode`（cc-mode + google-c-style） |

- 判定は `c` と `cpp` で独立して行う。片方だけ導入した環境でも壊れない。
- `.log` / `.cfg`（ログ閲覧用に c-mode を流用）と `.nut`（Squirrel）は C/C++ ではないため、
  文法が導入済みでも cc-mode のままにする。
- ts モードのインデントは google-c-style 相当（offset 4、namespace 非インデント、
  アクセス指定子は半段、case は 1 段）へ揃えてある。
- `c-toggle-auto-hungry-state` は ts モードに存在しないため、組み込み機能で再現している。
  - 自動改行: `electric-layout-mode` で
    `{` の後 / `}` の前後 / 文末の `;` の後 / アクセス指定子の `:` の後 に改行する。
    行途中（後ろに空白以外が残る）と `(` の内側（`for` の区切り）では改行しない
    （`c-hanging-semi&comma-criteria` 相当）。
    `}` の次行が `;` / `else` / `while` / `catch` のとき、および `{}` が空のときは
    1 行へ戻す（`c-cleanup-list` の `defun-close-semi` / `brace-else-brace` /
    `brace-elseif-brace` / `brace-catch-brace` / `empty-defun-braces` 相当）。
  - hungry delete: DEL を `backward-delete-char-untabify`（`'all`）へ差し替える。
    コメント・文字列の中と前置引数付きでは cc-mode と同じ通常削除に戻る
    （`kill-ring` は使わない）。リージョン選択中は選択範囲を削除する。
  - 未再現: `c-cleanup-list` の `list-close-comma` / `scope-operator`、
    ブレース初期化リストの `{` 前改行。
- 波括弧が 2 段以上開いている入力途中は tree-sitter が木全体を `ERROR` に落とし、
  既定の規則が桁 0 へ倒す。この間の桁は括弧の深さから算出して補う
  （`namespace` の波括弧は `(innamespace . 0)` に合わせて段数へ数えない）。
- **既知の使用感差分**: 上記 `ERROR` 状態の間だけ、波括弧に現れない段が 1 段
  浅くなる。該当するのは `case` ラベル配下の文（cc-mode は `case` から 1 段
  下げる）と、波括弧を省いた本体が解析できないとき（`for` で確認）である。
  構文が揃えば cc-mode + google-c-style と完全に一致する。

**補完バックエンド**

| 段 | 条件 | 使われる補完 |
|---|---|---|
| 1 | `clangd` があり `compile_commands.json` または `.clangd` を持つ | eglot（LSP） |
| 2 | 1 が不成立で `irony-server` が導入済み | irony |
| 3 | どちらも無い | cape（dabbrev / keyword / file）+ ggtags |

- eglot 管理下のバッファでは irony を自動的に止める（CAPF の競合防止）。
- `irony-server` が未導入の環境では irony をロードしない。導入は `M-x irony-install-server`。
- タグ検索（`C-t d` など）は eglot 管理下では xref へ委譲し、見つからなければ
  `global` コマンドへフォールバックする（`loads/site-elisp/my-gtags.el`）。

### パスヘルパー関数

`early-init.el` で定義。パスのハードコーディングを避け、以下を使用する:

| 関数 | 展開先 |
|---|---|
| `(my-set-loads "sub/")` | `~/.emacs.d/loads/sub/` |
| `(my-set-custom "sub/")` | `~/.emacs.d/custom/sub/` |
| `(my-set-history "file")` | `~/.emacs.d/var/hist/file` |
| `(my-set-backup "file")` | `~/.emacs.d/var/backup/file` |
| `(my-set-package "file")` | `~/.emacs.d/var/package/file` |
| `(my-set-db "file")` | `~/.emacs.d/var/database/file` |

---

## 4. パッケージ管理

straight.el による Git ベースのパッケージ管理。`use-package` と `straight-use-package-by-default t` の組み合わせにより `:straight t` は暗黙的に適用される。

### 変更検出の方式

起動時間を優先し、`early-init.el` で `straight-check-for-modifications` を
`(check-on-save find-when-checking only-once)` に設定している。既定の `find-at-startup` は
毎起動で `loads/straight/repos` 配下を `find(1)` で全走査するため、これを外している。

変更が自動記録されるのは次の場合で、通常の編集と更新はこれで足りる。

- Emacs でパッケージのファイルを保存した（`before-save-hook`）
- `M-x straight-pull-all` / `M-x straight-pull-package` で実際にマージが発生した

一方、次の場合は**自動検出されない**。`M-x straight-check-all` を実行してから利用する。

- `M-x straight-thaw-versions` でリビジョンを戻した
  （`straight-vc-git-check-out-commit` はチェックアウト成功時に変更記録を通らない）
- シェルから `loads/straight/repos/` 配下を直接 `git` 操作した

CI の `make straight-thaw` は、この理由からターゲット内で `straight-check-all` まで実行する。

`./emacs-setup.sh --extract-package` は展開後に `straight-rebuild-all` を実行するため、
手動の `straight-check-all` は不要である。`straight-rebuild-all` は変更検出を経由せず
全パッケージを無条件に再ビルドし、ビルドキャッシュの mtime も更新するため、
以降のセッションで検出漏れが起きる経路が無い。

``` sh
# パッケージのアーカイブ（バックアップ）
./emacs-setup.sh --packing-package

# パッケージの復元
./emacs-setup.sh --extract-package

# バッチモードで全パッケージをリビルド
emacs --batch --eval "(setq user-emacs-directory \"$HOME/.emacs.d\")" \
  -l early-init.el -l init.el -f straight-rebuild-all
```

### lockfile の更新

パッケージ更新時は、次の順序で設定と lockfile を同時に検証する。

1. Emacs で `M-x straight-pull-all` を実行する。
2. `make test` を実行する。
3. Emacs で `M-x straight-freeze-versions` を実行する。
4. `loads/straight/versions/default.el` を設定変更と同じコミットへ含める。

---

## 5. 回帰テスト

Emacs 標準の ERT と Makefile で、設定のユニットテスト、起動検査、
キーバインド不変条件、セットアップスクリプトを検証する。

``` sh
# lint から既存 shell テストまでを順番に実行
make test
```

| ターゲット | 検証内容 |
|---|---|
| `make test` | lint からセットアップスクリプトまでを fail-fast で一括実行 |
| `make lint` | Git 追跡中のシェルスクリプトを shellcheck、設定ファイルを一時ディレクトリへ byte compile（byte compile の警告は表示、エラーは失敗） |
| `make test-unit` | early-init.el のパスヘルパー |
| `make test-startup` | フル起動・init-loader エラーログ・起動時警告（allowlist 外は失敗） |
| `make test-keybinding` | C-t タグナビゲーションの固定キーバインド |
| `make test-cpp-config` | C/C++ スタイル・eglot 起動条件・検索経路・起動時性能設定・tree-sitter 段階移行・irony ゲート |
| `make test-deferred` | `:defer N` 遅延パッケージの `:config` とグローバルモード有効化 |
| `make test-invariants` | グローバルモードのフック登録・feature ロード状態・ts モードのフック parity |
| `make test-tty` | 非 GUI 分岐の tty ロード条件（corfu-terminal、GUI 限定宣言の eager 化カナリア） |
| `make test-tty-live` | 実 pty での `emacs -nw` 起動（モード活性化・モードライン・端末初期化・C-t 表） |
| `make test-setup` | 隔離した HOME で `test-emacs-setup.sh` を実行（引数パース、`--list` の抽出、ダウンロードの原子性、パッケージ復元のトランザクション、サンドボックスガード） |
| `make clean-test` | tests/ 配下の byte compile 生成物を削除 |

起動検査とキーバインド検査は、Git 追跡ファイルだけを展開した一時ルートで
実行する。実行時データは一時ルートへ隔離され、ローカル専用の未追跡設定は
読み込まれない。

このため `var/` 配下は一時ルートへ展開されず、`make test` から見た
tree-sitter 文法は通常「未導入」になる（システムの共有ライブラリ検索パスに
文法が置かれている場合は導入済みとして扱われる）。C/C++ のスタイル検査は
文法の可用性で排他になっており、未導入なら cc-mode 側
（`my-test-cpp-config-google-style`）、導入済みなら ts 側
（`my-test-cpp-config-c-ts-indent-google-equivalent`）が走る。
入力途中（`ERROR` 状態）の桁を固定する `my-test-cpp-config-c-ts-error-indent` も
文法が必要なため、未導入環境では skip される。
ts 側を明示的に走らせる場合は、文法を置いたディレクトリを
`treesit-extra-load-path` へ加えた状態で ERT を実行する。

`test-tty-live` は `script`（util-linux）と `timeout` を使う Linux 前提の
ターゲットで、実際の起動ライフサイクル（after-init → tty 端末初期化 →
emacs-startup → window-setup）を pty 上で再現して検証する。注意点:

- 非 Linux 環境では明示エラーで失敗する（`make test` のフル実行は Linux 前提。
  macOS では個別ターゲットで代替する）
- straight のビルドキャッシュを対話セッションと共有するため、対話 Emacs を
  起動したままの実行は避ける
- コールドキャッシュ時は先に `make test-startup` 等の batch 系ターゲットで
  ビルドを温めてから実行する（timeout 180 秒のため）

### 起動コストの計測

起動時間の内訳を実 pty で測るハーネスを `tests/` に置く。`make` ターゲットではなく
直接実行する（回帰テストではないため `make test` には含めない）。

``` sh
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
  異なる結果が出ることがある（`docs/eval/7-elpaca-ceiling/CORRECTION.md`）
- 修正前後を比較する場合は `git checkout <rev> -- <file>` で作業ツリーを一時的に戻し、
  同一ハーネス・同一キャッシュで測る（ハーネスは作業ツリーの差分を取り込む）
- `emacs-init-time` は `after-init-hook` の直前で止まるため、この設定の主要コストを
  計測窓の外へ出す。判断には使わない
- 出力先の `.bench/` は gitignored

GitHub Actions は push と pull request で Emacs 30.2 の安定レーンと
snapshot のカナリアレーンを実行する。snapshot の失敗は non-blocking とする。
実測時間（2026-07 時点）: キャッシュミス時（全パッケージ clone）は
安定レーンで約 4 分 30 秒、キャッシュヒット時は約 1 分 20 秒。

---

## 6. 設定ファイルの命名規則

設定ファイル（`loads/inits/*.el`）の命名規則:

- **`NN-name.el`**: 2 桁の数字プレフィックスで読み込み順を制御（`00` が最初、`99` が最後）
- **環境プレフィックス**: プラットフォーム固有設定用
  - `linux-`, `windows-`, `nw-`（ターミナル）, `cocoa-emacs-`
  - 現在このプレフィックスを使うファイルは無い。新規に分離する場合に用いる
- **外部パッケージは 20〜34 番台に記述**

---

## 7. トラブルシューティング

- ターミナル利用時は `xterm-256color` を設定
- straight.el の不整合時: `M-x straight-rebuild-all` または `--clean-all` で再構築
- Emacs 外でパッケージを書き換えた後にビルドが古いままのとき: `M-x straight-check-all`（「4. パッケージ管理」の変更検出の方式を参照）
- バッチモードでの起動検証: `make test-startup`

``` sh
make test-startup
```
