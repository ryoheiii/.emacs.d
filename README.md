<!-- -*- gfm -*- -->
# Emacs 設定リポジトリ

モジュール構成の Emacs 設定リポジトリ。C/C++ 開発、ドキュメント作成（Markdown/Org）、日本語入力（Mozc/Migemo）対応の汎用編集環境。

## 動作環境

- Emacs 30.x 以上
- Linux (Debian/Ubuntu) / WSL2 / macOS (Emacs 設定のみ。emacs-setup.sh は未対応)
- パッケージ管理: [straight.el](https://github.com/radian-software/straight.el)（`package.el` は不使用）

---

## 1. セットアップ

### 依存パッケージのインストール

``` sh
./emacs-setup.sh --setup
```

### Emacs のインストール

``` sh
# インストール可能なバージョンを確認
./emacs-setup.sh --list

# ビルド・インストール（GUI バックエンド: gtk3, lucid, pgtk, no）
./emacs-setup.sh --install <バージョン> [--gui <バックエンド>]

# 例: Emacs 30.1 を pgtk でインストール
./emacs-setup.sh --install 30.1 --gui pgtk
```

### アンインストール

``` sh
./emacs-setup.sh --uninstall
```

### クリーンアップ

``` sh
# キャッシュ・履歴・バックアップを削除（パッケージは保持）
./emacs-setup.sh --clean

# パッケージを含むすべての自動生成ファイルを削除
./emacs-setup.sh --clean-all
```

---

## 2. 初回起動後の設定

| 項目 | 手順 |
|---|---|
| Irony サーバー（非 LSP 環境） | C/C++ ファイルを開いて `M-x irony-install-server` |
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
| `var/package/` | eln-cache（ネイティブコンパイル） |

### モジュール構成

| ファイル | 説明 |
|---|---|
| `00-core.el` | コア設定（GC 復元、基本動作） |
| `01-ui.el` | UI・フォント・フレーム設定 |
| `02-keybindings.el` | キーバインド |
| `10-functions.el` | カスタム関数 |
| `18-built-in-package.el` | 組み込みパッケージ設定（eglot を含む） |
| `19-language-modes.el` | 言語モード設定 |
| `20-library.el` | 基盤ライブラリ（dash, s, diminish） |
| `21-ime.el` | 日本語入力（tr-ime, mozc） |
| `22-theme.el` | テーマとモードライン（doom-themes, doom-modeline, smart-mode-line） |
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
| `34-misc.el` | ユーティリティ（which-key, undo-fu, vundo） |
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
| C/C++ | cc-mode, google-c-style, eglot + clangd（CDB/.clangd 検出時）、ggtags（フォールバック）、irony（非 LSP 補完） |
| 日本語入力 | Mozc, Migemo, TR-ime (Windows) |
| Undo | vundo + undo-fu + undo-fu-session |

### パスヘルパー関数

`early-init.el` で定義。パスのハードコーディングを避け、以下を使用する:

| 関数 | 展開先 |
|---|---|
| `(my-set-loads "sub/")` | `~/.emacs.d/loads/sub/` |
| `(my-set-custom "sub/")` | `~/.emacs.d/custom/sub/` |
| `(my-set-history "file")` | `~/.emacs.d/var/hist/file` |
| `(my-set-backup "file")` | `~/.emacs.d/var/backup/file` |
| `(my-set-db "file")` | `~/.emacs.d/var/database/file` |

---

## 4. パッケージ管理

straight.el による Git ベースのパッケージ管理。`use-package` と `straight-use-package-by-default t` の組み合わせにより `:straight t` は暗黙的に適用される。

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
| `make lint` | Git 追跡中の設定ファイルを一時ディレクトリへ byte compile（警告は表示、エラーは失敗） |
| `make test-unit` | early-init.el のパスヘルパー |
| `make test-startup` | フル起動と init-loader エラーログ |
| `make test-keybinding` | C-t タグナビゲーションの固定キーバインド |
| `make test-cpp-config` | C/C++ スタイル・eglot 起動条件・検索経路・起動時性能設定 |
| `make test-setup` | 隔離した HOME でのセットアップスクリプト |
| `make clean-test` | tests/ 配下の byte compile 生成物を削除 |

起動検査とキーバインド検査は、Git 追跡ファイルだけを展開した一時ルートで
実行する。実行時データは一時ルートへ隔離され、ローカル専用の未追跡設定は
読み込まれない。

GitHub Actions は push と pull request で Emacs 30.1 の安定レーンと
snapshot のカナリアレーンを実行する。snapshot の失敗は non-blocking とする。
実測時間（2026-07 時点）: キャッシュミス時（全パッケージ clone）は
30.1 レーンで約 4 分 30 秒、キャッシュヒット時は約 1 分 20 秒。

---

## 6. 設定ファイルの命名規則

設定ファイル（`loads/inits/*.el`）の命名規則:

- **`NN-name.el`**: 2 桁の数字プレフィックスで読み込み順を制御（`00` が最初、`99` が最後）
- **環境プレフィックス**: プラットフォーム固有設定用
  - `linux-`, `windows-`, `nw-`（ターミナル）, `cocoa-emacs-`
  - 例: `linux-clipboard.el`
- **外部パッケージは 20〜34 番台に記述**

---

## 7. トラブルシューティング

- ターミナル利用時は `xterm-256color` を設定
- straight.el の不整合時: `M-x straight-rebuild-all` または `--clean-all` で再構築
- バッチモードでの起動検証: `make test-startup`

``` sh
make test-startup
```
