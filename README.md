<!-- -*- gfm -*- -->
# Emacs 設定リポジトリ

モジュール構成の Emacs 設定リポジトリ。C/C++ 開発、ドキュメント作成（Markdown/Org）、日本語入力（Mozc/Migemo）対応の汎用編集環境。

## 動作環境

- Emacs 30.x 以上
- Linux / WSL2 / macOS
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
| Irony サーバー | C/C++ ファイルを開いて `M-x irony-install-server` |
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
| `18-built-in-package.el` | 組み込みパッケージ設定 |
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
| `31-editing.el` | コード編集・タグナビゲーション（yasnippet, ggtags, multiple-cursors） |
| `32-navigation.el` | ナビゲーション・スペルチェック（migemo, neotree, flyspell） |
| `33-vcs.el` | バージョン管理（magit, diff-hl, difftastic） |
| `34-misc.el` | ユーティリティ（which-key, undo-fu, vundo） |
| `99-private.el` | プライベート設定 |

#### site-elisp モジュール

| ファイル | 説明 |
|---|---|
| `my-gtags.el` | ggtags カスタム関数群（global 直接実行エンジン） |
| `my-markdown.el` | Markdown カスタム関数群（pandoc コマンド構築、CSS 設定） |

### 技術スタック

| カテゴリ | パッケージ |
|---|---|
| 補完 UI | Vertico + Consult + Marginalia + Orderless + Prescient |
| バッファ内補完 | Corfu + Cape |
| テーマ | doom-themes (doom-dracula) + doom-modeline |
| Git | Magit + diff-hl + difftastic |
| C/C++ | cc-mode, google-c-style, irony, ggtags |
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

---

## 5. 設定ファイルの命名規則

設定ファイル（`loads/inits/*.el`）の命名規則:

- **`NN-name.el`**: 2 桁の数字プレフィックスで読み込み順を制御（`00` が最初、`99` が最後）
- **環境プレフィックス**: プラットフォーム固有設定用
  - `linux-`, `windows-`, `nw-`（ターミナル）, `cocoa-emacs-`
  - 例: `linux-clipboard.el`
- **外部パッケージは 20〜34 番台に記述**

---

## 6. トラブルシューティング

- ターミナル利用時は `xterm-256color` を設定
- straight.el の不整合時: `M-x straight-rebuild-all` または `--clean-all` で再構築
- バッチモードでの起動検証:

``` sh
emacs --batch -l early-init.el -l init.el \
  --eval '(message "Init: %s" (emacs-init-time))'
```
