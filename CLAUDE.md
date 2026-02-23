# CLAUDE.md

このファイルは、Claude Code (claude.ai/code) がこのリポジトリで作業する際のガイダンスを提供する。

# Part 1: 汎用ルール

## ワークフロー管理

### 1. 計画モードをデフォルトに
- 自明でないタスク（3ステップ以上、またはアーキテクチャ上の判断が必要）には必ず計画モードを使用する
- 問題が発生したら、すぐに停止して再計画する。無理に進めない
- 実装だけでなく、検証ステップにも計画モードを使用する
- 仕様を読む: @docs/specs/feature_x.md
- 曖昧さを減らすため、詳細な仕様を事前に作成する

### 2. サブエージェント戦略
- メインのコンテキストウィンドウをクリーンに保つため、サブエージェントを積極的に活用する
- 調査、探索、並列分析はサブエージェントにオフロードする
- 複雑な問題には、サブエージェントで計算リソースを投入する
- サブエージェント1つにつき1タスクを割り当て、集中実行する

### 3. 自己改善ループ
- ユーザーからの指摘があった場合は必ず `tasks/lessons.md` にパターンを記録する
- 同じミスを繰り返さないためのルールを自分用に作成する
- ミス発生率が下がるまで、これらの教訓を徹底的に反復改善する
- セッション開始時に、関連プロジェクトの教訓を確認する

### 4. 完了前に検証
- 動作を証明せずにタスクを完了とマークしない
- 必要に応じて、main ブランチと変更内容の動作差分を確認する
- 自問する：「シニアエンジニアはこれを承認するか？」
- テストを実行し、ログを確認し、正しさを実証する

### 5. エレガンスの追求（バランス重視）
- 自明でない変更の場合：一度立ち止まり「もっとエレガントな方法はないか？」と考える
- 修正が場当たり的に感じる場合：「今知っている全てを踏まえて、エレガントな解決策を実装する」
- 単純で明白な修正の場合はこのステップをスキップする。過剰設計を避ける
- 提示する前に、自分の作業に疑問を投げかける

### 6. 自律的なバグ修正
- バグ報告を受けたら、そのまま修正する。手取り足取り指示を求めない
- ログ、エラー、失敗したテストを指摘し、それらを解決する
- ユーザーにコンテキストスイッチを要求しない
- CI テストの失敗は、指示なしで修正する

## タスク管理

1. **まず計画**: `tasks/todo.md` にチェック可能な項目で計画を記述
2. **計画の検証**: 実装開始前に確認を取る
3. **進捗の追跡**: 完了したらその都度マークする
4. **変更の説明**: 各ステップで高レベルの要約を提供
5. **結果の文書化**: `tasks/todo.md` にレビューセクションを追加
6. **教訓の記録**: 指摘を受けた後は `tasks/lessons.md` を更新

## 基本原則

- **シンプル第一**: すべての変更を可能な限りシンプルに。コードへの影響を最小限に。
- **怠惰禁止**: 根本原因を見つける。一時的な修正はしない。シニア開発者の基準を守る。
- **最小限の影響**: 変更は必要な部分のみに留める。バグの混入を避ける。

## Git ワークフロー

### Git Worktree 戦略（Claude Code 使用時の推奨運用）

**原則：タスク専用の worktree で作業**

Claude Code で新しいタスク（feature/bugfix/refactor）に着手する際は、そのタスク専用の git worktree とブランチを作成し、その worktree 内で `claude` を起動する。

#### Worktree の作成

```bash
# 例: Issue #1234 の認証機能を実装する場合
git worktree add ../wt/1234-auth -b feat/1234-auth

cd ../wt/1234-auth
claude  # この worktree 専用の Claude セッションを起動
```

#### 命名規則

- **Worktree ディレクトリ名**: タスクが明確に分かる名前（例: `1234-auth`, `fix-login-timeout`, `refactor-utils`）
- **ブランチ名**: `feat/<short-desc>` / `fix/<short-desc>` / `refactor/<short-desc>` / `chore/<short-desc>` / `docs/<short-desc>`
  - `<short-desc>` は英小文字+ハイフン（例: `login-timeout`, `add-csv-export`）
  - Issue/Ticket がある場合は先頭に付与（例: `feat/1234-auth`, `fix/JIRA-123-null-guard`）

#### 禁止事項

- **Claude セッション中の同一 worktree でのブランチ切り替え禁止**
  - 文脈の混線を防ぐため、セッション中はブランチを固定する
  - 別ブランチで作業が必要な場合は、Claude セッションを終了し、別の worktree を作成するか新セッションで再開する

#### 作業完了後のクリーンアップ

```bash
# PR マージ後、worktree を削除
cd /path/to/main/repo
git worktree remove ../wt/1234-auth

# 不要な worktree を一括削除
git worktree prune
```

### ブランチ運用（必須）

- **main/master（=デフォルトブランチ）では作業しない。** すべての作業は新規ブランチを作ってから開始する。

#### 作業開始時（Claude Code が自律的に実行）

1. **作業ツリーがクリーンか確認** — `git status --porcelain`
   - 未コミット変更がある場合：**勝手に進めない**。ユーザーに「コミット / stash / 破棄」の希望を確認。
2. **デフォルトブランチを特定して最新化**
   - `git symbolic-ref --short refs/remotes/origin/HEAD` で取得（取得不可なら `main` → `master` の順で試行）
   - `git checkout <default-branch> && git pull --ff-only`
3. **新規ブランチを作成**
   - 命名規則: 上記「Git Worktree 戦略」のブランチ命名規則に従う
4. **既にデフォルトブランチ以外にいる場合**
   - そのブランチが今回の作業用と明確ならそのまま続行。不明なら新規ブランチの提案。

#### 作業中

- 変更は小さく、論理単位でコミットする。
- 生成/整形（fmt, lint, import 整理）とロジック変更は**別コミット**に分ける。

#### 作業終了時（PR 前）

- `git diff` / `git status` で意図しない差分がないか確認。
- テストを実行し、失敗があれば修正。
- `git push -u origin HEAD` でリモートに push。

#### 禁止事項

- `main/master` への直コミット禁止。
- `git push --force` 禁止（ユーザーの明示指示がある場合のみ検討）。
- ユーザーの指示なく履歴改変（rebase/squash の強制）しない。

### コミット規約

すべてのコミットメッセージは **Conventional Commits**（日本語）で記述する。

```
<type>(<scope>): <日本語の要約>

<本文（任意・日本語）>
```

| type | 用途 |
|---|---|
| `feat` | 新機能 |
| `fix` | バグ修正 |
| `docs` | ドキュメントのみの変更 |
| `style` | コードの意味に影響しない変更（空白、フォーマット等） |
| `refactor` | バグ修正でも機能追加でもないコード変更 |
| `test` | テストの追加・修正 |
| `chore` | ビルド、CI、依存関係などの雑務 |
| `ci` | CI 設定の変更 |

**注意**: `Co-Authored-By` トレーラーは付与しない。

### PR 作成時に含める情報

- 目的（Why）
- 変更内容（What）
- 影響範囲 / 互換性
- テスト結果（実行コマンドと結果）
- レビュー観点（特に見てほしい点）

## 変更ルール

- **変更前に提案**する：何を・なぜ・リスクを説明し、承認を待つ。
- 変更後は必ず **テストを実行**し、全パスを確認する。
- **秘密情報**（API キー、パスワード、資格情報）を出力・ログに含めない。

## Claude Code 設定

### 権限（`.claude/settings.local.json`）

許可されたコマンド：
- `.venv/Scripts/python.exe` — Python 実行
- `source` — 仮想環境の有効化

### コマンド

| コマンド | 用途 |
|---|---|
| `/review` | 現在の変更差分をレビュー |
| `/security-review` | セキュリティ観点でレビュー |

# Part 2: リポジトリ固有ルール

## リポジトリ概要

モジュール構成の Emacs 設定リポジトリ。コメントおよびドキュメントは日本語。主な用途は C/C++ 開発、ドキュメント作成（Markdown/Org）、日本語入力（Mozc/Migemo）対応の汎用編集。

## 主要コマンド

```sh
# Emacs ビルドに必要な依存パッケージをインストール
./emacs-setup.sh --setup

# インストール可能な Emacs バージョンを一覧表示
./emacs-setup.sh --list

# 指定バージョンの Emacs をビルド・インストール（GUI オプション: gtk3, lucid, pgtk, no）
./emacs-setup.sh --install <バージョン> [--gui <バックエンド>]

# 自動生成ファイルを削除（キャッシュ、履歴、バックアップ）※パッケージは保持
./emacs-setup.sh --clean

# パッケージを含むすべての自動生成ファイルを削除
./emacs-setup.sh --clean-all

# straight.el パッケージのアーカイブ／復元
./emacs-setup.sh --packing-package
./emacs-setup.sh --extract-package

# バッチモードで全パッケージをリビルド
emacs --batch --eval "(setq user-emacs-directory \"$HOME/.emacs.d\")" \
  -l early-init.el -l init.el -f straight-rebuild-all
```

## アーキテクチャ

### 起動シーケンス

1. **`early-init.el`** — Emacs UI 描画前に実行。OS 判定定数（`IS-MAC`, `IS-LINUX`, `IS-WINDOWS`）の定義、全ディレクトリパス変数（`my-emacs-dir`, `my-loads-dir`, `my-var-dir` 等）とヘルパー関数（`my-set-loads`, `my-set-custom`, `my-set-history` 等）の設定、`package.el` の無効化、`straight.el` のブートストラップ、バックアップ／自動保存パスの設定、不要な UI 要素の除去を行う。
2. **`init.el`** — 最小限のエントリーポイント。`loads/site-elisp/` を `load-path` に追加し、straight 経由で `use-package` をインストールし、`init-loader` を呼び出して `loads/inits/` 内の全ファイルを番号順に読み込む。
3. **`loads/inits/*.el`** — `init-loader` により番号／アルファベット順で読み込まれるモジュール型設定ファイル群。

### ディレクトリ構成

| パス | 用途 |
|---|---|
| `loads/inits/` | 番号付き設定モジュール（00=コア, 01=UI, 02=キーバインド, 10=関数, 18=組み込みパッケージ, 19=言語モード, 20=外部パッケージ, 98=バックアップ／非推奨, 99=プライベート） |
| `loads/site-elisp/` | ユーザー作成の Elisp ライブラリ（`load-path` に含まれる） |
| `loads/straight/` | straight.el のリポジトリとビルド成果物（パッケージマネージャデータ） |
| `custom/` | カスタムデータ: `custom.el`、YASnippet スニペット、Markdown 表示用 CSS/JS |
| `var/hist/` | 履歴ファイル、ブックマーク、TRAMP 永続化、transient |
| `var/backup/` | 自動保存・バックアップファイル |
| `var/package/` | eln-cache（ネイティブコンパイル） |

### 設定ファイルの命名規則

- **`NN-name.el`** — 2桁の数字プレフィックスで読み込み順を制御（00 が最初、99 が最後）
- **環境プレフィックス** — `linux-`, `windows-`, `nw-`（ターミナル）, `cocoa-emacs-` 等、プラットフォーム固有設定用
- **外部パッケージは 20 番台に記述**（`20-external-package.el`）

### パッケージ管理

- **straight.el** による再現性のある Git ベースのパッケージ管理（`package.el` は不使用）
- **use-package** と `straight-use-package-by-default t` の組み合わせ — `:straight t` は暗黙的に適用
- 全パッケージ宣言は `loads/inits/20-external-package.el` に集約（約96パッケージ）

### 主要技術スタック

- **補完**: Vertico + Consult + Marginalia + Orderless + Prescient（Helm/Ivy ではない）
- **バッファ内補完**: Corfu + Cape
- **テーマ**: doom-themes (doom-dracula) + doom-modeline
- **Git**: Magit + diff-hl
- **C/C++**: cc-mode, google-c-style, irony, ggtags
- **日本語入力**: Mozc, Migemo, TR-ime (Windows)
- **Undo**: vundo + undo-fu + undohist

### パスヘルパー関数

パスの構築には `early-init.el` で定義されたヘルパーを使用する。新しいパッケージのパス追加時はハードコーディングせず、以下を使うこと：

- `(my-set-loads "subdir/")` → `~/.emacs.d/loads/subdir/`
- `(my-set-custom "subdir/")` → `~/.emacs.d/custom/subdir/`
- `(my-set-history "filename")` → `~/.emacs.d/var/hist/filename`
- `(my-set-backup "filename")` → `~/.emacs.d/var/backup/filename`
- `(my-set-db "filename")` → `~/.emacs.d/var/database/filename`

### 設定編集時の規約

- 外部パッケージには `use-package` + `:straight t`（暗黙）、組み込みパッケージには `:straight nil` を使用
- 設定は `:custom`、フックは `:hook`、キーバインドは `:bind` にグループ化
- セクションヘッダーは `;;;;;` コメント形式: `;;;;; [Group] セクション名 ;;;;;`
- 自動生成ファイルはリポジトリルートに置かず、`my-var-dir` や `my-history-dir` 経由で配置する

### タグナビゲーションのキーバインド制約

以下の C/C++ タグナビゲーション用キーバインドは固定であり、変更してはならない。バックエンドの実装が変わっても、ユーザーの操作体験を維持すること。

| キーバインド | 機能 | 現在の実装 |
|---|---|---|
| `C-t d` / `C-t C-d` | 関数の定義場所の検索 (define) | `my/gtags-find-definition` |
| `C-t u` / `C-t C-u` | 関数の使用箇所の検索 (use) | `my/gtags-find-references` |
| `C-t v` / `C-t C-v` | 変数の使用箇所の検索 (variable) | `my/gtags-find-symbol` |
| `C-t f` / `C-t C-f` | ファイルの検索 (find) | `my/gtags-find-file` |
| `C-t p` / `C-t C-p` | 前の履歴へ移動 (previous) | `xref-go-back` |
| `C-t n` / `C-t C-n` | 次の履歴へ移動 (next) | `xref-go-forward` |

**注意事項:**
- バックエンドを将来変更する場合（例: ggtags → citre、eglot 等）でも、上記キーバインドは維持すること
- `C-t` プレフィックスは C/C++ モード専用のタグナビゲーション用に予約されている
- `update-gtags` 関数は手動タグ更新用に維持すること
