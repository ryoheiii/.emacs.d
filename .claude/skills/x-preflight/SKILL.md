---
name: x-preflight
description: 実装開始前の環境チェック。既存差分の保護、main の最新化、タスク専用ブランチ・worktree の作成、ツールチェーン確認、テストベースラインの取得を行い、READY / BLOCKED を報告する。
allowed-tools: Bash, Read, Grep, Glob
---

実装を始める前に、作業環境が既定の状態にあることを確認する。
チェックは上から順に実行し、失敗した項目があっても可能な範囲で残りを実行して
最後にまとめて報告する。

## Steps

### 1. 既存差分の保護

```bash
git status --porcelain
```

非空なら **BLOCKED**。ユーザーの未コミット変更を上書き・stash・破棄せず、
コミット / stash / 破棄の希望を確認する。

### 2. main の最新化

メインチェックアウトで `git fetch origin` → `git pull --ff-only origin main`。
fast-forward できない場合は BLOCKED として理由を報告する（履歴改変はしない）。

### 3. タスク専用ブランチ・worktree の作成

`.claude/rules/git-workflow.md` に従う:

- ブランチ名は `feat/`・`fix/`・`refactor/`・`chore/`・`docs/` + 英小文字とハイフン。
- 原則 1 タスク = 1 worktree + 1 ブランチ。
  例: `git worktree add .claude/worktrees/<name> -b <prefix>/<name> main`
- タスク内容が未定でブランチ名を決められない場合は、この手順を保留として報告する。

### 4. ツールチェーン確認

```bash
emacs --version | head -1
make --version | head -1
test -d loads/straight/repos && echo "straight: ok" || echo "straight: 未初期化"
```

`straight: 未初期化` の場合は初回起動またはパッケージ復元
（`./emacs-setup.sh --extract-package`）が必要な旨を報告する。

### 5. テストベースライン

作成した worktree（または現在のチェックアウト）で `make test-startup` を実行し、
変更前のベースラインが green であることを確認する。
失敗する場合は **変更前から壊れている** ことを意味するため、BLOCKED として
エラーログの要点を報告する（このスキルでは修正しない）。

### 6. 報告

各項目を表で報告する（項目 / 結果 / 備考）。全項目 OK なら **READY**、
1 つでも問題があれば **BLOCKED** とし、解消に必要なアクションを列挙する。
