---
description: ブランチ、worktree、PR、マージの安全な運用を定義する
globs: ["**/*"]
---

# Git ワークフロー

## 作業開始

1. `git status --porcelain` で作業ツリーがクリーンか確認する。
2. 未コミット変更があれば勝手に進めず、コミット、stash、破棄の希望をユーザーへ確認する。
3. デフォルトブランチを特定し、`git pull --ff-only` で最新化する。
4. タスク専用の新規ブランチを作成する。
すでに作業ブランチ上なら、今回のタスク用であることを確認して続行する。

## ブランチ命名

- 接頭辞は `feat/`、`fix/`、`refactor/`、`chore/`、`docs/` のいずれかを使う。
- 説明部分は英小文字とハイフンで記述する。
- Issue／Ticket 番号がある場合は説明部分の先頭へ付ける。例: `feat/1234-auth`、`fix/login-timeout`、`docs/agent-config`

## worktree 戦略

- 原則として 1 タスク = 1 worktree + 1 ブランチとする。
- タスク専用 worktree 内でエージェントのセッションを開始する。
- セッション中に同じ worktree のブランチを切り替えない。
- 別ブランチの作業は、別 worktree と別セッションへ分離する。
- PR マージ後は `git worktree remove <worktree>` で削除し、`git worktree prune` で管理情報を掃除する。

## コミットとマージ

- 変更を小さな論理単位でコミットする。
- マージ前に fixup／squash で履歴を論理単位へ整理するが、ユーザーの指示なく既存の公開履歴を改変しない。
- デフォルトブランチへは必ず `git merge --no-ff` でマージコミットを作る。
- fast-forward マージを行わない。
- マージコミットには `Merge branch '<ブランチ名>'` と変更概要を含める。

## PR の記載事項

- 目的（Why）
- 変更内容（What）
- 影響範囲／互換性
- テスト結果（実行コマンドと結果）
- レビュー観点（特に確認してほしい点）

## 禁止事項

- `main`／`master` への直コミット
- `git push --force`
- ユーザーの明示指示なき rebase、squash、その他の履歴改変
