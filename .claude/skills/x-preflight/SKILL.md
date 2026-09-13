---
name: x-preflight
description: 実装前にタスク専用 worktree と検証環境を準備する。既存のタスク環境では不足項目だけを確認する。
allowed-tools: Bash, Read, Grep, Glob
---

実装を安全に開始できる環境を準備し、READY / BLOCKED と根拠を報告する。
[Git 運用](../../rules/git-workflow.md) に従い、既存のタスク環境は作り直さない。

## 作業場所

- `git status --porcelain`、`git worktree list`、`git branch -vv` で現在の作業場所と差分を確認する。
  自分の継続タスクの差分は保持して続行する。出所不明の差分や他セッションとの競合は
  上書き・stash・破棄せず BLOCKED として報告する。
- 新規タスクでは `git fetch origin` 後、`origin/main` から専用 worktree とブランチを作る。
  メインチェックアウトを切り替えず、他セッションのブランチを更新しない。
  例: `git worktree add .claude/worktrees/<name> -b docs/<name> origin/main`。
- 既に今回のタスク用 worktree にいる場合は、ブランチとベースを確認して再利用する。
  fetch 失敗やベース不明は報告し、未確認の状態を最新扱いしない。

## 変更に必要な環境とベースライン

- 文書・指示だけの変更では、参照先と検証に使うツールの確認で足りる。
- Emacs 設定の変更では `emacs --version`、`make --version` と straight の初期化状態を確認し、
  `make test-startup` で変更前のベースラインを取得する。
  worktree にパッケージがない場合は [docs/testing.md](../../../docs/testing.md) の
  `STRAIGHT_DIR` 指定を使う。共有キャッシュへの影響も同文書で確認する。
- セットアップやテスト基盤は [検証規約](../../rules/verification.md) で関連ターゲットを選ぶ。
  同じ状態で取得済みの結果を再取得する必要はない。
- 必要なツール・パッケージがない場合やベースラインが失敗した場合は、原因と影響範囲を報告する。
  全パッケージの復元・再構築を自動で始めない。

必要条件がそろえば READY。実装に必要な条件が満たせなければ BLOCKED とし、
不足情報と解消方法を示す。対象外の検査は省略理由を添える。
