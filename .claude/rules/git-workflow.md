---
description: ブランチ、worktree、PR、マージの安全な運用を定義する
globs: ["**/*"]
---

# Git ワークフロー

## 作業開始

1. `git status --porcelain` で作業ツリーがクリーンか確認する。
2. 未コミット変更があれば勝手に進めず、コミット、stash、破棄の希望をユーザーへ確認する。
   他セッションの作業である可能性があるため、勝手に整形・破棄しない。
3. `git worktree list` と `git branch -vv` で他セッションの作業状況を確認する。
4. デフォルトブランチを特定し、`git fetch origin` で最新化する。
5. タスク専用の worktree とブランチを作成する（「【必須】worktree 戦略」を参照）。
すでにタスク専用 worktree の作業ブランチ上なら、今回のタスク用であることを確認して続行する。

## ブランチ命名

- 接頭辞は `feat/`、`fix/`、`refactor/`、`chore/`、`docs/` のいずれかを使う。
- 説明部分は英小文字とハイフンで記述する。
- Issue／Ticket 番号がある場合は説明部分の先頭へ付ける。例: `feat/1234-auth`、`fix/login-timeout`、`docs/agent-config`

## 【必須】worktree 戦略

このリポジトリでは開発時に複数セッションを同時に走らせる。
そのため 1 タスク = 1 worktree = 1 ブランチを必須とする。例外は認めない。

- 作業開始時にタスク専用の worktree を作成し、その中だけで編集する。

  ```sh
  git worktree add <worktree パス> -b <ブランチ名> origin/main
  ```

- メインチェックアウト（リポジトリルート）で直接実装しない。他セッションが
  同じディレクトリを使用している可能性があるため、そのブランチを切り替えない。
- 自分の worktree 内でもブランチを切り替えない（マージ作業を除く）。
- 他セッションの worktree、ブランチ、未コミット変更へ触れない。
- 作業前後に `git worktree list`、`git branch -vv`、`git status --porcelain` で
  他セッションの作業状況を確認する。想定外のブランチや差分を見つけたら、
  上書きせず停止してユーザーへ報告する。
- 別タスクの依頼を受けたら、同じ worktree で続けず新しい worktree と
  セッションへ分離する。
- マージ後の worktree 削除は「マージ後の後片付け」に従う。

## コミットとマージ

- 変更を小さな論理単位でコミットする。
- マージ前に fixup／squash で履歴を論理単位へ整理するが、ユーザーの指示なく既存の公開履歴を改変しない。
- デフォルトブランチへは必ず `git merge --no-ff` でマージコミットを作る。
- fast-forward マージを行わない。
- マージコミットには `Merge branch '<ブランチ名>'` と変更概要を含める。
- マージは main を checkout しているチェックアウトで行う。main が他セッションで
  checkout 済みの場合、git は同一ブランチの二重 checkout を拒否する。この場合は
  相手のチェックアウトを操作せず、停止してユーザーへ確認する。
- 自分の worktree で main を checkout してマージした場合も、後片付けで worktree を
  削除し、メインチェックアウトの状態を変えない。

## マージ後の後片付け

マージが完了したら、同じ作業の中で必ず後片付けまで行う。後回しにしない。
`/x-ship` を使わず手動でマージした場合も同じ手順を適用する。

1. マージ済みローカルブランチを削除する。

   ```sh
   git branch -d <ブランチ名>
   ```

2. リモートへ push 済みのブランチは、対応するリモートブランチも削除する。

   ```sh
   git push origin --delete <ブランチ名>
   ```

3. タスク用 worktree を削除する。

   ```sh
   git worktree remove <worktree パス>
   git worktree prune
   ```

4. 残骸が無いことを確認する。

   ```sh
   git branch --merged main
   git worktree list
   ```

- `git branch -D` による強制削除を行わない。`-d` が拒否する場合は未マージであり、
  原因を調べてユーザーへ報告する。
- 未マージのブランチ、および今回のタスク以外のブランチは削除しない。
- 削除したブランチと worktree を完了報告へ記載する。

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
