---
name: x-ship
description: 現在のブランチの変更を論理単位でコミットし、検証と Codex 最終レビューゲートを通してから main へ --no-ff でマージし、push と CI 確認、後片付けまで一気通貫で行う。
allowed-tools: Bash, Read, Grep, Glob, Edit, Write, Skill, ExitWorktree
disable-model-invocation: true
---

現在のブランチの変更をコミットし、検証 → 最終レビューゲート → `--no-ff` マージ →
push → CI 確認 → 後片付けまでを一気通貫で行う。

## Arguments

- `$ARGUMENTS`: コミットメッセージや変更概要の補足（任意）。
  `--no-push` を含む場合はローカルマージまでで終了する。

## 前提とルール

- `.claude/rules/git-workflow.md` / `commit-conventions.md` / `verification.md` に従う。
- main / master へ直接コミットしない。fast-forward マージをしない。force push をしない。
- fixup / squash などの履歴改変は **ユーザーの明示指示がある場合のみ** 行う。
- コミットメッセージは Conventional Commits の日本語形式。`Co-Authored-By` トレーラーを付けない。
- 各ステップは独立したシェルで実行されるため、変数はステップ内で解決する。

## Steps

### 1. 開始判定

```bash
BRANCH="$(git branch --show-current)"
case "$BRANCH" in main|master) echo "停止: main/master 上では実行しない"; exit 2;; esac
git status --porcelain
git rev-list main..HEAD --oneline
```

- main / master 上なら停止する（ブランチで作業すること）。
- dirty（未コミット変更あり）→ Step 2 へ。
- clean かつ main から ahead のコミットあり → Step 2 をスキップして Step 4 へ（resume）。
- clean かつ ahead なし → 「ship 対象がありません」と報告して停止する。

### 2. 論理単位コミット

- 変更を小さな論理単位に分けてコミットする。整形・自動生成とロジック変更を混在させない。
- `$ARGUMENTS` があればメッセージの参考にする。
- 検証中に生成された意図しないファイル（リポジトリルート直下の生成物など）を
  コミット対象に含めない（`git status --porcelain --ignored` で確認）。

### 3. 履歴確認

`git log --oneline main..HEAD` で履歴を確認する。整理（fixup/squash）が望ましい場合は
**提案のみ** 行い、ユーザーの明示指示があった場合に実施する。

### 4. 検証

`.claude/rules/verification.md` に従い、変更範囲に対応するテストを実行する:

- 起動設定のみ: `make test-startup`
- 表示・キーバインド・補完・クリップボード・GUI 分岐など tty へ影響する変更:
  `make test-tty` と `make test-tty-live` も実行（`emacs -nw` が主用途のため必須）
- それ以外の設定変更: `make test`
- `emacs-setup.sh` 変更: `make test-setup` も実行
- ドキュメントのみの変更: 記載パス・コマンドの実在確認で代替できる

失敗したら修正 → コミット → 再検証。**通るまで先へ進まない**。

### 5. 最終レビューゲート（fail-closed）

clean tree で、Codex final レビューの承認記録が現在の HEAD と一致することを確認する:

```bash
BRANCH_SAFE="$(git branch --show-current | tr '/' '-')"
RECORD=".claude/review-state/final-approval-${BRANCH_SAFE}"
if [ -f "$RECORD" ] && [ "$(cat "$RECORD")" = "$(git rev-parse HEAD)" ]; then
  echo "final review gate: APPROVED"
else
  echo "final review gate: MISSING_OR_STALE"
fi
```

- `MISSING_OR_STALE` の場合は `/x-codex-review-impl final` を実行して APPROVED を得てから
  このステップを再実行する。**APPROVED なしでマージへ進まない**。
- ドキュメントのみの軽微な変更でゲートを省略したい場合は、ユーザーの明示同意を得て
  省略した旨を最終報告に残す。

### 6. main へ --no-ff マージ

メインチェックアウト（worktree 作業時は `git rev-parse --path-format=absolute --git-common-dir`
の親ディレクトリ）で実行する:

```bash
MAIN_ROOT="$(dirname "$(git rev-parse --path-format=absolute --git-common-dir)")"
git -C "$MAIN_ROOT" pull --ff-only origin main
git -C "$MAIN_ROOT" merge --no-ff <ブランチ名>
```

- マージメッセージは `Merge branch '<ブランチ名>'` に変更概要を添える。
- `pull --ff-only` で main が進んだ場合は、マージせず停止してユーザーに確認する
  （ブランチへの main 取り込みが必要。rebase は明示指示がない限り提案しない）。
- コンフリクトが出たら自動解決を試みず、内容を報告して指示を仰ぐ。

### 7. push と CI 確認（`--no-push` 指定時はスキップ）

```bash
git -C "$MAIN_ROOT" push origin main
gh run list --branch main --limit 1
```

push 起動の CI（Emacs 回帰テスト）を `gh run watch <run-id> --exit-status` で確認する
（Claude Code では `run_in_background: true` で待つ）。失敗した場合はログを取得して
原因を報告し、修正方針の指示を仰ぐ（勝手に revert / force push しない）。

### 8. 後片付け

- マージ済みブランチを削除する: `git -C "$MAIN_ROOT" branch -d <ブランチ名>`
- タスク用 worktree を削除する: `git worktree remove <worktree パス>` → `git worktree prune`
  - セッションが対象 worktree 内にいる場合は、先に ExitWorktree（remove）で抜ける。
    ExitWorktree がブランチも削除した場合、ブランチ削除はスキップする。
- 承認記録 `.claude/review-state/final-approval-*` のうち当該ブランチ分を削除する。

### 9. 報告

- マージコミットの SHA とメッセージ
- 実行した検証コマンドと結果
- 最終レビューゲートの結果（省略時はその旨と理由）
- push / CI の結果（スキップ時はその旨）
- 削除したブランチ・worktree
- 残作業・未検証事項
