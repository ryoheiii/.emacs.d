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

タスク worktree から実行する。メインチェックアウトへの操作は `git -C` で行い、
次の独立シェルへ渡す状態はタスク専用の Git 管理ディレクトリへ保存する。
`--no-push` を指定された場合は、このシェルに `SHIP_PUSH=no` を渡す（既定は `yes`）:

```bash
set -eu
SHIP_PUSH="${SHIP_PUSH:-yes}"
case "$SHIP_PUSH" in yes|no) ;; *) exit 2;; esac
MAIN_ROOT="$(dirname "$(git rev-parse --path-format=absolute --git-common-dir)")"
TASK_BRANCH="$(git branch --show-current)"
TASK_WORKTREE="$(git rev-parse --show-toplevel)"
case "$TASK_BRANCH" in main|master|'') echo "停止: タスク worktree が必要です" >&2; exit 2;; esac
SHIP_STATE="$(git rev-parse --absolute-git-dir)/ship-state"
test "$(git -C "$MAIN_ROOT" branch --show-current)" = main
test -z "$(git -C "$MAIN_ROOT" status --porcelain)"
git -C "$MAIN_ROOT" pull --ff-only origin main
# このステップより前に main が更新されていた場合も検出する。
git merge-base --is-ancestor "$(git -C "$MAIN_ROOT" rev-parse HEAD)" "$TASK_BRANCH"
git -C "$MAIN_ROOT" merge --no-ff "$TASK_BRANCH"
MERGE_SHA="$(git -C "$MAIN_ROOT" rev-parse HEAD)"
umask 077
printf 'MAIN_ROOT=%q\nTASK_BRANCH=%q\nTASK_WORKTREE=%q\nMERGE_SHA=%q\nSHIP_PUSH=%q\n' \
  "$MAIN_ROOT" "$TASK_BRANCH" "$TASK_WORKTREE" "$MERGE_SHA" "$SHIP_PUSH" > "$SHIP_STATE.tmp"
mv -f "$SHIP_STATE.tmp" "$SHIP_STATE"
```

- マージメッセージは `Merge branch '<ブランチ名>'` に変更概要を添える。
- 更新後の main がタスクブランチに含まれていなければマージを中止する。
  main を取り込んでテスト・最終レビューをやり直してから再開する。
  rebase は明示指示がない限り行わない。
- コンフリクトが出たら自動解決を試みず、内容を報告して指示を仰ぐ。

### 7. push と CI 確認（`--no-push` 指定時はスキップ）

```bash
set -eu
SHIP_STATE="$(git rev-parse --absolute-git-dir)/ship-state"
test -r "$SHIP_STATE"
# Step 6 が %q で保存した、このタスク専用の状態だけを読み込む。
source "$SHIP_STATE"
test "$(git branch --show-current)" = "$TASK_BRANCH"
test "$(git rev-parse --show-toplevel)" = "$TASK_WORKTREE"
test "$(git -C "$MAIN_ROOT" rev-parse HEAD)" = "$MERGE_SHA"
if [ "$SHIP_PUSH" = no ]; then exit 0; fi
git -C "$MAIN_ROOT" push origin main
RUN_ID=""
for attempt in $(seq 1 24); do
  RUN_ID="$(gh run list --branch main --commit "$MERGE_SHA" --workflow test.yml --event push \
    --json databaseId --jq '.[0].databaseId // empty')" || exit 1
  if [ -n "$RUN_ID" ]; then break; fi
  sleep 5
done
test -n "$RUN_ID"
gh run watch "$RUN_ID" --exit-status
```

push 起動の CI（Emacs 回帰テスト）を `gh run watch <run-id> --exit-status` で確認する
（Claude Code では `run_in_background: true` で待つ）。失敗した場合はログを取得して
原因を報告し、修正方針の指示を仰ぐ（勝手に revert / force push しない）。

### 8. 後片付け

`.claude/rules/git-workflow.md`「マージ後の後片付け」に従い、必ず実施する。

タスク worktree から次を実行する。状態は worktree 削除前に読み込み、
削除後も同じシェル内でブランチの後片付けまで完了する。

```bash
set -eu
SHIP_STATE="$(git rev-parse --absolute-git-dir)/ship-state"
test -r "$SHIP_STATE"
source "$SHIP_STATE"
test "$(git branch --show-current)" = "$TASK_BRANCH"
test "$(git rev-parse --show-toplevel)" = "$TASK_WORKTREE"
git merge-base --is-ancestor "$TASK_BRANCH" main
# GitHub が自動削除したブランチの古い追跡 ref を取り除く。
# 通信失敗時は、再実行に必要な worktree と状態を残して停止する。
if [ "$SHIP_PUSH" = yes ]; then
  git fetch --prune origin
fi
cd "$MAIN_ROOT"
git worktree remove "$TASK_WORKTREE"
git worktree prune
# -d は upstream を優先する。main への取り込み確認済みの対象だけ解除する。
if [ -n "$(git for-each-ref --format='%(upstream)' "refs/heads/$TASK_BRANCH")" ]; then
  git branch --unset-upstream "$TASK_BRANCH"
fi
git branch -d "$TASK_BRANCH"
if [ "$SHIP_PUSH" = yes ] && git show-ref --verify --quiet "refs/remotes/origin/$TASK_BRANCH"; then
  git push origin --delete "$TASK_BRANCH"
fi
```

- `--no-push` 時は Step 7 を省略し、このステップではリモート削除も省略する。
- ExitWorktree（remove）を使う場合も、先に状態を読み込む。退出ツールが当該ブランチを削除済みなら `branch -d` は省略する。
- `-D` は使わない。削除が拒否されたら原因を確認して報告する。
- 承認記録 `.claude/review-state/final-approval-*` のうち当該ブランチ分を削除する。

### 9. 報告

- マージコミットの SHA とメッセージ
- 実行した検証コマンドと結果
- 最終レビューゲートの結果（省略時はその旨と理由）
- push / CI の結果（スキップ時はその旨）
- 削除したブランチ・worktree
- 残作業・未検証事項
