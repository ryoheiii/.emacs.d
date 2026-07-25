---
name: x-codex-review-impl
description: 未コミット変更とブランチ差分の両方を Codex CLI でレビューし、REVISE なら修正して同一セッションで再レビューする。final モードは clean tree 必須で、APPROVED 時に x-ship 用の承認記録を残す。
allowed-tools: Bash, Read, Write, Grep, Glob
---

現在の実装（未コミット変更 + ベースブランチとの差分）を、Codex CLI を厳格な
外部レビュアーとして使ってレビューする。

## モード

`$ARGUMENTS` が `final` の場合は final モード、それ以外は step モード（既定）。

- **step**: 論理単位ごとの incremental レビュー。dirty tree でも実行できる。
  承認記録は書かない（`/x-ship` のゲート代替にはならない）。
- **final**: `/x-ship` のマージ前ゲート用の whole-branch レビュー。
  **clean worktree 必須**（`git status --porcelain` が非空なら
  「final レビューは clean tree 必須」と表示して停止する）。
  最終 verdict が APPROVED のときだけ Step 4 で承認記録を残す。

## 実行環境による差異

- **Claude Code**: `codex-review.sh` は `run_in_background: true` で起動し、完了通知を待つ
  （`sleep` ポーリング禁止。task-id と bash_id の一致確認 → exit code + reply 非空の両方を検証）。
- **Codex**: 通常のシェル実行でよい。

## ルール

- Codex 呼び出しは必ず `.claude/scripts/codex-review.sh` を使う。リポジトリルートで実行する。
- レビューは最大 3 ラウンド。再レビュー前に必ず実質的な修正を加える。
- 再レビューは closure-first: 既出指摘の解決を先に確認し、新規の広域探索を再開しない。
- 修正後の再レビュー前には、修正範囲に対応するテストを実行して結果をプロンプトに含める。
- usage limit / rate limit / 認証エラー時は自動リトライせず、原因を報告して停止する。

## 手順

### Step 1: レビュー材料の収集

**Step 1a** — `REVIEW_ID` を生成する: `date +%Y%m%d-%H%M%S` の値に `-$$` を付けた形式。

**Step 1b** — final モードの場合のみ、clean tree を確認する
（`git status --porcelain` が非空なら停止）。

**Step 1c** — 差分を収集する（Bash、リポジトリルートで実行）:

```bash
if git rev-parse --verify origin/main >/dev/null 2>&1; then BASE_BRANCH=origin/main; else BASE_BRANCH=main; fi
git status --porcelain > /tmp/x-impl-uncommitted-changed-<REVIEW_ID>.txt
git diff HEAD > /tmp/x-impl-uncommitted-diff-<REVIEW_ID>.patch
git diff --name-status "$BASE_BRANCH"...HEAD > /tmp/x-impl-branch-changed-<REVIEW_ID>.txt
git diff "$BASE_BRANCH"...HEAD > /tmp/x-impl-branch-diff-<REVIEW_ID>.patch
echo "BASE_BRANCH=$BASE_BRANCH"
```

- 新規（untracked）ファイルは `git diff` に内容が出ないため、レビュー対象に含めるなら
  先に `git add -N <file>` してから Step 1c を実行する。
- 差分が両スコープとも空なら「レビュー対象がありません」と報告して停止する。
- 現在のブランチが main / master の場合は停止する（ブランチで作業すること）。

### Step 2: 初回レビュー

**Step 2a** — 収集した 4 ファイルを Read し、プロンプトを Write ツールで
`/tmp/x-impl-prompt-<REVIEW_ID>.txt` に書く。プレースホルダーは実内容で置換する:

```
You are reviewing a completed implementation in an Emacs configuration
repository (~/.emacs.d, module-based init: early-init.el / init.el /
loads/inits/NN-name.el loaded in numeric order by init-loader).

Review BOTH scopes together:

A. Uncommitted local changes
Changed files:
<UNCOMMITTED_CHANGED>

Diff:
<UNCOMMITTED_DIFF>

B. Committed branch changes relative to the base branch: <BASE_BRANCH>
Changed files:
<BRANCH_CHANGED>

Diff:
<BRANCH_DIFF>

Test output:
<TEST_OUTPUT>

Focus on:
- correctness bugs (elisp evaluation order, autoload/deferred-loading mistakes)
- missing edge cases
- incomplete requirement coverage
- regressions introduced by the change (startup errors, keybinding conflicts)
- repo convention violations: use-package sections (:custom/:hook/:bind),
  :straight nil for built-ins, path helpers instead of hardcoded paths,
  module numbering responsibilities, Japanese comments
- invariant violations: the fixed C-t tag-navigation keybindings must not change
- tty regressions: the primary usage is a terminal (`emacs -nw`); GUI-only code must
  stay guarded (`:if (display-graphic-p)`), terminal alternatives (corfu-terminal,
  xclip) must keep working, and no GUI-only assumption may leak into shared config
- startup-performance impact
- test gaps (make test / make test-startup / make test-tty / make test-tty-live coverage)
- conflicts between uncommitted changes and committed branch changes

Be concrete and actionable.
When a finding applies only to one scope, say whether it belongs to
UNCOMMITTED / BRANCH / BOTH.

End with exactly one line:
VERDICT: APPROVED
or
VERDICT: REVISE
```

初回の `<TEST_OUTPUT>` は、直近で実行したテスト結果があればそれを、
無ければ `not collected in initial review` を埋める。

**Step 2b** — 実行（Claude Code では `run_in_background: true`）:

```bash
bash .claude/scripts/codex-review.sh /tmp/x-impl-prompt-<REVIEW_ID>.txt /tmp/x-impl-reply-<REVIEW_ID>.md /tmp/x-impl-session-<REVIEW_ID>.txt
```

完了後、exit code を確認して reply と session id を読む。非ゼロなら
（1=codex 失敗 / 2=reply 空 / 3=session id 抽出失敗 / 124=タイムアウト）を報告して停止する。

### Step 3: VERDICT: REVISE の場合

1. 指摘に基づき実装を自分で修正する。
2. 修正範囲に対応するテストを実行し、結果を `/tmp/x-impl-tests-<REVIEW_ID>.txt` に保存する。
   `.claude/rules/verification.md` に従い選択する（最小: `make test-startup`、
   キーバインド変更: `make test-keybinding`、C/C++ 設定: `make test-cpp-config`、
   tty へ影響する変更: `make test-tty` と `make test-tty-live`、
   広範な変更: `make test`）。
3. Step 1c を再実行して差分を取り直す。
4. 再レビュープロンプトを書く。ヘッダを `Re-review the updated implementation.` に変え、
   Focus セクションを次に置き換え、`<TEST_OUTPUT>` に手順 2 の結果を埋める:

```
Corrective rerun guidance:
- Close or explicitly disposition every previously raised finding before looking for anything new.
- Do not re-report an unchanged finding that the current implementation resolves.
- Do not restart broad discovery inside this resumed corrective session.
- Add a net-new finding only when it is High/Critical and concretely anchored.
- If every prior finding is closed and no qualifying novel blocker exists, return VERDICT: APPROVED immediately.
```

5. `--resume <SESSION_ID>` で再実行する:

```bash
bash .claude/scripts/codex-review.sh /tmp/x-impl-prompt-<REVIEW_ID>.txt /tmp/x-impl-reply-<REVIEW_ID>.md --resume <SESSION_ID>
```

計 3 ラウンドまで繰り返す。3 ラウンドで APPROVED に達しない場合は、
未解決の指摘を列挙してユーザーの判断を仰ぐ。

### Step 4: 承認記録（final モードで APPROVED のときのみ）

clean tree のまま APPROVED に達した場合のみ、`/x-ship` のマージ前ゲートが照合する
承認記録を書く（Bash、リポジトリルートで実行）:

```bash
mkdir -p .claude/review-state
BRANCH_SAFE="$(git branch --show-current | tr '/' '-')"
git rev-parse HEAD > ".claude/review-state/final-approval-${BRANCH_SAFE}"
echo "recorded: .claude/review-state/final-approval-${BRANCH_SAFE}"
```

記録後に HEAD が変わった場合（追加修正・履歴整理）は記録が stale になるため、
final レビューをやり直す必要がある。

### Step 5: 最終報告

次を報告する:

- レビュー範囲の要約（未コミット変更とブランチ差分の両方）と検出したベースブランチ
- 主要指摘と実際に加えた修正
- 実行したテストと結果
- 最終 verdict（final モードでは承認記録のパスも）
- Codex セッション ID
- 残るリスク
