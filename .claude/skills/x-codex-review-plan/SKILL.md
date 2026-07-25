---
name: x-codex-review-plan
description: 現在の実装計画を Codex CLI で外部レビューし、REVISE なら計画を修正して同一セッションで再レビューする。修正が発生した場合のみ新規セッションで最終監査を行う。
allowed-tools: Bash, Read, Write
---

現在の会話にある具体的な実装計画を、Codex CLI を厳格な外部レビュアーとして使ってレビューする。

## 前提条件

- レビュー可能な具体的計画（実装ステップ・検証手順を含む）が会話または計画ファイルに存在すること。
  無い場合は停止し「先にレビュー可能な具体的 plan を作成してください」と伝える。
- リポジトリルートで実行する。

## 実行環境による差異

- **Claude Code**: 前景 Bash には 10 分の上限があるため、`codex-review.sh` は
  `run_in_background: true` で起動し、完了通知を待つ。`sleep` による能動ポーリングは禁止。
  完了通知の task-id が起動時の bash_id と一致することを確認してから、
  exit code と reply ファイル非空の **両方** を検証する。
- **Codex**: 通常のシェル実行でよい（背景実行の制約はない）。

## ルール

- Codex 呼び出しは必ず `.claude/scripts/codex-review.sh` を使う
  （モデル gpt-5.6-sol / effort xhigh / sandbox read-only を既定で固定）。
- レビューは最大 3 ラウンド。
- 再レビュー前に必ず計画へ実質的な修正を加える。
- 再レビューは closure-first: 既出指摘の解決・処置の確認を先に行い、
  同一セッション内で新規の広域探索を再開しない。
- REVISE が 1 回以上発生した場合のみ、新規セッションで最終監査を 1 回行う。
  初回 APPROVED の場合は監査をスキップする（anchoring リスクがないため）。
- このスキルから x-deep-plan を呼び出さない（再入禁止）。
- usage limit / rate limit / 認証エラー時は自動リトライせず、原因を報告して停止する。

## 手順

### Step 1: 状態ファイルの準備

1. `REVIEW_ID` を生成する（Bash）: `date +%Y%m%d-%H%M%S` の値に `-$$` を付けた形式。
2. 計画全文を Write ツールで `/tmp/x-plan-<REVIEW_ID>.md` に書き出す。
3. 計画ヘッダに `<!-- plan-file: <path> -->` メタデータがあり、パスが
   `~/.claude/plans/` 配下または `/tmp` 配下の `.md` なら `ORIGINAL_PLAN_PATH` として記録する。
   無ければ空のまま続行する。

### Step 2: 初回レビュー

**Step 2a** — レビュープロンプトを Write ツールで `/tmp/x-plan-prompt-<REVIEW_ID>.txt` に書く:

```
You are reviewing an implementation plan for an Emacs configuration repository
(~/.emacs.d, module-based init with early-init.el / init.el / loads/inits/).

Review the plan in:
/tmp/x-plan-<REVIEW_ID>.md

Focus on:
- missing steps
- risky assumptions
- validation/testing gaps (make test / make test-startup / make test-tty /
  make test-tty-live coverage)
- tty regressions: the primary usage is a terminal (`emacs -nw`); the plan must keep
  GUI-only code guarded, preserve terminal alternatives, and verify tty explicitly
- sequencing problems (init-loader load order, module numbering)
- violations of repo conventions (use-package sections, :straight nil for
  built-ins, path helpers instead of hardcoded paths, fixed C-t tag-navigation
  keybindings must not change)
- startup-performance regressions
- hidden edge cases that may break implementation
- systemic issues or inconsistencies across the whole plan
- anything only visible when reading the plan end-to-end

Be concrete and actionable.
End with exactly one line:
VERDICT: APPROVED
or
VERDICT: REVISE
```

**Step 2b** — 実行（Claude Code では `run_in_background: true`）:

```bash
bash .claude/scripts/codex-review.sh /tmp/x-plan-prompt-<REVIEW_ID>.txt /tmp/x-plan-reply-<REVIEW_ID>.md /tmp/x-plan-session-<REVIEW_ID>.txt
```

完了後、exit code を確認して reply と session id を読む。非ゼロなら
（1=codex 失敗 / 2=reply 空 / 3=session id 抽出失敗 / 124=タイムアウト）を報告して停止する。

### Step 3: VERDICT: REVISE の場合

1. 指摘に基づき計画を自分で修正する。
2. `/tmp/x-plan-<REVIEW_ID>.md` を修正後の全文で上書きする
   （ヘッダの `<!-- ... -->` メタデータ行は保持する）。
3. 再レビュープロンプトを Write で書き直す:

```
Re-review the updated implementation plan in:
/tmp/x-plan-<REVIEW_ID>.md

Check whether the previously raised issues are actually fixed.
Close or explicitly disposition every prior finding before looking for anything new.
Do not re-report an unchanged finding that the current plan resolves, and do not
restart broad discovery inside this corrective session.
A net-new finding is allowed only when it is High/Critical and concretely anchored.
If all prior findings are closed and no qualifying novel blocker exists, return
VERDICT: APPROVED immediately.

End with exactly one line:
VERDICT: APPROVED
or
VERDICT: REVISE
```

4. セッション ID を読み、`--resume` で再実行する:

```bash
bash .claude/scripts/codex-review.sh /tmp/x-plan-prompt-<REVIEW_ID>.txt /tmp/x-plan-reply-<REVIEW_ID>.md --resume <SESSION_ID>
```

計 3 ラウンドまで繰り返す。3 ラウンドで APPROVED に達しない場合は、
未解決の指摘を列挙してユーザーの判断を仰ぐ。

### Step 4: 最終監査（REVISE が発生した場合のみ）

新規セッション（`--resume` を使わない）で監査プロンプトを実行する:

```
You are performing a FINAL AUDIT of an implementation plan.

Review the plan in:
/tmp/x-plan-<REVIEW_ID>.md

Do NOT repeat minor feedback from incremental review.
Check for:
1. systemic issues missed by iterative review
2. consistency across the whole plan
3. naming / state / error-handling drift
4. anything only visible when reading the plan as a whole

End with exactly one line:
AUDIT: PASS
or
AUDIT: CONCERNS
```

### Step 5: 最終報告

1. APPROVED（かつ AUDIT: PASS または監査スキップ）で `ORIGINAL_PLAN_PATH` が非空なら、
   `/tmp/x-plan-<REVIEW_ID>.md` を Read し、ORIGINAL_PLAN_PATH へ Write で書き戻す。
2. 次を報告する:
   - レビューした計画の要約
   - 主要指摘と実際に加えた修正
   - 最終 verdict と監査結果（スキップ時はその旨）
   - final plan artifact path: `/tmp/x-plan-<REVIEW_ID>.md`
   - Codex セッション ID
   - 残る懸念
