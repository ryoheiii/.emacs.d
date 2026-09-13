---
name: x-codex-review-impl
description: 未コミット変更とブランチ差分を Codex CLI でレビューする。実装レビューの依頼、または x-ship の final ゲートで使う。
allowed-tools: Bash, Read, Write, Grep, Glob
---

作業中のタスク worktree のルートで、未コミット変更とベースブランチとの差分を合わせて
レビューする。main / master 上では実行しない。
レビューだけの依頼では差分・指摘の報告で完了し、実装や index を変更しない。
修正ループは、修正までの依頼や x-ship など、既に実装の修正が承認されている場合に使う。

## モード

- `step`（既定）: dirty tree でも実行できる。承認記録は作らない。
- `final`: x-ship 用のブランチ全体のレビュー。
  この場合だけ [最終承認の契約](references/final-approval.md) を先に読み、clean tree と
  レビューした HEAD に結びつく承認記録を扱う。step はこのゲートの代わりにならない。

## レビュー材料

一意な `REVIEW_ID` を使い、以下を `/tmp/x-impl-<REVIEW_ID>/` に保存する。

- `git status --porcelain` と `git diff HEAD`（未コミット変更）。
- ベースは `origin/main` があればそれ、なければ `main`。
  `git diff --name-status "$BASE_BRANCH"...HEAD` と `git diff "$BASE_BRANCH"...HEAD`。
- ベースと HEAD の SHA、`git rev-list --left-right --count "$BASE_BRANCH"...HEAD` による
  ahead/behind、`git merge-base --is-ancestor "$BASE_BRANCH" HEAD` の結果。
  ベース未統合なら step の報告に明記し、final は統合・関連検証が済むまで承認しない。
- ユーザーの要求・制約と直近の検証結果。未収集ならその旨を明記する。

新規ファイルの内容は `git diff` に出ないため、対象の内容を別途渡す。
新規ファイルを含め両スコープとも空ならレビュー対象なしと報告する。

## 外部レビュー

プロンプトに上記の材料（内容または絶対パス）を渡し、要件漏れ・不具合・退行・検証不足・
両差分間の不整合を根拠付きでレビューさせる。指摘が属する範囲を
UNCOMMITTED / BRANCH / BOTH で示し、最終行を
`VERDICT: APPROVED` または `VERDICT: REVISE` と指定する。

対象に関係する規約を参照させる。Emacs 設定の場合は起動順、遅延ロード、use-package、
パスヘルパー、固定タグナビゲーション、tty の GUI 分離と端末向け代替、起動性能を含める。
tty に影響する場合は `make test-tty` と `make test-tty-live` の検証結果も確認させる。

```bash
bash .claude/scripts/codex-review.sh /tmp/x-impl-<REVIEW_ID>/prompt.txt /tmp/x-impl-<REVIEW_ID>/reply.md /tmp/x-impl-<REVIEW_ID>/session.txt
```

プレースホルダーは実際の値へ置換する。ラッパーの read-only 実行とモデル・effort の既定値を使う。
Claude Code は `run_in_background: true` で起動して完了通知を待ち、task-id と bash_id を照合する
（sleep ポーリングはしない）。Codex は通常のシェル実行でよい。
終了コード 0、新しい非空 reply、有効な session ID、指定形式の verdict を確認する。
失敗・不明な verdict は承認扱いにしない。usage limit・rate limit・認証失敗は自動リトライしない。

## 修正と再レビュー

レビューだけの依頼なら REVISE と指摘を報告し、ここから先の編集・コミットは行わない。

1. REVISE の指摘に対して実質的な修正を行う。
2. [検証規約](../../rules/verification.md) に従い修正範囲を検証し、結果を保存する。
   文書だけなら参照・整合性検査でよく、無関係な Emacs テストを要求しない。
3. final は最終承認の契約に従って修正をコミットし、clean tree に戻してから材料を取り直す。
4. 更新した材料と検証結果を渡し、同じラッパーの第 3 引数以降を `--resume <SESSION_ID>` とする。
   既出指摘を先に解決・処置し、新たな広域探索を始めないよう依頼する。
   新規指摘は具体的根拠のある High/Critical に限る。
   すべて解決し、新たな重大問題がなければ `VERDICT: APPROVED` を返させる。

初回を含め最大 3 ラウンド。未解決なら指摘を報告して停止する。
完了時は範囲・ベース、主要指摘と修正、検証結果、verdict、session ID、残るリスクを報告する。
final では承認記録のパスも示す。
