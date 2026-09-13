# final モードの承認記録

final は clean tree が必須である。`git status --porcelain` が非空なら、レビューを開始せず
「final レビューは clean tree 必須」と報告する。step の APPROVED から記録を作らない。

レビュー開始時にブランチ名、HEAD、ベースの SHA を記録し、ベースが HEAD の祖先であることを確認する。
ベース未統合なら承認せず、統合後の関連検証が必要と報告する。
修正まで承認された依頼で REVISE を直した後は、その修正だけを
論理単位でコミットし、必要な検証と clean tree の確認後、新しい HEAD を記録して再レビューする。
無関係な差分は取り込まず、想定外の HEAD・ブランチ・差分の変更は停止して報告する。

APPROVED 後、同じブランチ・同じ HEAD・clean tree であることを再確認して初めて
x-ship が照合する記録を書く。次は Bash で実行し、`REVIEWED_BRANCH`・`REVIEWED_HEAD`・`REVIEWED_BASE` には
レビュー前に記録した実際の値を渡す（承認後の現在値で代用しない）。

```bash
set -eu
CURRENT_BRANCH="$(git branch --show-current)"
CURRENT_HEAD="$(git rev-parse HEAD)"
CURRENT_STATUS="$(git status --porcelain)"
test "$CURRENT_BRANCH" = "$REVIEWED_BRANCH"
test "$CURRENT_HEAD" = "$REVIEWED_HEAD"
test -z "$CURRENT_STATUS"
git merge-base --is-ancestor "$REVIEWED_BASE" "$REVIEWED_HEAD"
mkdir -p .claude/review-state
BRANCH_SAFE="$(printf '%s' "$CURRENT_BRANCH" | tr '/' '-')"
printf '%s\n' "$REVIEWED_HEAD" > ".claude/review-state/final-approval-${BRANCH_SAFE}"
```

記録後に HEAD が変われば記録は stale であり、final レビューをやり直す。
既存の記録があることだけを理由に承認扱いにしない。
