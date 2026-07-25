#!/usr/bin/env bash
# Codex CLI を外部レビュアーとして 1 ターン実行する共通ラッパー。
# x-codex-review-plan / x-codex-review-impl から呼び出す。
#
# 使い方:
#   codex-review.sh PROMPT_FILE REPLY_FILE SESSION_FILE        # 新規レビュー
#   codex-review.sh PROMPT_FILE REPLY_FILE --resume SESSION_ID # 同一セッションで再レビュー
#
# 注意:
#   - リポジトリルートで実行する（resume は -C を受け付けないため cwd が対象になる）。
#   - 設定は環境変数で上書きできる。
#     CODEX_REVIEW_MODEL   (default: gpt-5.6-sol)
#     CODEX_REVIEW_EFFORT  (default: xhigh)
#     CODEX_REVIEW_TIMEOUT_SEC (default: 1800)
#
# 終了コード:
#   0   = 成功（reply 非空。新規時は session id も取得済み）
#   1   = 引数不正または codex exec 失敗
#   2   = reply が空
#   3   = session id を抽出できない（新規時のみ）
#   124 = タイムアウト

set -u

MODEL="${CODEX_REVIEW_MODEL:-gpt-5.6-sol}"
EFFORT="${CODEX_REVIEW_EFFORT:-xhigh}"
TIMEOUT_SEC="${CODEX_REVIEW_TIMEOUT_SEC:-1800}"

PROMPT_FILE="${1:-}"
REPLY_FILE="${2:-}"
MODE_ARG="${3:-}"

usage() {
  echo "Usage: codex-review.sh PROMPT_FILE REPLY_FILE {SESSION_FILE | --resume SESSION_ID}" >&2
}

if [ -z "$PROMPT_FILE" ] || [ -z "$REPLY_FILE" ] || [ -z "$MODE_ARG" ]; then
  usage
  exit 1
fi
if [ ! -s "$PROMPT_FILE" ]; then
  echo "ERROR: prompt file が空か存在しません: $PROMPT_FILE" >&2
  exit 1
fi

LOG_FILE="${REPLY_FILE}.log"

if [ "$MODE_ARG" = "--resume" ]; then
  SESSION_ID="${4:-}"
  if [ -z "$SESSION_ID" ]; then
    usage
    exit 1
  fi
  # resume は -C / -s を受け付けないため、cwd 実行 + config 形式で sandbox を渡す
  timeout "$TIMEOUT_SEC" codex exec resume "$SESSION_ID" \
    -m "$MODEL" \
    -c "model_reasoning_effort=\"$EFFORT\"" \
    -c 'sandbox_mode="read-only"' \
    -o "$REPLY_FILE" - < "$PROMPT_FILE" > "$LOG_FILE" 2>&1
  CODEX_EXIT=$?
else
  SESSION_FILE="$MODE_ARG"
  timeout "$TIMEOUT_SEC" codex exec \
    -m "$MODEL" \
    -c "model_reasoning_effort=\"$EFFORT\"" \
    -s read-only \
    -o "$REPLY_FILE" - < "$PROMPT_FILE" > "$LOG_FILE" 2>&1
  CODEX_EXIT=$?
fi

if [ "$CODEX_EXIT" -eq 124 ]; then
  echo "ERROR: codex exec が ${TIMEOUT_SEC}s 以内に完了しませんでした（ログ: $LOG_FILE）" >&2
  exit 124
fi
if [ "$CODEX_EXIT" -ne 0 ]; then
  echo "ERROR: codex exec が exit $CODEX_EXIT で失敗しました（ログ: $LOG_FILE）" >&2
  exit 1
fi
if [ ! -s "$REPLY_FILE" ]; then
  echo "ERROR: reply が空です（ログ: $LOG_FILE）" >&2
  exit 2
fi

if [ "$MODE_ARG" != "--resume" ]; then
  SESSION_ID="$(sed -n 's/^.*session id: *//p' "$LOG_FILE" | head -1 | tr -d '[:space:]')"
  if [ -z "$SESSION_ID" ]; then
    echo "ERROR: session id を抽出できませんでした（ログ: $LOG_FILE）" >&2
    exit 3
  fi
  printf '%s\n' "$SESSION_ID" > "$SESSION_FILE"
  echo "OK: session=$SESSION_ID"
fi

echo "OK: reply=$REPLY_FILE"
exit 0
