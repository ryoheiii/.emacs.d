#!/bin/bash
# my-bench-run.sh --- 起動コストを実 pty で計測する
#
# issue #7（elpaca 移行の検討）の判断材料。
# 「非同期化で削減できる時間の上限 = 外部パッケージ活性化に費やしている時間」を測る。
#
# 使い方:
#   tests/my-bench-run.sh now  <試行数> <出力ディレクトリ>   # 現行設定
#   tests/my-bench-run.sh bare <試行数> <出力ディレクトリ>   # emacs -Q -nw の下限
#
# 計測はプロセス外側の wall time で行う（主用途が emacs -nw のため実 pty を使う）。

set -euo pipefail

MODE="${1:?mode (now|bare) が必要です}"
TRIALS="${2:?試行数が必要です}"
OUT_DIR="${3:?出力ディレクトリが必要です}"

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EMACS="${EMACS:-emacs}"
STRAIGHT_DIR="${STRAIGHT_DIR:-$REPO_ROOT/loads/straight}"
BENCH_ROOT="$REPO_ROOT/.bench/$MODE"

# 1 試行あたりの上限。超えたら無効試行として扱う。
TRIAL_TIMEOUT="${TRIAL_TIMEOUT:-300}"
# 1 分 loadavg がこの値を超える試行は無効にする。
LOADAVG_MAX="${LOADAVG_MAX:-4.0}"
# 無効試行の再試行上限。
RETRY_MAX="${RETRY_MAX:-8}"

for cmd in script timeout stty; do
  command -v "$cmd" >/dev/null || { echo "my-bench-run: $cmd が必要です" >&2; exit 1; }
done
test "$(uname)" = Linux || { echo "my-bench-run: Linux が必要です" >&2; exit 1; }

mkdir -p "$OUT_DIR/raw"

#### ベンチルートの構築（試行間で使い回す。eln-cache を温存するため永続にする） ####
setup_bench_root() {
  rm -rf "$BENCH_ROOT"
  mkdir -p "$BENCH_ROOT/xdg-cache"

  if [ "$MODE" = bare ]; then
    return 0
  fi

  git -C "$REPO_ROOT" checkout-index --all --prefix="$BENCH_ROOT/"
  if ! git -C "$REPO_ROOT" diff --quiet; then
    git -C "$REPO_ROOT" diff --binary | git -C "$BENCH_ROOT" apply --whitespace=nowarn
  fi

  # パッケージは worktree の実体を共有する（ベンチルート配下へ再クローンしない）。
  if [ -e "$BENCH_ROOT/loads/straight" ] || [ -L "$BENCH_ROOT/loads/straight" ]; then
    find "$BENCH_ROOT/loads/straight" -depth -delete
  fi
  mkdir -p "$BENCH_ROOT/loads"
  ln -s "$STRAIGHT_DIR" "$BENCH_ROOT/loads/straight"

  # 実 early-init を退避し、統計収集を有効化する shim を被せる。
  # use-package-compute-statistics は宣言の評価より前に t でなければならないため、
  # --eval では間に合わない（--eval は init 読み込み後に処理されるため）。
  mv "$BENCH_ROOT/early-init.el" "$BENCH_ROOT/my-bench-early-init-real.el"
  cat > "$BENCH_ROOT/early-init.el" <<'MY_BENCH_EARLY_INIT'
;;; early-init.el --- ベンチ専用 shim  -*- lexical-binding: t; -*-
;; 計測実行時の一時生成物。
;; straight-base-dir はベンチルートではなく実体を指す（tests/my-test-tty-live.el と同じ理由）。
(setq my-straight-base-dir-override (getenv "MY_BENCH_STRAIGHT_BASE_DIR"))
;; 宣言ごとの経過時間を use-package に記録させる（本計測の中核）。
(setq use-package-compute-statistics t)
(load (expand-file-name "my-bench-early-init-real.el" user-emacs-directory) nil t)
MY_BENCH_EARLY_INIT

  cat > "$BENCH_ROOT/run-bench.sh" <<MY_BENCH_RUNNER
#!/bin/sh
set -eu
stty cols 120 rows 40
export TERM=xterm-256color
export HOME="$BENCH_ROOT"
export XDG_CACHE_HOME="$BENCH_ROOT/xdg-cache"
export MY_BENCH_STRAIGHT_BASE_DIR="$STRAIGHT_DIR/../"
unset DISPLAY
exec $EMACS -nw --no-site-file --no-site-lisp \\
  --init-directory="$BENCH_ROOT" \\
  -L "$REPO_ROOT/tests" \\
  -l "$REPO_ROOT/tests/my-bench-startup.el"
MY_BENCH_RUNNER
  chmod +x "$BENCH_ROOT/run-bench.sh"
}

setup_bare_runner() {
  cat > "$BENCH_ROOT/run-bench.sh" <<MY_BARE_RUNNER
#!/bin/sh
set -eu
stty cols 120 rows 40
export TERM=xterm-256color
export HOME="$BENCH_ROOT"
export XDG_CACHE_HOME="$BENCH_ROOT/xdg-cache"
unset DISPLAY
exec $EMACS -Q -nw \\
  --eval '(add-hook (quote window-setup-hook) (lambda () (princ (format "MY_BENCH t1_window_setup=%.6f\\n" (float-time (time-subtract (current-time) before-init-time))) (function external-debugging-output)) (kill-emacs 0)) 90)'
MY_BARE_RUNNER
  chmod +x "$BENCH_ROOT/run-bench.sh"
}

#### 1 試行 ####
# 成功時は経過ミリ秒を stdout へ出す。無効試行なら空を返して非ゼロ終了。
run_trial() {
  local idx="$1"
  local log="$OUT_DIR/raw/${MODE}-${idx}.log"
  local load start end

  load="$(awk '{print $1}' /proc/loadavg)"
  if awk -v l="$load" -v m="$LOADAVG_MAX" 'BEGIN{exit !(l>m)}'; then
    echo "invalid: loadavg=$load > $LOADAVG_MAX" > "$log"
    return 1
  fi

  start="$(date +%s%N)"
  if ! timeout "$TRIAL_TIMEOUT" script -qec "$BENCH_ROOT/run-bench.sh" /dev/null > "$log" 2>&1; then
    echo "invalid: nonzero-exit-or-timeout" >> "$log"
    return 1
  fi
  end="$(date +%s%N)"

  if ! grep -q "MY_BENCH t1_window_setup=" "$log"; then
    echo "invalid: probe-line-missing" >> "$log"
    return 1
  fi
  if [ "$MODE" = now ] && ! grep -q "MY_BENCH end" "$log"; then
    echo "invalid: probe-incomplete" >> "$log"
    return 1
  fi

  echo "loadavg=$load" >> "$log"
  echo "$(( (end - start) / 1000000 ))"
}

#### 実行 ####
setup_bench_root
[ "$MODE" = bare ] && setup_bare_runner

# ウォームアップ（eln-cache とページキャッシュを温める）。値は捨てる。
echo "my-bench-run: $MODE ウォームアップ中..." >&2
for i in 1 2 3; do
  run_trial "warmup$i" >/dev/null 2>&1 || true
done

echo "my-bench-run: $MODE 本計測 ${TRIALS} 試行" >&2
: > "$OUT_DIR/${MODE}-wall-ms.txt"
valid=0
retries=0
i=0
while [ "$valid" -lt "$TRIALS" ]; do
  i=$((i + 1))
  if ms="$(run_trial "$i")"; then
    valid=$((valid + 1))
    echo "$ms" >> "$OUT_DIR/${MODE}-wall-ms.txt"
    printf '  trial %-3s %6s ms (valid %d/%d)\n' "$i" "$ms" "$valid" "$TRIALS" >&2
  else
    retries=$((retries + 1))
    printf '  trial %-3s INVALID (retry %d/%d)\n' "$i" "$retries" "$RETRY_MAX" >&2
    if [ "$retries" -gt "$RETRY_MAX" ]; then
      echo "my-bench-run: 無効試行が上限を超えました（有効 $valid/$TRIALS）。中止します。" >&2
      exit 2
    fi
  fi
done

echo "my-bench-run: $MODE 完了（有効 $valid、無効 $retries）" >&2
