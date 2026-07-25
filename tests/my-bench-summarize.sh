#!/bin/bash
# my-bench-summarize.sh --- 計測結果を集計する
#
# 使い方: tests/my-bench-summarize.sh <出力ディレクトリ>

set -euo pipefail

OUT_DIR="${1:?出力ディレクトリが必要です}"

# 中央値・四分位を出す（mean は使わない。外れ値の影響が大きいため）。
stats2() {
  sort -n | awk '
    { v[NR] = $1 }
    END {
      if (NR == 0) { print "n=0"; exit }
      med = (NR % 2) ? v[(NR+1)/2] : (v[NR/2] + v[NR/2+1]) / 2
      q1i = int(NR * 0.25) + 1; q3i = int(NR * 0.75) + 1
      if (q3i > NR) q3i = NR
      printf "n=%d min=%.4f q1=%.4f median=%.4f q3=%.4f max=%.4f iqr=%.4f\n", \
             NR, v[1], v[q1i], med, v[q3i], v[NR], v[q3i] - v[q1i]
    }'
}

extract() { # <mode> <正規表現> -> 値の並び
  grep -h -E "$2" "$OUT_DIR"/raw/"$1"-[0-9]*.log 2>/dev/null | tr -d '\r' \
    | sed -E "s/.*$3=([0-9.]+).*/\1/"
}

echo "# 起動コスト計測結果"
echo
echo "計測日時: $(date -Iseconds)"
echo "ホスト: $(uname -sr)"
echo "Emacs: $(${EMACS:-emacs} --version | head -1)"
echo

echo "## wall time (ms, プロセス外側)"
echo
for m in bare now; do
  if [ -s "$OUT_DIR/${m}-wall-ms.txt" ]; then
    printf '%-5s %s\n' "$m" "$(stats2 < "$OUT_DIR/${m}-wall-ms.txt")"
  fi
done
echo

echo "## Emacs 内部の観測点 (秒)"
echo
for key in t1_window_setup t3_ready emacs_init_time; do
  for m in bare now; do
    vals="$(extract "$m" "MY_BENCH $key=" "$key" || true)"
    if [ -n "$vals" ]; then
      printf '%-16s %-5s %s\n' "$key" "$m" "$(echo "$vals" | stats2)"
    fi
  done
done
echo

echo "## 初回描画までに払った use-package コスト (秒)"
echo
for cls in external builtin; do
  vals="$(grep -h "MY_BENCH at_t1 class=$cls " "$OUT_DIR"/raw/now-[0-9]*.log 2>/dev/null \
          | tr -d '\r' | sed -E 's/.* eager=([0-9.]+).*/\1/' || true)"
  if [ -n "$vals" ]; then
    printf '%-10s eager %s\n' "$cls" "$(echo "$vals" | stats2)"
  fi
done
echo

echo "## 健全性検査 (eager 総和 <= t1)"
grep -h "MY_BENCH sanity " "$OUT_DIR"/raw/now-[0-9]*.log 2>/dev/null | tr -d '\r' \
  | sed -E 's/.*ok=([a-z]+).*/\1/' | sort | uniq -c
echo

echo "## eager コスト上位パッケージ (全試行の中央値, 秒)"
echo
grep -h "MY_BENCH pkg " "$OUT_DIR"/raw/now-[0-9]*.log 2>/dev/null | tr -d '\r' \
  | sed -E 's/.*name=([^ ]+) class=([^ ]+) eager=([0-9.]+).*/\1 \2 \3/' \
  | awk '{ key = $1 " " $2; vals[key] = vals[key] " " $3 }
         END { for (k in vals) {
                 n = split(vals[k], a, " ")
                 for (i = 1; i < n; i++) for (j = i+1; j <= n; j++)
                   if (a[i] > a[j]) { t = a[i]; a[i] = a[j]; a[j] = t }
                 med = (n % 2) ? a[(n+1)/2] : (a[n/2] + a[n/2+1]) / 2
                 printf "%.6f %s\n", med, k
               } }' \
  | sort -rn | head -12
