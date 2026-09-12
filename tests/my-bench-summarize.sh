#!/bin/bash
# my-bench-summarize.sh --- 完走した実行の有効試行だけを集計する
set -euo pipefail
python3 "$(dirname "${BASH_SOURCE[0]}")/my-bench-summarize.py" "${1:?出力ディレクトリが必要です}"
