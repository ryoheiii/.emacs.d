#!/usr/bin/env python3
"""manifest をコミット点として、全指標に同じ有効試行集合を使う。"""
from pathlib import Path
import re
import statistics
import sys


def stats(values):
    v = sorted(values)
    if not v:
        return 'n=0'
    q1, q3 = v[int(len(v) * .25)], v[min(int(len(v) * .75), len(v) - 1)]
    return (f'n={len(v)} min={v[0]:.4f} q1={q1:.4f} median={statistics.median(v):.4f} '
            f'q3={q3:.4f} max={v[-1]:.4f} iqr={q3-q1:.4f}')


def summarize(root):
    modes = {}
    for mode in ('bare', 'now'):
        manifest = root / f'{mode}-manifest.tsv'
        if not manifest.exists():
            if (root / f'{mode}-wall-ms.txt').exists():
                raise ValueError(f'{mode}: manifest がないため試行集合を特定できません。再計測してください。')
            continue
        trials, seen = [], set()
        for row in manifest.read_text().splitlines():
            path, ms = row.split('\t')
            if not re.fullmatch(rf'raw/{mode}-run-[A-Za-z0-9]+/{mode}-[1-9][0-9]*\.log', path):
                raise ValueError('manifest のログパスが不正です')
            file = root / path
            if not file.resolve().is_relative_to(root.resolve()) or path in seen:
                raise ValueError('manifest に範囲外または重複ログがあります')
            seen.add(path)
            if not re.fullmatch(r'[0-9]+', ms):
                raise ValueError('wall time が不正です')
            log = file.read_text()
            if 'invalid:' in log or 'MY_BENCH t1_window_setup=' not in log:
                raise ValueError('manifest に無効ログがあります')
            if mode == 'now' and 'MY_BENCH end' not in log:
                raise ValueError('manifest に未完了ログがあります')
            trials.append((float(ms), log))
        if not trials:
            raise ValueError('manifest が空です')
        modes[mode] = trials
    if not modes:
        raise ValueError('完走した実行の manifest がありません')
    print('# 起動コスト計測結果\n\n## wall time (ms)')
    for mode, trials in modes.items():
        print(mode, stats([ms for ms, _ in trials]))
    print('\n## Emacs 内部の観測点 (秒)')
    for key in ('t1_window_setup', 't3_ready', 'emacs_init_time'):
        for mode, trials in modes.items():
            values = [re.findall(rf'MY_BENCH {key}=([0-9]+(?:\.[0-9]+)?)', log) for _, log in trials]
            if mode == 'now' or key == 't1_window_setup' or any(values):
                if any(len(v) != 1 for v in values):
                    raise ValueError(f'{mode}: {key} の観測数が不一致です')
                print(key, mode, stats([float(v[0]) for v in values]))
    print('\n## 初回描画までの use-package コスト (秒)')
    for cls in ('external', 'builtin'):
        values = [re.findall(rf'MY_BENCH at_t1 class={cls} .*?eager=([0-9.]+)', log)
                  for _, log in modes.get('now', [])]
        if values:
            if any(len(v) != 1 for v in values):
                raise ValueError(f'{cls}: 観測数が不一致です')
            print(cls, stats([float(v[0]) for v in values]))
    packages = {}
    for _, log in modes.get('now', []):
        if not re.search(r'MY_BENCH sanity .*ok=yes\b', log):
            raise ValueError('eager 総和の健全性検査に失敗しました')
        for name, cls, value in re.findall(r'MY_BENCH pkg name=(\S+) class=(\S+) eager=([0-9.]+)', log):
            packages.setdefault((name, cls), []).append(float(value))
    print('\n## eager コスト上位パッケージ (中央値、秒)')
    for (name, cls), values in sorted(packages.items(), key=lambda entry: statistics.median(entry[1]), reverse=True)[:12]:
        if len(values) != len(modes['now']):
            raise ValueError(f'{name}: 観測数が不一致です')
        print(f'{statistics.median(values):.6f} {name} {cls}')


if __name__ == '__main__':
    try:
        summarize(Path(sys.argv[1]))
    except (OSError, ValueError, IndexError) as error:
        sys.exit(f'集計失敗: {error}')
