#!/usr/bin/env python3
"""straight の lockfile と導入済みリポジトリを変更せず照合する。"""
import argparse
from pathlib import Path
import re
import subprocess
import sys


def check(lockfile, repos, allow_missing=()):
    text = lockfile.read_text()
    pattern = r'\("([^"/\n]+)"\s+\.\s+"([a-f0-9]{40})"\)'
    pairs = re.findall(pattern, text)
    remainder = re.sub(pattern, '', text)
    if not pairs or re.sub(r'[\s()]|:epsilon', '', remainder):
        raise ValueError('lockfile の形式が不正です')
    locked = dict(pairs)
    if len(locked) != len(pairs):
        raise ValueError('lockfile に重複があります')
    allowed = set(allow_missing)
    if allowed - locked.keys():
        raise ValueError('未導入の許容名が lockfile にありません')
    if not repos.is_dir():
        raise ValueError('repos ディレクトリがありません')
    present = {p.name for p in repos.iterdir() if p.is_dir()}
    errors = [f'{name}: lock entry がありません' for name in sorted(present - locked.keys())]
    skipped = []
    checked = 0
    for name, commit in pairs:
        repo = repos / name
        if name not in present:
            if name in allowed:
                skipped.append(name)
            else:
                errors.append(f'{name}: repo がありません')
            continue
        try:
            head = subprocess.check_output(['git', '-C', str(repo), 'rev-parse', 'HEAD'], text=True).strip()
            dirty = subprocess.check_output(['git', '-C', str(repo), 'status', '--porcelain'], text=True)
        except subprocess.CalledProcessError:
            errors.append(f'{name}: Git 状態を取得できません')
            continue
        if head != commit:
            errors.append(f'{name}: HEAD が lockfile と不一致です')
        if dirty:
            errors.append(f'{name}: 未コミット変更があります')
        checked += 1
    print(f'lock照合: {checked} repo、条件付き未導入 {len(skipped)} ({", ".join(skipped) or "なし"})')
    if errors:
        raise ValueError('\n'.join(errors))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('lockfile', type=Path)
    parser.add_argument('repos', type=Path)
    parser.add_argument('--allow-missing', action='append', default=[])
    args = parser.parse_args()
    try:
        check(args.lockfile, args.repos, args.allow_missing)
    except (OSError, ValueError) as error:
        sys.exit(str(error))
