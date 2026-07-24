---
name: x-tidy-settings
description: .claude/settings.json（共有）と .claude/settings.local.json（ローカル）を分類・重複排除・整列する。絶対パスやマシン固有の権限をローカル側へ移動し、共有設定を共有可能な状態に保つ。
allowed-tools: Bash, Read, Write, Grep, Glob
---

`.claude/settings.json`（共有・コミット対象）と `.claude/settings.local.json`
（ローカル・gitignored）を整理する。CLAUDE.md の「設定ファイル」節の運用を実施するスキル。

## Steps

### 1. 読み込み

- `.claude/settings.json` を読む。
- `.claude/settings.local.json` を読む（存在しなければ
  `{"permissions":{"allow":[],"additionalDirectories":[]}}` として扱う）。

### 2. 分類（shared vs local）

- **settings.json（共有）に残す:**
  - コマンド名ベースの `Bash(コマンド名:*)` 許可（絶対パスを含まないもの）
  - `WebFetch(domain:...)` / `WebSearch` などプロジェクト共通のもの
  - `Skill(...)`: リポジトリ共通のスキル
  - `Read(//tmp/**)` など環境非依存のパス
  - `hooks` 設定
- **settings.local.json（ローカル）へ移動する:**
  - `/home/`・`/mnt/`・`C:\` を含むパスのエントリ（`Read` / `Bash` / `additionalDirectories`）
  - マシン固有のコマンドや個人環境だけで必要な権限

### 3. 重複排除と整列

- 同一ファイル内で、広いパターンに包含される狭いエントリのみ削除する
  （例: `Bash(git:*)` があれば `Bash(git status:*)` は不要）。
- 広いパターンを新規に作り出さない（既存エントリの削除・移動のみ）。
- `allow` 配列はカテゴリ順に整列する:
  1. `WebFetch` / `WebSearch` → 2. `Skill` → 3. `Read` →
  4. Bash 読み取り系ユーティリティ → 5. Bash 開発ツール（git, gh, make, emacs, codex など）→
  6. その他（各カテゴリ内はアルファベット順）。
- 未知の形式のエントリは削除せず各カテゴリ末尾に置く。

### 4. 確認と更新

- 変更前後の差分を表示し、ユーザーに確認を求める。
- 確認後、両ファイルを更新する。

### 5. 検証

- JSON 構文チェック: `python3 -c "import json; json.load(open('.claude/settings.json'))"`
  （local 側も同様）。
- 共有側にローカルパス（`/home/`・`/mnt/`・`C:\`）が残っていないことを確認する。
- ローカル側の既存エントリが失われていないことを確認する。
- `hooks` 設定が壊れていないことを確認する。

### 6. コミット

- `.claude/settings.json` に差分がある場合は、`chore/` プレフィックスのブランチで
  `chore(settings): 要約` としてコミットする（main へ直接コミットしない。
  マージと push は通常の Git 運用に従い、push はユーザーの明示依頼時のみ）。
- `.claude/settings.local.json` は git 管理外のためコミットしない。
