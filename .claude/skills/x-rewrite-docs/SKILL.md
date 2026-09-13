---
name: x-rewrite-docs
description: リポジトリ文書の記載を実装と照合して修正する。ドキュメントの整合性確認・書き直しを依頼されたときに使う。
allowed-tools: Bash, Read, Grep, Glob, Edit, Write, Agent
disable-model-invocation: true
---

ドキュメントを実装の実態（真実源）と突き合わせ、乖離を修正する。
憶測で記述を追加せず、実在しないファイル・手順への参照を作らない。

## 対象ホワイトリストと真実源

| 文書 | 真実源 |
|---|---|
| `README.md` | `early-init.el` / `init.el` / `loads/inits/` の実ファイル / `Makefile` / `emacs-setup.sh` |
| `docs/architecture.md` | 起動シーケンス実装・実ディレクトリ構成・`loads/inits/` の実ファイル・パスヘルパー定義 |
| `docs/keybindings.md` | `loads/inits/*.el` と `loads/site-elisp/my-*.el` の実バインド定義 |
| `docs/cpp.md` | `18-built-in-package.el` / `19-language-modes.el` / `31-editing.el` / `my-gtags.el` |
| `docs/packages.md` | `early-init.el` の straight 設定 / `emacs-setup.sh` / `loads/straight/versions/default.el` |
| `docs/testing.md` | `Makefile` のターゲット定義 / `tests/` の実ファイル / `.github/workflows/` |
| `AGENTS.md` | 実在ファイル・実在コマンド・`.claude/rules/*.md` |
| `.claude/rules/codebase-map.md` | 起動シーケンス実装・実ディレクトリ構成・パスヘルパー定義 |
| `.claude/rules/verification.md` | `Makefile` のターゲット定義 |
| `.claude/rules/` のその他 | 対応する実装・設定ファイル |
| `.codex/README.md` | `.codex/` の実構成・`AGENTS.md` |

上記以外のファイルはこのスキルの対象外とする。`$ARGUMENTS` で対象を指定できる。
会話で指定された範囲も対象指定として扱う。全体の書き直し依頼なら表の範囲で進め、
対象が結果を左右するほど曖昧な場合だけ確認する。

## Steps

### 1. 実態の読み取り

対象文書の主張に関係する真実源だけを読む。確認対象の例:

- `loads/inits/` の実ファイル一覧と番号帯
- `Makefile` のターゲット一覧（`rg '^[a-z-]+:' Makefile`）
- `emacs-setup.sh` のオプション一覧
- 文書が参照しているパス・コマンド・キーバインド

### 2. 乖離の検出

文書の記述と実態を突き合わせ、乖離リストを作る（記載箇所 / 現状の記述 / 実態 / 対処案）。
機械的に確認できるものは Bash で確認する:

- 記載パスの実在（`test -e`）
- make ターゲットの実在
- 文書間リンク・参照先の実在

### 3. 書き換え

- 修正依頼の範囲内なら編集・検証まで進める。監査だけの依頼なら乖離の報告で完了する。
- 日本語で記述し、既存の文体・構成に合わせる。
- 規約が文書間で重複している場合は、正本（`AGENTS.md` または `.claude/rules/*.md`）へ
  集約し、他方は参照に置き換える（AGENTS.md の単一情報源原則）。
- 実装の変更はしない。実装側が誤っていると思われる場合は報告に留める。

### 4. 検証

- Step 2 の機械チェックを書き換え後に再実行し、新たな乖離が無いことを確認する。
- `git diff` で意図しない編集が無いことを確認する。

### 5. コミットと報告

- コミットする場合は `docs/` プレフィックスのブランチで `docs(scope): 要約` 形式にする
  （main へ直接コミットしない）。
- 修正した乖離、書き換えの要約、残した課題（実装側の疑義を含む）を報告する。
