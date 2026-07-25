# .codex/ — Codex 向け案内

このディレクトリは、Codex CLI でこのリポジトリを扱うための共有設定と案内を置く。

## 正本

- このリポジトリの全エージェント共通ルールの正本は、リポジトリルートの `AGENTS.md` である。
- 状況依存の詳細ルールは `.claude/rules/*.md` にある。Codex もこれらを読み取り参照する。
- 単一情報源を保つため、共通ルールを `.codex/` 配下へミラーしない。
- とくに `AGENTS.md` の「【最優先】CLI (`emacs -nw`) 前提」は必ず適用する。
  日常利用は端末の `emacs -nw` であり、tty の退行を許さず、tty へ影響する変更では
  `make test-tty` と `make test-tty-live` を実行する。

## `.claude/` の扱い

- `.claude/` 配下（settings、rules など）は Claude Code の設定領域である。
- Codex は既定で読み取りのみとし、変更はユーザーの明示指示がある場合に限る。
- 変更する場合も、`AGENTS.md` と `.claude/rules/*.md` の整合性を保つ。

## スキルとスラッシュコマンド

- スキルの正本は `.claude/skills/<name>/SKILL.md` であり、`.codex/skills` は
  `.claude/skills` への symlink である（一覧と用途は `AGENTS.md` を参照）。
- `/x-deep-plan` などのスラッシュコマンドは、`~/.codex/prompts/` に置かれた汎用シム
  （現在のチェックアウトの `.codex/skills/<name>/SKILL.md` を読み込んで従う形式）で解決する。
  シムが無い環境では、`.codex/skills/<name>/SKILL.md` を直接読み込んで実行する。
- レビュー系スキルが書き込む `.claude/review-state/`（git 管理外の実行時記録）は、
  「`.claude/` は読み取りのみ」の原則の例外として Codex からの書き込みを認める。

## 設定の使い分け

| ファイル | 用途 | Git 管理 |
|---|---|---|
| `.codex/config.toml` | 信頼済みチェックアウト向けの共有デフォルト | コミット対象 |
| `~/.codex/config.toml` | マシン固有の trust 判断、writable roots、モデル設定 | リポジトリ外 |

- 共有デフォルトは `approval_policy = "never"`、`sandbox_mode = "danger-full-access"` である。
  2026-07-25 にユーザーが明示決定した、信頼済みチェックアウト前提の設定である。
- この 2 値は承認プロンプトとサンドボックス保護を無効化する。第三者がこのチェックアウトを
  trust した場合も同じ権限が適用されることを理解して扱う。
- エージェントはこの 2 値を勝手に変更しない。緩める方向も、安全側（`on-request` /
  `workspace-write`）へ戻す方向も、ユーザーの明示指示がある場合だけ行う。
- API キー、トークン、個人情報をこのリポジトリへ置かない。`AGENTS.md` の禁止事項は
  権限設定にかかわらず適用される。

## 補足

- `sandbox_mode = "danger-full-access"` ではサンドボックスの保護が働かないため、
  `.codex/` 配下への自己書き込みも技術的には防げない。運用ルールとして、
  このディレクトリの変更はユーザーまたは Codex 以外のエージェントが明示的に行う。
