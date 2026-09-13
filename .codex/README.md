# Codex 向け案内

共通ルールは [AGENTS.md](../AGENTS.md)、作業別の詳細はそこから参照する
`.claude/rules/` が正本である。Codex 固有の `agent.md` や規約のコピーは作らない。

## スキルの利用と編集

`.codex/skills` は `../.claude/skills` への symlink である。
このリポジトリの Codex スキルの変更依頼は、リンク先の共通スキルの編集として扱う。
Claude Code も使うため、特定モデルだけを前提にした指示にしない。

| スキル | 用途 |
|---|---|
| `x-preflight` | 実装前の作業環境と worktree の準備 |
| `x-deep-plan` | 設計上の未決事項を調査し、実装計画を作成 |
| `x-codex-review-plan` | 具体的な計画の外部レビュー |
| `x-codex-review-impl` | 差分の外部レビュー。`final` は ship の承認記録を作成 |
| `x-ship` | コミットからマージ・push・CI・後片付けまで |
| `x-rewrite-docs` | ドキュメントと実装の乖離を修正 |
| `x-tidy-settings` | Claude Code の共有・ローカル設定を整理 |

- `/x-deep-plan` などは `~/.codex/prompts/` の汎用シムが、現在のチェックアウトの
  `.codex/skills/<name>/SKILL.md` を読み込む。シムがなければそのファイルを直接読む。
- `SKILL.md` の適用条件でスキルを選び、用途に対応する参照だけを必要な時点で読む。
- 依頼された `.codex/` と共通スキルの編集、およびレビューが作る
  `.claude/review-state/` の実行時記録は Codex から更新できる。
  その他の Claude Code 固有設定は、変更依頼がある場合だけ編集する。

## 設定

| ファイル | 用途 |
|---|---|
| [.codex/config.toml](config.toml) | 信頼済みチェックアウト向けの共有デフォルト（Git 管理） |
| `~/.codex/config.toml` | マシン固有の trust 判断、writable roots、モデル設定（リポジトリ外） |

共有の `approval_policy = "never"` と `sandbox_mode = "danger-full-access"` は
2026-07-25 のユーザー決定である。承認プロンプトとサンドボックス保護を無効化し、
第三者が trust したチェックアウトや `.codex/` 自身への書き込みにも適用される。
この 2 値は安全側への変更も含め、ユーザーが明示的に変更を依頼した場合だけ更新する。
権限設定にかかわらず [AGENTS.md](../AGENTS.md) の安全規約を守る。

## 指示を保守するとき

[OpenAI の記事](https://developers.openai.com/blog/rethinking-skills-and-prompts-for-gpt-6-astra)
を踏まえ、短く具体的な適用条件、必要時に読む詳細、作業に応じた検証を使う。
一般的な心得や固定回数の作業を重ねず、tty・Git・承認境界など実際の制約を残す。
最適化は指示の長さだけで判断せず、通常の編集と境界条件で判断が保たれるかを確認する。
