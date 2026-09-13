# AGENTS.md

この文書は全エージェント共通の正本である。エージェント固有の設定より先に読み、
矛盾する場合は安全側の指示を優先する。コメント、ドキュメント、完了報告は日本語で記述する。

## リポジトリと変更範囲

C/C++ 開発、Markdown／Org、Mozc／Migemo による日本語編集に使う Emacs 設定である。
`early-init.el`（環境・パス・パッケージ基盤）→ `init.el`（use-package・init-loader）→
`loads/inits/NN-name.el`（番号／アルファベット順）の責務とモジュール境界を保つ。
変更は依頼された目的を達成する範囲に限定する。

## 【最優先】CLI (`emacs -nw`) 前提

日常利用は端末上の `emacs -nw` である。tty の動作を最優先の不変条件とし、
機能強化も tty 側を優先する。GUI だけを改善して tty を据え置く設計は選ばない。

- GUI 限定機能は `:if (display-graphic-p)` などで分離する。
- アイコンフォント、画像、ピクセル指定、GUI 専用キーやマウスを共通設定の前提にしない。
  キーバインドは端末が送出できるキーで機能させる。
- `corfu-terminal`、`xclip` など端末向けの代替を削除・無効化・弱体化しない。
- tty に影響し得る変更と GUI 分岐の変更では `make test-tty` と `make test-tty-live` を
  必ず実行する。既存テストで守れない挙動はテストを追加してから変更する。
  GUI 側だけの変更でも tty の退行がないことを確認する。

## 安全と作業の継続

- 作業開始時に `git status --porcelain` で既存差分を確認する。
  ユーザーや他セッションの変更を上書き・破棄・整形しない。
- 秘密情報、資格情報、個人情報を出力・ログ記録・コミットしない。
  発見した場合は内容を表示せず、場所と対処方針だけを報告する。
- `var/`、`loads/straight/`、`eln-cache`、`package.tar.gz` などの自動生成物を手で編集せず、
  リポジトリルートへ生成物を置かない。削除には `emacs-setup.sh` の対応コマンドを使う。
  `--clean` はバックアップ・自動保存・操作履歴など復元不能なデータも消し、
  `--clean-all` はさらにパッケージを消すため、目的と影響について承認を得てから実行する。
- 依頼範囲の調査・編集・検証と、その変更に起因する失敗の修正・再検証は継続して行う。
  実装の初稿で止めず、完了条件まで進める。既に承認された操作の再確認は不要である。
- 結果を大きく左右する不明点は、まず読み取り調査で解消する。
  解消できない仕様判断、範囲拡大、未承認の破壊的操作・外部公開が必要な場合に確認する。

## Git 運用

- **1 タスク = 1 worktree = 1 ブランチを必須とする。** タスク専用 worktree 内だけで編集し、
  メインチェックアウトで直接実装したり、そのブランチを切り替えたりしない。
- 他セッションの worktree・ブランチへ触れない。想定外のブランチや差分を見つけたら、
  上書きせず停止して報告する。作成・再利用の手順は
  [.claude/rules/git-workflow.md](.claude/rules/git-workflow.md) に従う。
- `main`／`master` へ直接コミットしない。コミットは小さな論理単位とし、
  整形・自動生成とロジック変更を混在させない。
- マージ前に履歴を確認・整理するが、明示指示なしの rebase・squash・履歴改変は行わない。
  デフォルトブランチへのマージは必ず `git merge --no-ff` とする。
- マージ済みのタスクブランチと worktree は同じ作業中に削除する
  （push 済みならリモートも）。他タスクや未マージのものは削除しない。
  `git branch -D` と force push は禁止する。

## 変更に応じて読む規約

該当する作業の規約を読み、無関係な文書まで一括で読み込まない。

| 作業 | 参照先と守る契約 |
|---|---|
| Elisp の変更 | [.claude/rules/elisp-conventions.md](.claude/rules/elisp-conventions.md): straight.el・use-package、組み込みの `:straight nil`、`:custom`・`:hook`・`:bind`、環境プレフィックス |
| 配置・起動順・パスの変更 | [.claude/rules/codebase-map.md](.claude/rules/codebase-map.md): 番号帯とパスヘルパー。パスをハードコードせず、パッケージ同梱データは公開変数で参照する |
| C/C++・スニペットの変更 | [.claude/rules/elisp-conventions.md](.claude/rules/elisp-conventions.md): 固定タグナビゲーションと yasnippet の個人用 2 レイアウトを維持する |
| シェルの変更 | [.claude/rules/shell-conventions.md](.claude/rules/shell-conventions.md): エラー処理と shellcheck 方針 |
| 検証の選択・実行 | [.claude/rules/verification.md](.claude/rules/verification.md)、[docs/testing.md](docs/testing.md): 対象別ターゲットと実行条件 |
| コミット・マージ・後片付け | [.claude/rules/git-workflow.md](.claude/rules/git-workflow.md)、[.claude/rules/commit-conventions.md](.claude/rules/commit-conventions.md) |
| セットアップ・保守・復旧 | [README.md](README.md)、[docs/packages.md](docs/packages.md)、`emacs-setup.sh --help` |

## 検証と完了条件

変更範囲に対応する最小十分な検証を行う。設定変更は `make test-startup`、
セットアップ動作の変更は `make test-setup` に加えて、該当する回帰テストを選ぶ。
tty の必須検証は上記のとおり。文書・指示だけの変更では参照先・コマンド・整合性を検査し、
無関係な Emacs テストやパッケージ再構築を要求しない。
全パッケージの再構築はパッケージ状態が影響する場合のみ、目的と影響を確認して行う。

調査で `emacs --batch` を直接実行するときは必ず `early-init.el` を読み込む。
省くと native-comp の生成物が直下の `eln-cache/` に落ちる
（正規の保存先は `var/package/eln-cache/`）。作業中のチェックアウトを指す例:

```sh
emacs --batch --eval '(setq user-emacs-directory (file-name-as-directory default-directory))' \
  -l early-init.el -l init.el -l <調査用の elisp>
```

依頼された変更と関連検証を終え、`git diff` と `git status` で意図しない差分・生成物を
除外して完了とする。成功した検証、未検証事項と理由、残る制約・リスクを日本語で簡潔に報告する。
動作・整合性を確認できなければ完了扱いにしない。

## エージェント設定とスキル

- 共通スキルの正本は `.claude/skills/<name>/SKILL.md`。
  `.codex/skills` は `.claude/skills` への symlink とし、内容を複製しない。
- 共通方針は本書、詳細は `.claude/rules/`、エージェント固有設定は `.claude/`・`.codex/`
  に置く。重複は正本への参照に置き換え、実在しない参照を作らない。
- スキルは本書と該当 rules に従う。明示されたスキル、または依頼に必要な専門手順を選び、
  計画・レビュー・ship をすべての編集へ自動適用しない。
- Codex のスキルとスラッシュコマンド、設定の扱いは [.codex/README.md](.codex/README.md) を参照する。
