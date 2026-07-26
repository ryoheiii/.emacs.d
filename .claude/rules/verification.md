---
description: Emacs 設定変更後の必須検証と差分確認手順を定義する
globs: ["**/*"]
---

# 検証手順

変更を完了とする前に、変更範囲に応じて次を上から実行する。

## 1. 回帰テスト

設定変更後は、変更範囲に応じて Makefile のテストランナーを実行する。

```sh
# フルスイート
make test

# 起動設定だけを変更した場合の最小検証
make test-startup
```

終了コードと標準エラーを確認する。init-loader のエラーログが非空の場合は
`make test-startup` が非ゼロ終了するため、警告を無条件に成功扱いしない。

`make test` を実行しない場合は、変更範囲に対応する次のターゲットを追加で実行する。
全ターゲットの一覧と検証内容は `docs/testing.md` を正本とする。

| 変更範囲 | 追加で実行するターゲット |
|---|---|
| 設定ファイルの構文・byte compile 警告 | `make lint` |
| `early-init.el` のパスヘルパー、バックアップ先 | `make test-unit` |
| C-t タグナビゲーションのキーバインド | `make test-keybinding` |
| C/C++ 設定（スタイル、eglot 起動条件、検索経路、tree-sitter 切替、irony ゲート） | `make test-cpp-config` |
| `:defer` 付きパッケージ宣言 | `make test-deferred` |
| グローバルモードのフック登録、feature のロード状態、ts モードのフック parity | `make test-invariants` |
| テスト基盤・lint 基盤（`test-emacs-setup.sh`、`tests/my-bench-run.sh`、`Makefile` の `lint*` / `test-*`） | `make test-guards` |

`make clean-test` は `tests/` 配下の byte compile 生成物を削除する補助ターゲットである。
`make straight-thaw` は CI 専用で、`CI=true` 以外の環境では実行できない。

## 2. tty (`emacs -nw`) 検証

日常利用は `emacs -nw` である。表示、モードライン、キーバインド、補完、
クリップボード、端末初期化、GUI 分岐に影響する変更では次を必ず実行する。

```sh
# 非 GUI 分岐のロード条件（batch）
make test-tty

# 実 pty での emacs -nw 起動ライフサイクル
make test-tty-live
```

- `make test-startup` だけでは tty 固有の退行を検出できない。GUI 分岐へ触れた
  場合は GUI 側だけの確認で完了としない。
- `test-tty-live` は Linux と `script`、`timeout` を必要とする。実行条件と
  注意点（対話 Emacs との straight ビルドキャッシュ共有、コールドキャッシュ時の
  事前ウォームアップ）は `docs/testing.md` を参照する。
- 実行できない環境では成功扱いにせず、理由と代替確認を報告する。

## 3. パッケージのリビルド

パッケージ宣言、straight の recipe、コンパイル状態へ影響する場合は、必要に応じて `straight-rebuild-all` を付けたバッチリビルドを実行する。
単なる文書変更では実行しない。

## 4. セットアップスクリプトのテスト

`test-emacs-setup.sh` が存在し、`emacs-setup.sh` または関連するセットアップ動作を変更した場合は、次を実行する。

```sh
make test-setup
```

## 5. 差分確認

- `git status --porcelain` で変更対象を列挙する。
- `git status --porcelain --ignored` で ignore 対象も確認し、リポジトリルート直下へ意図しない生成物が増えていないことを確かめる。
- `git diff` で内容を確認し、意図しない編集がないことを確かめる。
- 特にリポジトリルート直下へ自動生成ファイルが混入していないことを確認する。
- 実行したコマンド、結果、未実施項目と理由を完了報告へ記載する。
