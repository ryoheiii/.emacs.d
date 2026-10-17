<!-- -*- gfm -*- -->
# パッケージ管理

パッケージは [straight.el](https://github.com/radian-software/straight.el) で管理する（`package.el` は無効）。
各パッケージの Git リポジトリ（以下 repo）は `loads/straight/repos/`、ビルド成果物は `loads/straight/build/` にあり、
`loads/straight/` のうち Git で管理するのは lockfile（`loads/straight/versions/default.el`）だけ。

lockfile は、各パッケージで使う Git コミットを記録した一覧。別の環境や CI でも同じ版を使うために保存する。
「更新」は新しいソースを取得すること、「リビルド」は取得済みのソースから Emacs が使うファイルを作り直すことを指す。

## 更新と lockfile

1. `M-x straight-pull-all` で全パッケージの新しいソースを取得する。
2. `make test` を実行する。
3. `M-x straight-freeze-versions` で現在使っている版を lockfile に記録する。
4. 更新した lockfile をコミットする。更新に合わせて設定を直したときは、同じコミットに含める。

- パッケージを追加・削除したときも手順 3〜4 で lockfile を更新する。依存に組み込みパッケージが含まれるときは
  [組み込みパッケージの固定](#組み込みパッケージの固定) も確認する。
- lockfile にはそのセッションで有効なパッケージだけが書かれる。Copilot を無効にした環境で更新すると Copilot 関連が
  lockfile から消えるため、更新は Copilot が有効な環境（Node.js あり）で行う。
- CI は lockfile のリビジョンを適用し、導入済みの repo と照合する。lockfile に無い repo と、repo が見つからない lockfile の
  エントリは、どちらも失敗になる。OS 限定などで導入しないことがある repo は、`.github/workflows/test.yml` の
  `LOCK_ALLOW_MISSING` と `.github/workflows/platform.yml` の `--allow-missing` へ追加する（名前は lockfile のキー）。
- パッケージを削除したり組み込みへ固定したときは、`.github/workflows/test.yml` の straight キャッシュキー（`straight-v<N>-`）の
  番号も上げる。旧キャッシュに残った repo が照合で失敗する。
- 日常利用している `loads/straight/` では `M-x straight-thaw-versions` を実行しない（lockfile へ反映していない更新が巻き戻る）。
  lockfile との照合は `make check-lockfile STRAIGHT_DIR=<隔離した straight>` で行う（[テスト](testing.md#ターゲット一覧)）。

## アーカイブと復元

GitHub へ到達できない環境へパッケージを持ち込むときに使う。手順は README の
[GitHub へ到達できない環境](../README.md#github-へ到達できない環境)。ここでは `--extract-package` の挙動と失敗時の復旧を書く。

- `--extract-package` はアーカイブを展開して検証した後、既存の `loads/straight/` を `loads/straight.bak` へ退避して配置し、
  `var/cache/eln-cache/` を破棄する。続けて設定を読み込んで全パッケージをリビルドし、成功したら `loads/straight.bak` を削除する。
- 設定の読み込みエラー（`custom/custom.el` や `loads/inits/99-private.el` のものを含む）があるとリビルドを始めずに中止する。
- リビルドに失敗すると `loads/straight.bak` が残り、残っている間は `--extract-package` を再実行できない。
  元の状態へ戻すには次を実行する。

  ```sh
  rm -rf ~/.emacs.d/loads/straight && mv ~/.emacs.d/loads/straight.bak ~/.emacs.d/loads/straight
  ```

- `var/state/`、`var/backup/`、tree-sitter 文法、Copilot language server は消さない。`--clean` はこれらも削除するため、
  復旧作業の前には実行しない（[クリーンアップ](../README.md#クリーンアップ)）。

## 手動リビルド

全パッケージのビルド成果物を作り直すときに使う（`--extract-package` は内部で実行するため不要）。`~/.emacs.d` で次を実行する。

```sh
emacs --batch --eval "(setq user-emacs-directory \"$HOME/.emacs.d\")" \
  -l early-init.el -l init.el -f straight-rebuild-all
```

Emacs 内からは `M-x straight-rebuild-all`。リビルドで新たに生成されたファイルは、Emacs を再起動するまで読み込まれない。

パッケージのソース変更は、Emacs でパッケージのファイルを保存したときと `straight-pull-all` の実行時に straight が自動で記録し、
次の起動時にそのパッケージだけをリビルドする。`straight-thaw-versions` でリビジョンを戻したときや、シェルから `loads/straight/repos/` で
`git` コマンドを実行したときは記録されないため、`M-x straight-check-all` で変更を検出して変更分だけをリビルドする。

## 組み込みパッケージの固定

組み込みパッケージは Emacs 本体に同梱されているもの。straight は、組み込みで足りるパッケージでも取得方法の指定（レシピ）があれば
repo をクローンし、Emacs はそのクローン版を組み込み版より優先して読み込む。
次の依存は `straight-override-recipe` で組み込み版（`:type built-in`）に固定している。固定したパッケージは lockfile に載らない。

| パッケージ | 配置 | 依存元 |
|---|---|---|
| `compat`、`seq` | `init.el` | vertico、corfu、consult、magit など |
| `project` | `loads/inits/32-navigation.el` | breadcrumb |
| `org`、`jsonrpc`、`track-changes` | `loads/inits/35-copilot.el` | copilot、copilot-chat |

`compat` と `seq` は多数のパッケージが依存するため、最初の依存元より前に効くよう `init.el` で固定する。
Copilot 用の 3 件は Copilot が有効なときだけ固定する。
追加したパッケージが、表の依存について組み込み版より新しい版を要求するときは、この表と固定を見直す。
