<!-- -*- gfm -*- -->
# パッケージ管理（straight.el）

[straight.el](https://github.com/radian-software/straight.el) による Git ベースの
パッケージ管理を使う。`package.el` は `early-init.el` で無効化している。

`use-package` と `straight-use-package-by-default t` の組み合わせにより、
外部パッケージの宣言で `:straight t` は省略できる。組み込みパッケージには
必ず `:straight nil` を明示する。

## 変更検出の方式

起動時間を優先し、`early-init.el` で `straight-check-for-modifications` を
`(check-on-save find-when-checking only-once)` に設定している。既定の `find-at-startup` は
毎起動で `loads/straight/repos` 配下を `find(1)` で全走査するため、これを外している。

変更が自動記録されるのは次の場合で、通常の編集と更新はこれで足りる。

- Emacs でパッケージのファイルを保存した（`before-save-hook`）
- `M-x straight-pull-all` / `M-x straight-pull-package` で実際にマージが発生した

一方、次の場合は**自動検出されない**。`M-x straight-check-all` を実行してから利用する。

- `M-x straight-thaw-versions` でリビジョンを戻した
  （`straight-vc-git-check-out-commit` はチェックアウト成功時に変更記録を通らない）
- シェルから `loads/straight/repos/` 配下を直接 `git` 操作した

CI の `make straight-thaw` は、この理由からターゲット内で `straight-check-all` まで実行する。

`./emacs-setup.sh --extract-package` は展開後に `straight-rebuild-all` を実行するため、
手動の `straight-check-all` は不要である。`straight-rebuild-all` は変更検出を経由せず
全パッケージを無条件に再ビルドし、ビルドキャッシュの mtime も更新するため、
以降のセッションで検出漏れが起きる経路が無い。

## パッケージの更新と lockfile

パッケージ更新時は、次の順序で設定と lockfile を同時に検証する。

1. Emacs で `M-x straight-pull-all` を実行する。
2. `make test` を実行する。
3. Emacs で `M-x straight-freeze-versions` を実行する。
4. `loads/straight/versions/default.el` を設定変更と同じコミットへ含める。

lockfile は CI の `make straight-thaw` がリビジョンを固定するために使う。
新しいパッケージを追加したら必ず凍結する。

## アーカイブと復元

回線が細い環境やオフライン環境へ移すときに使う。

```sh
# パッケージのアーカイブ（repos と lockfile を package.tar.gz へ固める）
./emacs-setup.sh --packing-package

# パッケージの復元（展開後に straight-rebuild-all まで自動実行する）
./emacs-setup.sh --extract-package
```

`--extract-package` は既存の `loads/straight/` を削除してから展開し、
続けて `--clean` 相当の後始末とリビルドを行う。

## 手動リビルド

```sh
emacs --batch --eval "(setq user-emacs-directory \"$HOME/.emacs.d\")" \
  -l early-init.el -l init.el -f straight-rebuild-all
```

Emacs 内からは `M-x straight-rebuild-all`。

## Copilot を有効にしたときの依存

`copilot-chat` は org / jsonrpc / polymode / shell-maker / request / aio などを
依存として引き込む。このうち **`org` と `jsonrpc` は Emacs 組み込み版より
straight のビルドが優先される**（`load-path` の順による）。

- どちらも ELPA 版のほうが新しく、eglot・org の通常利用に支障は確認されていない。
- `my/copilot-enabled` を `nil` にすると `copilot-chat` 自体が登録されないため、
  `org` と `jsonrpc` は組み込み版に戻る。

Copilot の切り替え方法は [../README.md](../README.md) の「環境ごとの切り替え」を参照。
