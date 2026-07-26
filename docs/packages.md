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
`var/package/`（ネイティブコンパイルキャッシュ等）を消してリビルドする。
`--clean` とは異なり、**`var/hist/` と `var/backup/` のユーザーデータは残す**。

## 手動リビルド

```sh
emacs --batch --eval "(setq user-emacs-directory \"$HOME/.emacs.d\")" \
  -l early-init.el -l init.el -f straight-rebuild-all
```

Emacs 内からは `M-x straight-rebuild-all`。

## 組み込みパッケージの上書きを避ける

straight の依存解決は、**組み込みで足りるパッケージでもレシピリポジトリに
存在すればそちらをクローンする**。`straight--convert-recipe` が `:type built-in`
へ落ちるのは、どのレシピリポジトリにも見つからなかったときだけである。

クローンされた版は `load-path` 上で組み込み版を覆い隠すため、`:straight nil`
で組み込みを使うつもりの宣言（`24-org.el` の org など）が実際には ELPA 版を
設定していた、という食い違いが起きる。

そのため `35-copilot.el` は、copilot / copilot-chat の依存のうち
**Emacs 30.2 の組み込み版で要件を満たすものだけ** built-in へ固定する。

```elisp
(dolist (pkg '(org jsonrpc))
  (straight-override-recipe (list pkg :type 'built-in)))
```

| パッケージ | 要求 | Emacs 30.2 組み込み | 扱い |
|---|---|---|---|
| `org` | 9.4.6 (copilot-chat) | 9.7.11 | built-in へ固定 |
| `jsonrpc` | 1.0.14 (copilot) | 1.0.25 | built-in へ固定 |
| `track-changes` | 1.4 (copilot) | 1.2 | 要件未満のため straight から導入 |
| `transient` | 0.8.3 (copilot-chat) | 0.7.2.2 | 要件未満のため straight から導入 |

固定した 2 つは lockfile にも載らない。組み込み版の要件を満たさなくなる
パッケージを追加した場合は、この表とオーバーライドを見直す。

Copilot の残りの依存（polymode / shell-maker / request / aio / mcp など）は
組み込みに無いため通常どおり straight が管理する。
切り替え方法は [../README.md](../README.md) の「環境ごとの切り替え」を参照。
