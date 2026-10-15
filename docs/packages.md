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

**この判断は straight.el の実装読解に基づくもので、動的検証はしていない。**
想定と違っていた場合は `M-x straight-check-all` を手動実行すれば回復できる。

## パッケージの更新と lockfile

パッケージ更新時は、次の順序で設定と lockfile を同時に検証する。

1. Emacs で `M-x straight-pull-all` を実行する。
2. `make test` を実行する。
3. Emacs で `M-x straight-freeze-versions` を実行する。
4. `loads/straight/versions/default.el` を設定変更と同じコミットへ含める。

lockfile は CI の `make straight-thaw` がリビジョンを固定するために使う。
thaw 後は `make check-lockfile` で各 repo の HEAD・未コミット変更・過不足を検査する。
未導入を許す条件付きパッケージは CI の `LOCK_ALLOW_MISSING` に名前を列挙し、件数も出力する。
ローカルの現状確認と lockfile の再現確認は分け、日常利用中の共有キャッシュを thaw しない。
新しいパッケージを追加したら必ず凍結する。

## アーカイブと復元

回線が細い環境やオフライン環境へ移すときに使う。

```sh
# パッケージのアーカイブ（repos と lockfile を package.tar.gz へ固める）
./emacs-setup.sh --packing-package

# パッケージの復元（展開後に straight-rebuild-all まで自動実行する）
./emacs-setup.sh --extract-package
```

アーカイブ作成は一時ファイルへ完了してから `package.tar.gz` を置換する。
作成に失敗した場合、以前のアーカイブを保持する。

`--extract-package` は一時ディレクトリへ展開・検証してから既存の
`loads/straight/` を `.bak` へ退避し、新しいツリーを配置する。配置失敗時は
旧ツリーを戻す。起動設定のエラー（init-loader が捕捉したものを含む）や
その検査の失敗時はリビルドを開始せず、リビルド失敗時と同様に `.bak` を残して復旧方法を表示する。
リビルド成功後にだけ `.bak` を削除する。残っている `.bak` を上書きしない。

削除するのは通常ディレクトリの `var/package/eln-cache/` だけで、同名の symlink は保持する。
**文法・Copilot サーバーを含む兄弟データ、`var/hist/`、`var/backup/` は保持する**。
`--clean` はバックアップ世代や自動保存も削除するため、復旧作業前には実行しない。

## 手動リビルド

```sh
emacs --batch --eval "(setq user-emacs-directory \"$HOME/.emacs.d\")" \
  -l early-init.el -l init.el -f straight-rebuild-all
```

Emacs 内からは `M-x straight-rebuild-all`。`early-init.el` が `load-path-filter-function`（Emacs 31.1）で
load-path の走査結果をキャッシュしており、キャッシュはセッション中に無効化されない。
再ビルドで既存のビルドディレクトリへ増えたファイルは、Emacs を再起動するか
`(setq load-path-filter--cache nil)` を評価するまで読めない。

## 組み込みパッケージの上書きを避ける

straight の依存解決は、**組み込みで足りるパッケージでもレシピリポジトリに
存在すればそちらをクローンする**。`straight--convert-recipe` が `:type built-in`
へ落ちるのは、どのレシピリポジトリにも見つからなかったときだけである。

クローンされた版は `load-path` 上で組み込み版を覆い隠すため、`:straight nil`
で組み込みを使うつもりの宣言（`24-org.el` の org など）が実際には ELPA 版を
設定していた、という食い違いが起きる。

そのため、依存のうち **組み込み版で要件を満たすものだけ** `straight-override-recipe` で
built-in へ固定する。固定は宣言ごとに置く。ただし多数のパッケージが共通で依存する
`compat` / `seq` は、最初の依存元より前に効かせるため `init.el` にまとめて置く。

```elisp
;; init.el (全パッケージ共通の依存。straight-use-package 'use-package より前)
(dolist (pkg '(compat seq))
  (straight-override-recipe (list pkg :type 'built-in)))
;; 35-copilot.el (copilot / copilot-chat の依存)
(dolist (pkg '(org jsonrpc track-changes))
  (straight-override-recipe (list pkg :type 'built-in)))
;; 32-navigation.el (breadcrumb の依存)
(straight-override-recipe '(project :type built-in))
```

| パッケージ | 要求 | Emacs 31.1 組み込み | 扱い |
|---|---|---|---|
| `compat` | 30 (vertico / corfu / consult / magit / copilot など) | stub（版は Emacs に追随） | built-in へ固定。31.1 の stub が `compat-call` / `compat-function` を提供する |
| `seq` | 2.24 (magit)、2.23 (ELPA 版 compat) | 2.24（preload 済み） | built-in へ固定 |
| `org` | 9.4.6 (copilot-chat) | 9.8.7 | built-in へ固定 |
| `jsonrpc` | 1.0.14 (copilot) | 1.0.29 | built-in へ固定 |
| `project` | 0.9.8 (breadcrumb) | 0.12.0 | built-in へ固定（`tests/my-test-packages.el` の `my-test-packages-project-built-in` が固定） |
| `track-changes` | 1.4 (copilot) | 1.5 | built-in へ固定（straight 版 1.5 とコードも同一） |
| `transient` | 0.8.3 (copilot-chat) | 0.13.3 | 31.1 では要件を満たすが、magit が使う transient の差し替えを含めて未検証のため引き続き straight から導入 |

`compat` / `seq` / `track-changes` の固定は `tests/my-test-packages.el` の
`my-test-packages-common-deps-built-in` が検査する。

固定したパッケージは lockfile にも載らない。組み込み版の要件を満たさなくなる
パッケージを追加した場合は、この表とオーバーライドを見直す。

Copilot の残りの依存（polymode / shell-maker / request / aio / mcp など）は
組み込みに無いため通常どおり straight が管理する。
切り替え方法は [../README.md](../README.md) の「環境ごとの切り替え」を参照。
