<!-- -*- gfm -*- -->
# Emacs 設定

端末（`emacs -nw`）での日常利用を第一にした、モジュール構成の Emacs 設定。
C/C++ 開発、Markdown / Org、Mozc による日本語入力と Migemo による日本語検索に対応する。
パッケージは [straight.el](https://github.com/radian-software/straight.el) で管理し、使う版の一覧（lockfile）でバージョンを固定する。
外部ツールはどれも任意で、無ければ該当する機能が無効になるか代替へ切り替わる。

- 対象 Emacs: 31.1 以上
- 対象 OS: Linux（Debian / Ubuntu）、WSL2。macOS は設定のみ対応（`emacs-setup.sh` の `--setup` と `--install` は Linux 専用）

| ドキュメント | 内容 |
|---|---|
| [キーバインド一覧](docs/keybindings.md) | 追加・変更したキーの一覧 |
| [C/C++ 開発環境](docs/cpp.md) | 補完・タグナビゲーション・tree-sitter の仕組み、GTAGS の作成 |
| [パッケージ管理](docs/packages.md) | パッケージの更新、アーカイブからの復元 |
| [ファイル構成](docs/architecture.md) | ファイルと設定モジュールの配置 |
| [テスト](docs/testing.md) | 設定変更後の検証 |

## セットアップ

`emacs-setup.sh` の `--setup` は Linux（Debian / Ubuntu）向けで、`sudo` と `apt` が必要。
リポジトリは `~/.emacs.d` に置く（スクリプトはこのパスを固定で参照する）。
`--setup` は Emacs のビルド依存と、[外部ツール](#外部ツール)のうち「入れ方」が `--setup` のものを導入する。

```sh
git clone <このリポジトリ> ~/.emacs.d
cd ~/.emacs.d

export PATH="$HOME/.local/bin:$PATH"   # シェル設定にも追記する（clangd と emacs の導入先）
./emacs-setup.sh --setup               # ビルド依存と外部ツール
./emacs-setup.sh --install 31.1        # Emacs を ~/.local へビルド・導入。31.1 以上が既にあれば不要
./emacs-setup.sh --setup-treesit       # C/C++ の tree-sitter 文法（任意）
emacs -nw                              # 初回は straight がパッケージを取得・ビルドするため時間がかかる
```

- PATH の追記は `--setup` より前に済ませる。`--setup` は clangd を導入した後に PATH 上の `clangd` の版を検査し、見つからないと失敗する。
- clangd は GitHub から取得する Linux x86_64 専用の配布物で、ほかのアーキテクチャや GitHub へ到達できない環境では `--setup` がここで止まる。
- `--gui` は `--setup` と `--install` に付けられる。`--setup --gui no` は GUI 依存を省く。`--install` では GUI バックエンドを
  `gtk3`（既定）/ `lucid` / `pgtk`（Wayland）/ `no`（端末専用）から選ぶ。`--setup` を `--gui no` にしたなら `--install` も `--gui no` にする。
- tree-sitter 対応の Emacs と `git` が既にある環境では、`--setup` が C/C++ の文法も導入する。
- アクションの一覧、短縮形、ダウンロード元を変える環境変数は `./emacs-setup.sh --help` を参照。

### 外部ツール

無くても設定は動き、該当する機能だけが無効になるか代替へ切り替わる。

| ツール | 入れ方 | 無いとき |
|---|---|---|
| clangd | `--setup`（公式の最新安定版を `~/.local/share/clangd/` へ導入。更新は `--setup` の再実行） | C/C++ の補完は irony、それも無ければ cape + GNU Global |
| GNU Global（`global` / `gtags`） | `--setup` | `C-t` のタグ検索は eglot 管理下の LSP 経路だけになり、`C-t v` / `C-t f` は使えない |
| cmigemo | `--setup`。辞書は `/usr/share/cmigemo/utf-8/migemo-dict`（別の場所なら `loads/inits/99-private.el` で `migemo-dictionary` を設定） | ローマ字での日本語検索が無効 |
| Mozc（`mozc-server` / `emacs-mozc-bin`） | `--setup` | Emacs 内の日本語入力が無効 |
| ripgrep | `--setup` | `C-x g` が `grep` になる |
| hunspell / aspell | `--setup` | スペルチェックが無効 |
| pandoc | `--setup` | Markdown のプレビューが無効 |
| xclip | `sudo apt install xclip`（`--setup` に含まれない） | 端末でシステムクリップボードを共有できない |
| tree-sitter 文法（C/C++） | `--setup-treesit`。tree-sitter 対応の Emacs（`--install` で作ったものは対応）、`git`、C コンパイラが必要（[詳細](docs/cpp.md#文法の導入と-cc-mode-への固定)） | cc-mode で動く |
| Node.js | `--setup-node`（fnm / nvm で導入済みなら不要） | 既定の `auto` では Copilot 関連を読み込まない |
| Copilot language server | Node.js 導入後に Emacs を再起動し、`M-x copilot-install-server` → `M-x copilot-login` | `copilot-mode` が有効化されない |
| irony-server | `M-x irony-install-server`（`cmake` と libclang は `--setup` に含まれる） | cape + GNU Global で補完 |
| Nerd Font | `M-x nerd-icons-install-fonts` | GUI のアイコン表示だけに影響。端末では使わない |

`--setup-node` は `~/.local/downloads/node/` に公式の `node-v<版>-linux-<x64|arm64|armv7l>.tar.xz` があればそれを使い
（`python3` 3.9 以降が必要）、無ければ fnm 経由で取得する。`--uninstall-node` はオフライン導入分と、このスクリプトが導入した fnm を
配下のバージョンごと削除する。既存の fnm で入れた Node.js は削除せず、手順を案内する。

### GitHub へ到達できない環境

初回起動時のパッケージ取得は GitHub を必要とする。到達できない環境では、到達できる環境で作ったアーカイブから復元する。
アーカイブに入るのは作成元で実際に読み込まれたパッケージだけなので、復元先と同じ条件（端末か GUI か、`DISPLAY` と `xclip` の有無、
Node.js の有無）で一度起動してから作る。足りないパッケージがあると、復元時のリビルドが取得を試みて失敗する。
clangd、tree-sitter 文法、Node.js、Copilot language server はアーカイブに含まれず、無ければ該当する機能が無効になるだけで設定は動く。

```sh
# 到達できる環境で（一度起動してパッケージを揃えた後）
./emacs-setup.sh --packing-package     # loads/straight/repos と lockfile を ~/.emacs.d/package.tar.gz へ

# 到達できない環境で
#   1. package.tar.gz を ~/.emacs.d/ に置く
#   2. Emacs 31.1 以上を PATH に用意する（--extract-package は emacs コマンドを使う）
#   3. Copilot のパッケージが入っていないアーカイブを Node.js のある環境へ展開するなら、
#      custom/custom.el に (setq my/copilot-enabled nil) を書く（不足分の取得でリビルドが失敗するため）
./emacs-setup.sh --extract-package     # 展開して全パッケージをリビルドする
emacs -nw
```

失敗したときの復旧は [アーカイブと復元](docs/packages.md#アーカイブと復元) を参照。

## 使い方

追加・変更したキーの一覧は [キーバインド一覧](docs/keybindings.md)。`C-t`、`C-q`、`C-z`、`C-c j`、`C-c J` などの
プレフィックスを押すと which-key が候補を出す。`C-h` はバックスペースに割り当ててあり、ヘルプは `M-?`。

キーを覚えていないときは **`C-c h` を押して待つ**。日本語の操作ヘルプが出る。
`C-c h s` で「保存」「検索」などの目的、または `save-buffer` などの英語名を入力すると、
代表操作と現在のショートカットを選べる。`C-n` / `C-p` で選択し、`RET` で実行、`C-g` で中止する。
`C-c h h` は基本操作ガイド、`C-c h k` は押したキーの説明。
詳しい使い方は [覚えずに操作する](docs/keybindings.md#覚えずに操作する) を参照。

| キー | 動作 |
|---|---|
| `C-s` | 現在編集中の本文を検索。未保存の変更も対象（Migemo があればローマ字で日本語も引ける） |
| `M-s l` | 同じプロジェクトで開いているバッファの本文をまとめて検索（`C-u` で全バッファ） |
| `C-x g` | プロジェクト内の保存済みファイルの本文を検索。開いていないファイルも対象（`rg` があれば `.gitignore` を尊重。無ければ `grep`） |
| `C-x b` / `C-x C-r` | バッファ切り替え / 最近使ったファイル |
| `C-.` | 指定行へ移動 |
| `M-a` | 候補やカーソル位置に対するアクション（embark） |
| `M-p` | 他のウィンドウへ移動。1 枚なら分割 |
| `C-z n` / `C-z p` / `C-z f` / `C-z k` | 次のタブ / 前のタブ / タブを新規作成 / 閉じる |
| `<f8>` | ファイルツリー（neotree） |
| `C-q` → `n` / `p` / `a` | 複数カーソル（続けて押せる） |
| `C-,` | 選択範囲を段階的に広げる |
| `<f3>` / `<f4>` | カーソル位置と同じ名前を強調表示 / 全解除 |
| `C-x u` | undo をツリー表示（vundo） |
| `C-\` / `<f5>` | ブロックの折りたたみ |
| `C-t d` / `C-t u` | C/C++ の関数・変数・型などについて、定義されている場所 / 名前を使っている場所を探す |
| `C-x G` | Magit |

画面下部のコマンド名・検索語などを入力する欄（ミニバッファ）では、Vertico が候補を表示する。
スペース区切りの複数条件で絞り込める（Orderless）。例えば `read file` で、両方の語を含む候補を探せる。ファイル名補完は別の方式を使う。
本文の入力中は Corfu が、1 文字目から補完候補を自動で出す。候補は `C-n` / `C-p` で選んでから `TAB` か `RET` で確定する
（候補を選ぶ前の `RET` は候補を閉じるだけなので、改行には 2 回押す）。

### 端末での制約

- `C-.`、`C-,`、`C-c C-/` は、Ctrl と記号の組み合わせを区別して送れる端末（xterm の modifyOtherKeys や CSI u 相当）が前提。
- システムクリップボードの共有は、環境変数 `DISPLAY` と `xclip` がそろったときだけ有効になる。
- `C-z` はタブ操作のプレフィックスなので、Emacs のサスペンドは `C-x C-z`。

### C/C++

`C-t` は C/C++ バッファで使うタグナビゲーションと読解補助のプレフィックス。
入力補完は clangd（eglot）→ irony → cape + GNU Global の順に使えるものを選ぶ。
定義場所・使用箇所を探す `C-t d` / `C-t u` は、条件に応じて clangd（eglot）と GNU Global を使い分ける。
ほかの `C-t` コマンドには eglot 管理下でのみ使えるものもある。
GNU Global を使うプロジェクトでは、初回だけ `M-x ggtags-create-tags` で検索用の索引（GTAGS）を作る。
仕組みと操作は [C/C++ 開発環境](docs/cpp.md)、キーは [キーバインド一覧](docs/keybindings.md#cc-タグナビゲーション)。

### 編集・保存の挙動

- 30 秒操作が無いとファイルを自動保存し、全フレームがフォーカスを失ったときも保存する。
- 明示的に保存すると行末の空白を削除する（Markdown を除く）。
- undo 履歴は Emacs を終了しても残る。

### 日本語

- Linux では Mozc で入力する。GUI では `変換` で ON、`無変換` で OFF、`半角/全角` でトグル。
  端末にはこれらのキーが届かないため、`M-x toggle-input-method` で切り替えるか、端末側の IME を使う。
- Migemo により `C-s` やミニバッファの絞り込みでローマ字のまま日本語を引ける。

### Markdown / Org

- Markdown の HTML 出力は `C-c C-c e`。[mdh](https://github.com/ryoheiii/markdown-to-html) が PATH 上にあれば、保存してから同じフォルダーへ単一 HTML を生成する。mdh の導入はリンク先の手順を参照。
  mdh 未導入時は従来の Pandoc へフォールバックする。`C-u C-c C-c e` でも Pandoc を選べる。mdh の実行失敗は `*mdh*` に表示する。
- Markdown のプレビューは `C-c C-c p`（pandoc で HTML を生成。見た目は [custom/README.md](custom/README.md#css-と-js) の CSS / JS で変える）。
- Org の TODO は `TODO` / `IN-PROGRESS` / `WAITING` / `DONE` / `CANCELLED` の 5 段階で、完了時に `CLOSED:` の時刻が付く。

### GitHub Copilot

Node.js と Copilot language server を導入して `M-x copilot-login` で認証すると、`prog-mode` のバッファでインライン補完が動く
（導入手順は [外部ツール](#外部ツール)）。`C-c j` が補完、`C-c J` が Chat のプレフィックス
（全キーは [キーバインド一覧](docs/keybindings.md#github-copilot)）。有効・無効の切り替えは [Copilot の有効と無効](#copilot-の有効と無効)。

## 環境ごとの設定

| ファイル | 用途 | Git |
|---|---|---|
| `custom/custom.el` | `M-x customize` の保存先。手で `setq` を書いてもよく、設定モジュールより先に読み込まれる | gitignored |
| `loads/inits/99-private.el` | この環境だけのパッケージ宣言や設定。最後に読み込まれる | gitignored |
| `custom/snippets/` | 個人用の yasnippet スニペット（[custom/README.md](custom/README.md#snippets)） | gitignored |

`custom/custom.el` で効くのは `my/copilot-enabled` のような切り替え用の変数。設定モジュールが `use-package` の `:custom` で
値を与えている変数（`migemo-dictionary` など）は起動時に上書きされるため、`loads/inits/99-private.el` に `setq` で書く。

### Copilot の有効と無効

有効・無効は `my/copilot-enabled` で決め、`custom/custom.el`（または `M-x customize-variable`）で設定する。
反映には Emacs の再起動が必要。今のバッファでだけ補完を止めるには `C-c j m`。

| 値 | 動作 |
|---|---|
| `auto`（既定） | Node.js が見つかる環境でだけ有効 |
| `t` | 常に有効 |
| `nil` | 完全に無効。パッケージの取得もしない |

`M-x my/copilot-toggle` で `auto` と `nil` を切り替えて `custom/custom.el` へ保存する。
GitHub へ到達できない環境では、[復元手順](#github-へ到達できない環境) の 3 のとおり初回起動の前に `nil` にする。

### tree-sitter（C/C++）

文法を導入すると、次の起動から C/C++ が `c-ts-mode` / `c++-ts-mode` で開く。
cc-mode に戻す方法は [C/C++ 開発環境](docs/cpp.md#文法の導入と-cc-mode-への固定)。

## メンテナンス

- Emacs の更新は `./emacs-setup.sh --uninstall` の後に `--install <版>`（ソースツリーが残っていると `--install` が拒否する）。
- パッケージの更新と lockfile、アーカイブからの復元は [パッケージ管理](docs/packages.md)。
- 設定を変更した後の検証は [テスト](docs/testing.md)。変更するときの規約は [AGENTS.md](AGENTS.md)。

### 保存先の整理

実行時データは `var/state/`、`var/cache/`、`var/backup/`、`var/packages/`、`var/tmp/` に分ける。
旧配置から更新するときは、同じ設定を使う Emacs をすべて終了してから新しい設定を起動する。
起動時に旧履歴やサーバーを移し、直下の `.cache/copilot/` と `eln-cache/` も `var/` 配下へ収める。
同名ファイルの衝突は自動で上書きしない。詳しくは [保存先と移行手順](docs/architecture.md#実行時データと旧配置からの移行)。

### クリーンアップ

Emacs を終了してから実行する。キャッシュだけの掃除には `--clean-cache` を使う。

```sh
./emacs-setup.sh --clean-cache       # var/cache/ と var/tmp/ だけを削除（履歴・バックアップ・導入物は保持）
./emacs-setup.sh --clean             # var/ 配下と、このスクリプトが入れた clangd の旧版を削除（straight のパッケージは残す）
./emacs-setup.sh --clean-all         # 上記に加えて loads/straight/（パッケージ）と、このスクリプトが入れた clangd を全版削除
./emacs-setup.sh --uninstall-clangd  # このスクリプトが入れた clangd だけを削除
./emacs-setup.sh --uninstall-node    # オフライン導入分と、このスクリプトが入れた fnm を丸ごと削除（既存の fnm 経由の Node.js は残す）
```

`--clean` は次の復元できないデータも消す。バックアップ世代と自動保存、ミニバッファ履歴、カーソル位置、最近使ったファイル、
ブックマーク、undo 履歴、プロジェクト一覧、保存したチャット。tree-sitter 文法、Copilot language server、irony-server も消えるため、使うなら再導入する。
`--clean-all` は Git 管理の lockfile `loads/straight/versions/default.el` も消す。パッケージは起動時に lockfile のリビジョンで
取得されるため、Emacs を起動する前に `git checkout -- loads/straight/versions/default.el` で戻す。
