<!-- -*- gfm -*- -->
# Emacs 設定

端末（`emacs -nw`）での日常利用を第一に作った、モジュール構成の Emacs 設定。
C/C++ 開発、Markdown/Org でのドキュメント作成、Mozc/Migemo による日本語入力に対応する。

- **主用途は `emacs -nw`。** GUI でも動くが、判断に迷う場面では tty 側を優先している。
- **環境が揃わなくても動く。** tree-sitter・clangd・Node.js・Copilot などは
  すべて任意で、無ければ自動的に 1 段下の機構へフォールバックする。
- **パッケージは [straight.el](https://github.com/radian-software/straight.el)** で管理する
  （`package.el` は不使用）。バージョンは lockfile で固定する。

| | |
|---|---|
| 対象 Emacs | 30.x 以上 |
| 対象 OS | Linux (Debian/Ubuntu) / WSL2。macOS は設定のみ対応（`emacs-setup.sh` は Linux 前提） |
| 詳細ドキュメント | [キーバインド一覧](docs/keybindings.md) ・ [アーキテクチャ](docs/architecture.md) ・ [C/C++ の段階構成](docs/cpp.md) ・ [パッケージ管理](docs/packages.md) ・ [テスト](docs/testing.md) |

---

## 1. クイックスタート

```sh
git clone <このリポジトリ> ~/.emacs.d
cd ~/.emacs.d

./emacs-setup.sh --setup          # 依存パッケージを導入
./emacs-setup.sh --install 30.2   # Emacs をビルドして ~/.local へ導入

# ~/.local/bin は PATH へ追加されない。シェル設定へ追記するか、フルパスで起動する
export PATH="$HOME/.local/bin:$PATH"

./emacs-setup.sh --setup-treesit  # C/C++ の tree-sitter 文法を導入
emacs -nw                         # 初回起動（straight が全パッケージを取得する）
```

初回起動はパッケージの clone とビルドで数分かかる。2 回目以降は 2 秒程度で立ち上がる。

`--install` は `~/.local` へ入れるだけで PATH を変更しない。追記しないと
コマンドが見つからないか、既存の古い `/usr/bin/emacs` が起動する。

すでに Emacs 30.x が入っているなら `--install` は不要。

---

## 2. セットアップ

### 2.1 依存パッケージ

```sh
./emacs-setup.sh --setup

# GUI 依存を入れない（端末専用の環境向け）
./emacs-setup.sh --setup --gui no
```

Emacs のビルドに必要なライブラリに加えて、この設定が使う外部コマンド
（`clang` / `clangd` / `global` / `cmigemo` / `mozc-server` / `ripgrep` /
`hunspell` / `aspell` / `cmake` / `pandoc` / Ricty Diminished フォント）を
まとめて導入する。

tree-sitter 有効ビルドの Emacs がすでにある環境では、C/C++ の tree-sitter 文法も
併せて導入する。無ければスキップして案内を出すので、`--install` の後に
`./emacs-setup.sh --setup-treesit` を実行する。

`--gui` は `no` を指定したときだけ GUI パッケージ（X11 群、画像ライブラリなど）を
除外する。`gtk3` / `lucid` / `pgtk` を指定しても既定（すべて導入）と同じ結果になる。
TLS など GUI に依存しない依存は `--gui no` でも導入される。

### 2.2 Emacs のビルド

```sh
# インストール可能なバージョンを確認
./emacs-setup.sh --list

# ビルド・インストール（GUI バックエンド: gtk3, lucid, pgtk, no）
./emacs-setup.sh --install <バージョン> [--gui <バックエンド>]

# 例
./emacs-setup.sh --install 30.2 --gui pgtk   # Wayland 向け
./emacs-setup.sh --install 30.2 --gui no     # 端末専用

# アンインストール
./emacs-setup.sh --uninstall
```

`~/.local` へインストールする。native-compilation、JSON、tree-sitter、
動的モジュールを有効にしてビルドする。

ダウンロード元は環境変数で差し替えられる。

| 環境変数 | 既定値 |
|---|---|
| `EMACS_SETUP_MIRROR_URL` | `https://ftp.jaist.ac.jp/pub/GNU/emacs` |
| `EMACS_SETUP_UPSTREAM_URL` | `https://ftp.gnu.org/gnu/emacs`（ミラー失敗時のフォールバック） |
| `EMACS_SETUP_INDEX_URL` | `$EMACS_SETUP_MIRROR_URL/`（`--list` の取得先） |

### 2.3 任意コンポーネント

**どれも実行しなくても設定は動く。** 未導入なら自動的に代替へフォールバックする。
必要になった時点で入れればよい。

| 項目 | 手順 | 入れない場合 |
|---|---|---|
| tree-sitter 文法（C/C++） | `./emacs-setup.sh --setup-treesit`（`git` と C コンパイラが必要）。Emacs からは `M-x my/treesit-install-c-grammars` または `M-x treesit-install-language-grammar`。再起動で ts モードへ切り替わる | cc-mode で動作する |
| Node.js（Copilot 用） | `./emacs-setup.sh --setup-node` | Copilot 関連を一切読み込まない |
| Copilot language server | `M-x copilot-install-server` → `M-x copilot-login` | `copilot-mode` を有効化しない |
| irony サーバー（非 LSP 環境の補完） | `M-x irony-install-server`（`cmake` と libclang が必要） | cape + ggtags へフォールバックする |
| Nerd Font | `M-x nerd-icons-install-fonts` | GUI のアイコン表示のみ影響。tty では既定で無効 |
| Migemo 辞書 | `--setup` の `cmigemo` に同梱。パスは `/usr/share/cmigemo/utf-8/migemo-dict` | ローマ字での日本語検索が無効になる |

`clangd`（C/C++ の LSP）、`ripgrep`（`C-x g`）、Mozc（日本語入力）は
`--setup` に含まれる。個別に入れる場合は
`sudo apt install clangd ripgrep mozc-server emacs-mozc-bin` を実行する。

#### GitHub Copilot を使う場合

```sh
./emacs-setup.sh --setup-node   # Node.js を導入（オフライン tarball 優先、fnm フォールバック）
```

導入後、シェル設定へ PATH を追加する（コマンドの最後に案内が出る）。
Emacs 側は fnm / nvm / `~/.local/node/bin` を自動で探すため、
どの方法で入れても検出される。

Emacs を再起動してから、

```text
M-x copilot-install-server    ; language server を var/package/copilot/ へ導入
M-x copilot-login             ; GitHub アカウントを認証
```

を実行すると、`prog-mode` のバッファでインライン補完が有効になる。
操作は `C-c j`（補完）と `C-c J`（Chat）。詳細は
[キーバインド一覧](docs/keybindings.md#github-copilot) を参照。

Node.js を戻すときは `./emacs-setup.sh --uninstall-node`。
このスクリプトが入れた fnm だけを削除し、**もともと入っていた fnm には触れない**
（利用者の他の Node バージョンを消さないため）。その場合はスキップした旨を表示する。

Copilot を使えない環境での止め方は「[4. 環境ごとの切り替え](#4-環境ごとの切り替え)」を参照。

---

## 3. 使い方

キーバインドの全一覧は **[docs/keybindings.md](docs/keybindings.md)** にある。
ここではよく使うものだけを挙げる。

忘れたときは、プレフィックス（`C-t`、`C-q`、`C-z`、`C-c j`、`C-c J`）まで押せば
which-key が候補を出す。

### 3.1 移動と検索

| キー | 動作 |
|---|---|
| `C-s` | 現在のバッファ内を検索（Migemo があればローマ字で日本語も引ける） |
| `M-s l` | 全バッファ横断検索（1 文字から開始） |
| `C-x g` | プロジェクト検索（`rg` があれば ripgrep で `.gitignore` を尊重、無ければ `grep`） |
| `C-c g` | 生の `grep -nr`（ignore されたファイルも対象） |
| `C-x b` / `C-x C-r` | バッファ切り替え / 最近使ったファイル |
| `C-.` | 指定行へジャンプ |
| `M-a` | 候補やカーソル位置に対するアクション（embark） |

`C-h` は BackSpace に割り当てているので、ヘルプは `M-?` を使う。

> **`C-S` は使えない**: Emacs では `C-S` は `C-s` と同一のキーイベントであるため、
> 横断検索は `M-s l` へ置いている。詳細は
> [docs/keybindings.md](docs/keybindings.md) を参照。

`rg` は `--setup` で導入される。個別に入れる場合は `sudo apt install ripgrep`。

### 3.2 補完

- **ミニバッファ**は Vertico が縦に候補を出す。絞り込みは Orderless
  （スペース区切りで複数条件、Migemo によるローマ字検索対応）。
- **バッファ内**は Corfu が 1 文字目から自動で出る（`corfu-auto-prefix` の既定値は 2 だが、
  Orderless 連携が Corfu バッファで 1 へ上書きする）。`TAB` で確定、`C-n` / `C-p` で候補移動。
  端末では corfu-terminal がポップアップを描画する。
- 補完候補の供給元は状況で切り替わる（C/C++ は eglot → irony → cape の順）。
  詳細は [C/C++ の段階構成](docs/cpp.md)。

### 3.3 C/C++ 開発

タグナビゲーションは `C-t` プレフィックスに固定してある。
LSP が使える環境では LSP、無ければ GNU Global へ自動で委譲する。

| キー | 動作 |
|---|---|
| `C-t d` | 定義へジャンプ |
| `C-t u` | 参照を検索 |
| `C-t v` | シンボルの出現箇所を検索 |
| `C-t f` | ファイル名で検索 |
| `C-t p` / `C-t n` | ジャンプ履歴を戻る / 進む |
| `C-c c` | `compile` |

`compile_commands.json` か `.clangd` があり `clangd` が入っていれば、
C/C++ ファイルを開いた時点で eglot が自動起動する。無ければ irony、
それも無ければ cape + ggtags で補完する。どの段でも上記のキー操作は変わらない。

### 3.4 編集

| キー | 動作 |
|---|---|
| `C-q` → `n` / `p` / `a` | 複数カーソル（そのまま連続で押せる） |
| `C-,` | 選択範囲を段階的に広げる |
| `<f3>` / `<f4>` | シンボルをハイライト / 全解除 |
| `C-x C-a` / `C-x a` / `C-x C-g` | 画面内 / 関数内 / バッファ全体でシンボルを置換 |
| `C-M-p` / `C-M-n` | 行を上下へ移動 |
| `C-x u` | undo をツリー表示（vundo） |
| `C-c y` | スニペットを一覧から挿入（yasnippet） |
| `C-\` / `<f5>` | コードの折りたたみ |

undo 履歴は Emacs を終了しても保持される（undo-fu-session）。
バッファは 30 秒操作が無ければ自動保存され、フォーカスを失ったときにも保存される。

### 3.5 ウィンドウ・タブ・ファイルツリー

| キー | 動作 |
|---|---|
| `M-p` | 他ウィンドウへ移動。1 枚なら分割する |
| `C-c C-r` | ウィンドウサイズ調整（`f`/`b`/`n`/`p`） |
| `S-<arrow>` | 矢印方向のウィンドウへ移動 |
| `C-z n` / `C-z p` / `C-z f` / `C-z k` | タブの移動・作成・削除 |
| `<f8>` | ファイルツリー（neotree）をプロジェクトルートで開閉 |
| `C-c i` | バッファ内シンボル一覧 |

### 3.6 Git

`C-x G` で Magit を開く（`q` で閉じる）。変更行はフリンジ／マージンに表示される
（diff-hl）。diff メニューの `D` / `S` で difftastic による構文差分を見られる。

### 3.7 日本語入力

Linux では Mozc、Windows では TR-IME を使う。
`変換` キーで ON、`無変換` キーで OFF、`半角/全角` でトグルする。

straight が導入するのは `mozc.el`（Emacs 側）だけで、変換エンジンと helper は
別に要る。`--setup` で導入されるが、個別に入れる場合は次を実行する。

```sh
sudo apt install mozc-server emacs-mozc-bin
```

検索の絞り込みでは Migemo によりローマ字のまま日本語を引ける
（`C-s` や `M-x` などの補完で有効）。`C-x g` の ripgrep / grep は外部プロセスへ
正規表現を渡すため Migemo は効かない。

### 3.8 Markdown / Org

Markdown は保存時に行末空白を残す（ハードブレイク用）。`README.md` は GFM モードで開く。
`C-c C-v` プレフィックスに見出し・リンク・コードブロック・折り畳み・目次の挿入を割り当てている。
markdown-mode 標準の `C-c C-c p`（プレビュー）や `C-c C-c v`（エクスポートしてプレビュー）は
pandoc 経由で HTML を生成する（CSS/JS は `custom/css/`、`custom/js/`）。

Org は TODO を `TODO` / `IN-PROGRESS` / `WAITING` / `DONE` / `CANCELLED` の
5 段階にし、完了時刻を `:LOGBOOK:` に記録する。

### 3.9 GitHub Copilot

`C-c j` がインライン補完、`C-c J` が Chat。よく使うのは次の 4 つ。

| キー | 動作 |
|---|---|
| `C-c j j` | 表示中の補完を確定 |
| `C-c j m` | Copilot の ON/OFF（`copilot-mode` はバッファローカル。押したバッファだけ切り替わる） |
| `C-c J J` | Chat を開く |
| `C-c J e` | 選択範囲を説明させる |

Chat の回答は org 形式・日本語で返る。モデルは `C-c J m` で切り替える。

---

## 4. 環境ごとの切り替え

同じ設定を「何でも入れられる自宅」と「制限のある職場」の両方で使うことを想定している。
切り替えは Emacs 標準の customize で行い、保存先の `custom/custom.el` は gitignored なので
環境ごとに違う値を持てる。

### GitHub Copilot

Node.js が無い環境、GitHub へ到達できない環境、Copilot の契約が無い環境では、
**何もしなくてもよい。** 既定の `auto` が Node.js の有無を見て自動で判断する。

| 状態 | 動作 |
|---|---|
| Node.js が無い | Copilot 関連パッケージを取得も読み込みもしない |
| Node.js はあるが language server 未導入 | パッケージは読むが `copilot-mode` は有効化しない |
| 両方そろっている | `prog-mode` でインライン補完が動く |

明示的に止めたい場合:

```text
M-x my/copilot-toggle                     ; 有効 ⇔ 無効を切り替えて custom.el へ保存
M-x customize-variable RET my/copilot-enabled   ; auto / t / nil から選ぶ
```

| 値 | 意味 |
|---|---|
| `auto`（既定） | Node.js が見つかる環境でだけ有効にする |
| `t` | Node.js の有無に関わらず有効にする |
| `nil` | 完全に無効。パッケージの clone とビルドも行わない |

反映には Emacs の再起動が必要。**その場で補完だけ止めたいときは `C-c j m`** を使う。

### tree-sitter（C/C++）

文法を導入すると次回起動から `c-ts-mode` / `c++-ts-mode` に切り替わる。
導入は `./emacs-setup.sh --setup-treesit`（`--setup` でも前提が揃っていれば入る）。
一時的に従来の cc-mode へ戻すには `my/use-treesit-for-cc` を `nil` にする（要再起動）。
詳細は [docs/cpp.md](docs/cpp.md)。

### この環境だけの設定

| ファイル | 用途 | Git |
|---|---|---|
| `custom/custom.el` | `M-x customize` の保存先。手で `setq` を書いてもよい | gitignored |
| `loads/inits/99-private.el` | 環境固有のパッケージ宣言や設定 | gitignored |

`custom.el` は `early-init.el` の最初期に読み込まれるため、
他のすべての設定より先に値を決められる。

---

## 5. メンテナンス

### パッケージの更新

```text
M-x straight-pull-all         ; 全パッケージを更新
```

更新後は `make test` を実行し、問題なければ `M-x straight-freeze-versions` で
`loads/straight/versions/default.el` を更新してコミットする。
手順の詳細は [docs/packages.md](docs/packages.md)。

### バックアップと復元

```sh
./emacs-setup.sh --packing-package   # パッケージを package.tar.gz へ固める
./emacs-setup.sh --extract-package   # 展開してリビルドまで実行する
```

### クリーンアップ

```sh
./emacs-setup.sh --clean       # var/ 配下の生成物とユーザー操作履歴、および
                               # 直下へ迷い込んだ eln-cache/ を削除
                               # （symlink の場合は残す）
./emacs-setup.sh --clean-all   # 上記に加えてパッケージも削除
```

> **警告: `--clean` は復元できないデータを消す。**
>
> | 種別 | 例 |
> |---|---|
> | 再生成可能 | ネイティブコンパイルキャッシュ、`var/backup/` |
> | **復元不可能** | ミニバッファ履歴（savehist）、カーソル位置（places）、最近使ったファイル、undo 履歴（undo-fu-session） |

### テスト

```sh
make test          # フルスイート
make test-startup  # 起動だけを素早く確認
```

ターゲット一覧と変更範囲ごとの最小検証は [docs/testing.md](docs/testing.md)。

---

## 6. トラブルシューティング

| 症状 | 対処 |
|---|---|
| 端末で色が正しく出ない | `TERM=xterm-256color` を設定する |
| 起動時にエラーが出る | `make test-startup` でバッチ起動のエラーを確認する |
| パッケージが壊れた | `M-x straight-rebuild-all`。直らなければ `./emacs-setup.sh --clean-all` で再構築する |
| Emacs 外でパッケージを書き換えた後、古いビルドのままになる | `M-x straight-check-all`（[docs/packages.md](docs/packages.md) の変更検出の方式を参照） |
| Copilot がつながらない | `M-x copilot-diagnose`。`node` が PATH にあるか、`M-x copilot-login` 済みかを確認する |
| C/C++ で補完が効かない | `compile_commands.json` の有無と `clangd` の導入を確認する（`clangd` は `--setup` で入る。個別なら `sudo apt install clangd`） |
| 定義ジャンプが見つからない | GTAGS が未作成なら C/C++ バッファで `M-x ggtags-create-tags`。作成済みなら `M-x update-gtags` で更新する |
| `make lint` が失敗する | shellcheck を導入する（`sudo apt install shellcheck`） |

---

## 7. 設定を変更するとき

このリポジトリで作業する際の規約は次にまとめてある。

| ファイル | 内容 |
|---|---|
| `AGENTS.md` | 全体の判断原則、tty 優先の不変条件、Git 運用、禁止事項 |
| `.claude/rules/elisp-conventions.md` | `use-package` 規約、tty 対応、C/C++ フック parity、yasnippet の不変条件 |
| `.claude/rules/codebase-map.md` | ディレクトリと番号帯の責務、パスヘルパー |
| `.claude/rules/verification.md` | 変更範囲ごとに実行すべき検証 |
| `.claude/rules/git-workflow.md` | worktree 戦略、ブランチ命名、マージ手順 |
| `.claude/rules/commit-conventions.md` | コミットメッセージ形式 |
