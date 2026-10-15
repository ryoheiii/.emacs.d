<!-- -*- gfm -*- -->
# C/C++ の段階構成

セットアップの自由度が低い環境（社内環境など）でも動くよう、C/C++ 関連は
**使える機構を自動選択し、使えなければ 1 段下へフォールバックする**構成にしている。
どの段でも `C-t` タグナビゲーションと補完 UI（Corfu）は同じように使える。

関連ファイル: `loads/inits/19-language-modes.el`、`loads/inits/18-built-in-package.el`、
`loads/inits/31-editing.el`、`loads/inits/32-navigation.el`、`loads/site-elisp/my-gtags.el`、
`loads/site-elisp/my-lsp-nav.el`

## メジャーモードの選択

| 条件 | 使われるモード |
|---|---|
| tree-sitter 文法（`c` / `cpp`）が導入済み | `c-ts-mode` / `c++-ts-mode` |
| 文法が無い、または `my/use-treesit-for-cc` が `nil` | `c-mode` / `c++-mode`（cc-mode + google-c-style） |

- 判定は `c` と `cpp` で独立して行う。片方だけ導入した環境でも壊れない。
- `.log` / `.cfg`（ログ閲覧用に c-mode を流用）と `.nut`（Squirrel）は C/C++ ではないため、
  文法が導入済みでも cc-mode のままにする。ts へ回すとバッファ全体が ERROR ノードになり
  フォントロックを失うためである。
- ts モードのインデントは google-c-style 相当（offset 4、namespace 非インデント、
  アクセス指定子は半段、case は 1 段）へ揃えてある。
  標準の K&R を選び、モードフックで差分を `treesit-simple-indent-override-rules` へ置く。
  上書き規則は標準規則より先に評価され、`c-ts-mode-set-style` で標準規則を作り直しても残る。
  ベース規則の取得に Emacs 内部関数は使わない。
- `c-toggle-auto-hungry-state` は ts モードに存在しないため、組み込み機能で再現している。
  - 自動改行: `electric-layout-mode` で
    `{` の後 / `}` の前後 / 文末の `;` の後 / アクセス指定子の `:` の後 に改行する。
    行途中（後ろに空白以外が残る）と `(` の内側（`for` の区切り）では改行しない
    （`c-hanging-semi&comma-criteria` 相当）。
    `}` の次行が `;` / `else` / `while` / `catch` のとき、および `{}` が空のときは
    1 行へ戻す（`c-cleanup-list` の `defun-close-semi` / `brace-else-brace` /
    `brace-elseif-brace` / `brace-catch-brace` / `empty-defun-braces` 相当）。
    `}` の次行が `,` のときも直付けする（`list-close-comma` 相当）。
  - hungry delete: DEL を `backward-delete-char-untabify`（`'all`）へ差し替える。
    コメント・文字列の中と前置引数付きでは cc-mode と同じ通常削除に戻る
    （`kill-ring` は使わない）。リージョン選択中は選択範囲を削除する。
  - `scope-operator` 相当: アクセス指定子の改行で割れた `::` を繋ぎ直す。
    `public` がマクロや名前空間名のとき `public::X` が書けるため、
    `:` の自動改行が `::` を割ることがある。
  - 上記の cleanup はいずれも **行末でだけ**働く。point の後ろに空白以外が残る
    場合は何もしない（cc-mode と同じ。既存行の途中へ挿入したときに前の行を
    巻き込まないため）。
  - **意図的な差分**: ブレース初期化リストの `{` を次行へ送らない。
    cc-mode は `int a[] = \n{\n    1, 2\n};` にする。これは google-c-style が
    `c-hanging-braces-alist` に `brace-list-open` を持たず cc-mode の既定
    （前後に改行）へ落ちた結果である。Google C++ スタイルガイドは同一行の
    `int a[] = {1, 2};` を示しつつ、折り返す場合に `{` の前で改行する形も
    許しているため、どちらも style 準拠になる。ts 側は同一行を選ぶ。
    このため入れ子の初期化は `},{` と続く（cc-mode は `},` で改行する）。
- 波括弧が 2 段以上開いている入力途中は tree-sitter が木全体を `ERROR` に落とし、
  既定の規則が桁 0 へ倒す。この間の桁は括弧の深さから算出して補う
  （`namespace` の波括弧は `(innamespace . 0)` に合わせて段数へ数えない）。
- **既知の使用感差分**: 上記 `ERROR` 状態の間だけ、波括弧に現れない段が 1 段
  浅くなる。該当するのは `case` ラベル配下の文（cc-mode は `case` から 1 段
  下げる）と、波括弧を省いた本体が解析できないとき（`for` で確認）である。
  構文が揃えば、上記のブレース初期化の `{` 配置を除いて cc-mode + google-c-style
  と一致する。

文法の導入は `./emacs-setup.sh --setup-treesit`（`git` と C コンパイラが必要）。
`--setup` も、tree-sitter 有効ビルドの Emacs がすでにある環境では併せて導入する。
Emacs からは `M-x my/treesit-install-c-grammars`（C/C++ をまとめて導入）か、
標準の `M-x treesit-install-language-grammar` を使う。どちらも同じ固定タグと
同じ導入先を使う（レシピは `loads/site-elisp/my-treesit.el` が正本）。

導入先は `var/package/tree-sitter/` で、`~/.emacs.d/tree-sitter/` は使わない。
自動ではインストールしないため、実行しない限り cc-mode のまま動く。

`C-u M-x my/treesit-install-c-grammars` で導入済みでも再ビルドする。
一時的に cc-mode へ戻したい場合は `my/use-treesit-for-cc` を `nil` にする（要再起動）。

## 補完バックエンドの選択

| 段 | 条件 | 使われる補完 |
|---|---|---|
| 1 | `clangd` があり `compile_commands.json` または `.clangd` を持つ | eglot（LSP） |
| 2 | 1 が不成立で `irony-server` が導入済み | irony |
| 3 | どちらも無い | cape（dabbrev / keyword / file）+ ggtags |

- eglot の自動起動は C/C++ の実ソース拡張子（`.c` `.cc` `.C` `.cpp` `.cxx`
  `.h` `.hh` `.hpp` `.hxx`）に限る。`.log` / `.cfg` では起動しない。
- `compile_commands.json` は直上ディレクトリ群に加えて `build/compile_commands.json` も探す。
- `.clangd` だけがある（CDB が無い）プロジェクトも意図的に自動起動の対象とする。
- clangd の起動引数は `--background-index --header-insertion=never
  --header-insertion-decorators=0 --limit-references=0`。`--header-insertion=never` は
  補完確定時の `#include` 自動挿入を止めるために必須である。`--limit-references=0` は
  参照検索（`C-t u` / `C-t c` の LSP 経路）の既定上限 1000 件を外す。`--limit-results` は
  コード補完の候補数上限（100）も兼ねるため変えない。
- `eglot-extend-to-xref` を有効にしている。xref で飛んだプロジェクト外のファイル
  （`/usr/include` など）も同じサーバーの管理下に入り、移動先でも `C-t` の LSP 経路が使える。
  CDB を持つ別プロジェクトのファイルへ飛んだときは新しいサーバーを立てず、元のサーバーを使う。
- 診断（flymake）は既定で無効にしている（`eglot-stay-out-of`）。
- inlay hints（引数名・推論型のヒント）は既定で無効にしている
  （`eglot-ignored-server-capabilities`）。`C-t l` でバッファ単位に ON にできる
  （[コードリーディング補助](#コードリーディング補助) を参照）。
- eglot 管理下のバッファでは irony を自動的に止める（CAPF の競合防止）。
- `irony-server` が未導入の環境では irony をロードしない。導入先は `var/hist/irony/`。

  導入手順（`cmake` と libclang が必要）:

  ```text
  M-x irony-install-server
  ```

  `irony-install-server` は irony 本体に autoload cookie が無く、かつ irony は
  「サーバー導入済みの環境でだけロードする」ゲートを通るため、そのままでは未導入の
  環境から呼べない。`use-package irony` の `:commands` で autoload を張って
  `M-x` から直接到達できるようにしている。
  導入後は C/C++ バッファを開き直すと `irony-mode` が有効になる。

## タグナビゲーション

キーバインドは [keybindings.md](keybindings.md) の「C/C++ タグナビゲーション」を参照。
`C-t` プレフィックスは不変条件であり、バックエンドが変わっても変更しない。

`C-t d/u/v/f`（`C-t C-d/C-u/C-v/C-f` も同じ）は、次の 2 段階で検索する。

1. ミニバッファで名前を補完する。カーソル位置のシンボル名・ファイル名が初期入力になり、
   GNU Global の候補を Vertico で表示する。Orderless による途中一致と、スペース区切りの
   複数語で絞り込める。
2. 検索結果から移動先を選ぶ。結果が 1 件でも選択を待ち、`RET` でジャンプする。
   複数件は Consult のプレビュー付き、1 件はファイル名・行番号・本文の表示のみとなる。

候補は `C-n` / `C-p` で移動、`TAB` で入力欄へ挿入、`RET` で確定する。
`C-g` は検索を中止する。名前入力で候補にない名前や正規表現を使う場合は、
`M-RET`（端末では `ESC RET` でも可）で入力そのものを確定できる。
`C-t v` は GNU Global の `-s` に対応し、GTAGS に定義のないシンボルの出現箇所を検索する。

定義・参照検索（`d/u`）の委譲は次の順で決まる（`loads/site-elisp/my-gtags.el`）。

1. eglot 管理下で、初期入力のシンボル名を変えずに確定する → `xref`（LSP）で検索する。
2. 名前を変更した場合、LSP が見つけられない場合、非 LSP 環境 → GNU Global で検索する。

`C-u C-t d` / `C-u C-t u` は GNU Global を指定して検索する。
GTAGS や `global` コマンドがない環境でも、LSP 検索では元のシンボル名を候補にして検索を続けられる。
候補の取得に失敗した場合も診断を残し、LSP での検索を続ける。
GNU Global は `call-process` で直接実行し、ggtags の xref バックエンド・プロセス管理は経由しない。

GTAGS は初回だけ作成が必要である。`update-gtags` は `global -uv` を実行するだけで、
GTAGS が無い状態では何も作られない（`global` が終了コード 3 で失敗する）。
`ggtags-mode` の有効化も自動生成は行わない。

| 状況 | 操作 |
|---|---|
| GTAGS 未作成 | C/C++ バッファで `M-x ggtags-create-tags`（またはシェルで `gtags`） |
| 作成済み・全体を更新 | C/C++ バッファで `M-x update-gtags` |
| 作成済み・保存時 | 自動更新（`ggtags-update-on-save`） |

`update-gtags` は ggtags のロード後に定義されるため、C/C++ バッファ以外からは呼べない。

### 呼び出し階層・型階層・ヘッダ/ソース切替・横断シンボル検索

`C-t` 配下の追加キー（固定表の対象外）。実装は `loads/site-elisp/my-lsp-nav.el`。
eglot 管理下では clangd へ直接問い合わせ、結果は `C-t d/u` と同じ Consult の選択 UI で
表示する（1 件でも `RET` で選択、`C-t p` で戻れる）。Emacs 31 の eglot には
`eglot-show-call-hierarchy` / `eglot-show-type-hierarchy`（専用バッファのツリー表示）も
あるが、`C-t` の操作感と非 LSP 環境へのフォールバックを揃えるため使っていない。
多段をツリーで眺めたいときはそれらを `M-x` で呼ぶ。

| キー | LSP 管理下 | 非 LSP 環境 |
|---|---|---|
| `C-t c` 呼び出し元 | `callHierarchy/incomingCalls`。呼び出し箇所ごとに 1 件、要約は `呼び出し元関数名: 行` | `C-t u` と同じ参照検索へ委譲。`C-u` は GNU Global 強制 |
| `C-t e` 呼び出し先 | `callHierarchy/outgoingCalls`。定義名の位置へ着地 | エラー（代替なし） |
| `C-t t` 型階層 | `typeHierarchy/supertypes` と `subtypes` を両方取得し `[基底]` / `[派生]` を前置 | エラー（代替なし） |
| `C-t o` ヘッダ/ソース | clangd 拡張 `textDocument/switchSourceHeader`（`src/` と `include/` のような別ディレクトリの対も見つかる）。対が無ければ ff へ | `ff-find-other-file`（同じディレクトリなどを探す。見つからなくても新規ファイルは作らない）。`C-u` で常に ff |
| `C-t s` シンボル検索 | `workspace/symbol`（名前のあいまい一致） | GTAGS があれば `global --grep`（正規表現）。無ければエラー |

- 階層は直接の 1 段だけを表示する。次の段は移動先で再実行する。
- clangd 18 は `callHierarchy/outgoingCalls` を実装していない（LLVM 20 で追加）。
  この版では `C-t e` が「clangd 20 以降が必要」のエラーになる。
- `C-t s` は `xref-find-apropos` を経由しない。ggtags の xref バックエンドは
  カーソル位置の語が GTAGS に無いと選ばれず Global へ届かないため、
  Global 経路はバックエンドを指定して `xref-backend-apropos` を直接呼ぶ。LSP 経路は
  eglot の xref バックエンドも使わず（内部の要求ラッパーがタイムアウトを無期限にするため）、
  `workspace/symbol` を有限タイムアウトの要求経路で自前で送る。
- `C-t s` の入力は名前の補完なしの自由文字列。LSP 経路はファジー一致、Global 経路は
  正規表現で意味が違う。
- 要求には `jsonrpc-default-request-timeout`（10 秒）を明示している。eglot 自身の要求
  ラッパーはタイムアウトを無効化したまま転送するため、使わずに `jsonrpc-request` を
  直接呼ぶ。タイムアウトしたときや、サーバーがメソッドを持たないときは原因を含む
  `user-error` になる。
- clangd がある環境では `make test-cpp-config` が実 clangd を起動し、要求層をスタブせずに
  これらの経路と inlay hints、breadcrumb を通す（`my-test-cpp-config-lsp-live-clangd`。
  clangd が無い CI では skip）。

## コードリーディング補助

既定の見た目と入力の感触は変えず、追加キーで読解用の機能を使えるようにしている。

### 関数単位の折りたたみと一覧（outline）

- `c-ts-mode` / `c++-ts-mode` でだけ `outline-minor-mode` を有効にする。見出しは
  tree-sitter の関数定義ノード（`class` / `namespace` は見出しにならない）。
  cc-mode の既定は `outline-regexp "[^#\n\^M]"`（`#` 以外の全行が見出し）で
  実用にならないため、cc-mode では現状（hideshow のみ）を維持する。
  4 モードのフック parity 規約の意図的な例外で、`tests/my-test-cpp-config.el` の
  `my-test-cpp-config-outline-hook-ts-only` が固定する。
- 見出し判定の述語は `my/c-ts-outline-predicate`（関数定義ノード全体）に差し替えている。
  31.1 の既定述語は定義行でも動くが `namespace_definition` / `class_specifier` も
  見出しにするため、`<f9>` と `consult-outline` の見出しを関数定義だけに保つ目的で使う。
- `<f9>` は本体の中からでも見出しへ移動して折りたたみを切り替える。`C-u <f9>` は
  バッファ全体（`outline-cycle-buffer`）。見出しのレベルはすべて 1 なので、3 状態のうち
  「上位見出しのみ」と「全見出し」は同じ見え方になる。
- `M-g o`（`consult-outline`）は関数定義の一覧からプレビュー付きでジャンプする。
- hideshow（`<f5>`、内側のブロック、`invisible 'hs`）と outline（`<f9>`、関数、
  `invisible 'outline`）は独立した overlay 層で、両方で隠した領域は片方の show では
  開かない。
- `outline-minor-mode-cycle`（TAB の乗っ取り）と `outline-minor-mode-highlight` は
  既定の nil のまま。

### inlay hints のトグル

`C-t l`（`my/eglot-inlay-hints-toggle`）で引数名・推論型のヒントをバッファ単位に ON/OFF する。
既定は `eglot-ignored-server-capabilities` の `:inlayHintProvider` で常時 OFF。
ON にするときはバッファローカルに `:inlayHintProvider` を外してから
`eglot-inlay-hints-mode` を有効化し、OFF ではローカル値を捨てて既定へ戻す。
ローカル値は ON の間だけ残るため、`eglot-reconnect` 後の自動有効化でも ON が維持される。
eglot 未管理のバッファでは `user-error`。

### header-line のパス表示（breadcrumb）

C/C++ の 4 モードで `breadcrumb-local-mode` を有効にし、header-line にプロジェクト内パスと
imenu のパスを表示する。他モードの既定の見た目（header-line なし）は変えず、
`which-function-mode`（モードライン）も維持する。情報源はモードで異なる。

| 状態 | パスの精度 |
|---|---|
| eglot 管理下 | eglot-imenu が範囲付きの index を返すため正確（`namespace > Class > method`） |
| ts モードのみ | `treesit-simple-imenu` は位置しか持たないため「point 以前の最後のリーフ」。カテゴリ名（`Function` など）も先頭に出る |
| cc-mode | 正規表現の imenu（`imenu-auto-rescan-maxout` を超えると再走査しない） |

更新は編集後 `breadcrumb-idle-time`（1 秒）の idle timer で imenu を再走査する。
表示は文字と face だけで tty でも動く。
依存の `project` は組み込み版で足りるため、`straight-override-recipe` で built-in に固定する
（[packages.md](packages.md) を参照）。

## 環境ごとの必要物

| 段 | 必要なもの | 導入方法 |
|---|---|---|
| tree-sitter モード | `git`、C コンパイラ | `./emacs-setup.sh --setup-treesit`（`M-x my/treesit-install-c-grammars` でも可） |
| eglot | `clangd`、CDB か `.clangd` | `./emacs-setup.sh --setup`（`clangd` パッケージ。`clang` とは別パッケージ）。CDB はビルド系で生成する |
| irony | `cmake`、libclang | `./emacs-setup.sh --setup` の後、`M-x irony-install-server` |
| ggtags | GNU Global | `./emacs-setup.sh --setup`（`global` パッケージ） |

## テスト

C/C++ 設定の不変条件は `make test-cpp-config` が固定する。
検証内容と注意点は [testing.md](testing.md) を参照。
