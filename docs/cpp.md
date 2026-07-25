<!-- -*- gfm -*- -->
# C/C++ の段階構成

セットアップの自由度が低い環境（社内環境など）でも動くよう、C/C++ 関連は
**使える機構を自動選択し、使えなければ 1 段下へフォールバックする**構成にしている。
どの段でも `C-t` タグナビゲーションと補完 UI（Corfu）は同じように使える。

関連ファイル: `loads/inits/19-language-modes.el`、`loads/inits/18-built-in-package.el`、
`loads/inits/31-editing.el`、`loads/site-elisp/my-gtags.el`

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
- `c-toggle-auto-hungry-state` は ts モードに存在しないため、組み込み機能で再現している。
  - 自動改行: `electric-layout-mode` で
    `{` の後 / `}` の前後 / 文末の `;` の後 / アクセス指定子の `:` の後 に改行する。
    行途中（後ろに空白以外が残る）と `(` の内側（`for` の区切り）では改行しない
    （`c-hanging-semi&comma-criteria` 相当）。
    `}` の次行が `;` / `else` / `while` / `catch` のとき、および `{}` が空のときは
    1 行へ戻す（`c-cleanup-list` の `defun-close-semi` / `brace-else-brace` /
    `brace-elseif-brace` / `brace-catch-brace` / `empty-defun-braces` 相当）。
  - hungry delete: DEL を `backward-delete-char-untabify`（`'all`）へ差し替える。
    コメント・文字列の中と前置引数付きでは cc-mode と同じ通常削除に戻る
    （`kill-ring` は使わない）。リージョン選択中は選択範囲を削除する。
  - 未再現: `c-cleanup-list` の `list-close-comma` / `scope-operator`、
    ブレース初期化リストの `{` 前改行。
- **既知の使用感差分**: 波括弧が 2 段以上開いている入力途中は tree-sitter が木全体を
  `ERROR` に落とすため、インデントが概算になる（直前の行を基準にした近似で、
  既定の桁 0 よりは近い）。構文が揃えば cc-mode + google-c-style と完全に一致する。

文法の導入は `M-x my/treesit-install-c-grammars`（`git` と C コンパイラが必要）。
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
  --header-insertion-decorators=0`。`--header-insertion=never` は補完確定時の
  `#include` 自動挿入を止めるために必須である。
- 診断（flymake）は既定で無効にしている（`eglot-stay-out-of`）。
- eglot 管理下のバッファでは irony を自動的に止める（CAPF の競合防止）。
- `irony-server` が未導入の環境では irony をロードしない。導入先は `var/hist/irony/`。

  導入手順（`cmake` と libclang が必要）:

  ```text
  M-x load-library RET irony RET   ; irony 本体をロードする
  M-x irony-install-server
  ```

  `irony-install-server` には autoload cookie が無く、irony 本体がロードされるまで
  `M-x` から見えない。かつ irony は「サーバー導入済みの環境でだけロードする」ゲートを
  通るため、未導入の環境では自動ではロードされない。そのため 1 行目が必要になる。

## タグナビゲーション

キーバインドは [keybindings.md](keybindings.md) の「C/C++ タグナビゲーション」を参照。
`C-t` プレフィックスは不変条件であり、バックエンドが変わっても変更しない。

検索の委譲は次の順で決まる（`loads/site-elisp/my-gtags.el`）。

1. eglot 管理下のバッファでカーソル位置にシンボルがある → `xref`（LSP）で検索する。
2. LSP が見つけられない、または非 LSP 環境 → `global` コマンドを `call-process` で直接実行する。

`global` の結果は `consult-xref` → vertico で表示する。候補が 1 件なら直接ジャンプする。
ggtags の xref バックエンド・プロセス管理は経由しない（最短パスで結果を得るため）。

GTAGS はファイル保存時に更新される（`ggtags-update-on-save`）。
全体を作り直すときは、C/C++ バッファで `M-x update-gtags`
（`update-gtags` は ggtags のロード後に定義されるため、C/C++ バッファ以外からは呼べない）。

## 環境ごとの必要物

| 段 | 必要なもの | 導入方法 |
|---|---|---|
| tree-sitter モード | `git`、C コンパイラ | `M-x my/treesit-install-c-grammars` |
| eglot | `clangd`、CDB か `.clangd` | `./emacs-setup.sh --setup` で clang を導入。CDB はビルド系で生成する |
| irony | `cmake`、libclang | `./emacs-setup.sh --setup` の後 `M-x irony-install-server` |
| ggtags | GNU Global | `./emacs-setup.sh --setup`（`global` パッケージ） |

## テスト

C/C++ 設定の不変条件は `make test-cpp-config` が固定する。
検証内容と注意点は [testing.md](testing.md) を参照。
