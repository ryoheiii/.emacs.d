<!-- -*- gfm -*- -->
# キーバインド一覧

この設定で追加・変更したキーバインドをまとめる。Emacs 標準のままのものは載せない。
端末（`emacs -nw`）での利用を前提にする。端末で届かないキーの条件は README の [端末での制約](../README.md#端末での制約) を参照。

`C-` は Ctrl、`M-` は Alt（端末では `ESC` を押して離してから次のキーでも可）。`C-t d` は Ctrl+t の後に d を押す。
`C-t` のような後続キーを待つキーをプレフィックスと呼ぶ。そこまで押せば which-key が続きの候補を表示する。
`C-u` は次のコマンドの動作を変える前置操作で、効果は各説明を参照。未使用のキーを探すときは `M-x free-keys`。

## プレフィックス

| プレフィックス | 用途 | 範囲 |
|---|---|---|
| `C-q` | 複数カーソル（`repeat-mode` で連続実行できる） | 全バッファ |
| `C-z` | タブ操作（Emacs のサスペンドは `C-x C-z`） | 全バッファ |
| `C-t` | C/C++ のタグナビゲーションと読解補助 | C/C++ バッファ |
| `C-c C-v` | Markdown の挿入 | Markdown バッファ |
| `C-c j` | Copilot のインライン補完 | Copilot 有効時 |
| `C-c J` | Copilot Chat | Copilot 有効時 |

## 基本操作

| キー | コマンド | 説明 |
|---|---|---|
| `C-h` | `DEL` | バックスペースに変換。ヘルプは `M-?` |
| `M-?` | `help-for-help` | ヘルプ |
| `C-m` | `smart-newline` | 改行してインデントと位置を自動調整（C/C++・Emacs Lisp・Lisp） |
| `C-c a` | `align` | 選択範囲の代入記号などを、言語ごとの規則に従って縦にそろえる |
| `C-c M-a` | `align-regexp` | 選択範囲で、入力した正規表現に一致する記号などの位置を縦にそろえる |
| `C-c d` | `delete-indentation` | 現在行を前の行につなぎ、境目の空白を調整する。範囲を選択していれば範囲内の行を連結 |
| `C-c ;` | `comment-or-uncomment-region` | コメントアウトの切り替え |
| `C-c C-i` | `hippie-expand` | 入力途中の単語やファイル名などを補う。続けて押すと次の候補へ切り替える |
| `C-c 0` | `my/copy-file-name` | 現在のファイル名（ディレクトリを除く）をコピー履歴へ入れる。OS のクリップボードとの共有条件は [端末での制約](../README.md#端末での制約) を参照 |
| `M-f` | `forward-symbol` | 関数名・変数名など、ひとまとまりの名前単位で前進 |
| `C-\` / `<f5>` | `hs-toggle-hiding` | ブロックの折りたたみ（C/C++・Emacs Lisp・Lisp） |
| `<f9>` | `my/outline-cycle-dwim` | カーソル位置の見出しの折りたたみを切り替える。C/C++ の ts モード（`c-ts-mode` / `c++-ts-mode`）では関数単位、Markdown では見出し単位。outline が無効なバッファでは `hs-toggle-hiding` |
| `C-u <f9>` | `my/outline-cycle-dwim` | outline が有効なバッファでは、バッファ全体の折りたたみを切り替える（無効なら `hs-toggle-hiding`） |
| `<f7>` | `toggle-truncate-lines` | 行の折り返しの切り替え |
| `C-x u` | `vundo` | undo をツリー表示（`C-f` / `C-b` で前後、`C-n` / `C-p` で分岐） |

## 選択・シンボル操作

ここでいうシンボルは、`read_data` のようなひとまとまりの名前。
以下の置換は、指定範囲内で同じ綴りの名前を置き換える。別の関数・クラスに属する同名変数も、範囲内にあれば置換対象になる。

| キー | コマンド | 説明 |
|---|---|---|
| `C-,` | `er/expand-region` | 選択範囲を段階的に広げる |
| `C-M-p` / `C-M-n` | `move-text-up` / `move-text-down` | 行を上下へ移動 |
| `<f3>` | `symbol-overlay-put` | カーソル位置と同じ名前を強調表示する。同じ名前で再実行すると解除 |
| `<f4>` | `symbol-overlay-remove-all` | ハイライトを全解除 |
| `C-x C-a` | `my-symbol-overlay-rename-visible` | 現在のウィンドウに表示している範囲で、カーソル位置と同じ名前を置換 |
| `C-x a` | `my-symbol-overlay-rename-in-function` | カーソルがある関数・メソッド内で同じ名前を置換（C/C++・Python。ほかのモードではバッファ全体） |
| `C-x C-g` | `symbol-overlay-rename` | 現在のバッファ全体で、カーソル位置と同じ名前を置換 |

## 複数カーソル（`C-q`）

表のキーは `C-q` に続けて押す（例: `C-q n`）。2 回目以降は `repeat-mode` により `C-q` を省き、`n` `n` … と続けて押せる。

| キー | コマンド | 説明 |
|---|---|---|
| `n` / `C-n` | `mc/mark-next-like-this` | 次の一致箇所にカーソルを追加 |
| `p` / `C-p` | `mc/mark-previous-like-this` | 前の一致箇所にカーソルを追加 |
| `a` / `C-a` / `*` | `mc/mark-all-like-this` | すべての一致箇所にカーソルを追加 |
| `d` / `C-d` | `mc/mark-all-like-this-dwim` | 関数の中なら関数内、外ならバッファ全体の一致箇所にカーソルを追加 |
| `m` / `C-m` | `mc/mark-more-like-this-extended` | 矢印キーでカーソルを追加・削除する対話操作 |
| `u` / `C-u` | `mc/unmark-next-like-this` | 次方向に最後に追加したカーソルを外す |
| `U` | `mc/unmark-previous-like-this` | 前方向に最後に追加したカーソルを外す |
| `s` / `C-s` | `mc/skip-to-next-like-this` | 現在の箇所を飛ばして次の一致箇所へ |
| `S` | `mc/skip-to-previous-like-this` | 現在の箇所を飛ばして前の一致箇所へ |
| `i` / `C-i` | `mc/insert-numbers` | 各カーソルに連番を挿入 |
| `l` / `C-l` | `mc/insert-letters` | 各カーソルに連続する文字を挿入 |
| `o` / `C-o` | `mc/sort-regions` | 各カーソルの選択範囲を並べ替え |
| `O` | `mc/reverse-regions` | 各カーソルの選択範囲を逆順に |
| `C-q`（`C-q C-q`） | `quoted-insert` | 標準の `C-q`。ここで連続入力は終わる |

## ウィンドウ・タブ・ファイルツリー

バッファは Emacs が保持する編集中の内容で、未保存の変更も含む。ウィンドウはバッファを表示する画面の分割領域。
タブはウィンドウの分割配置を切り替えるもので、1 ファイルにつき 1 タブではない。

| キー | コマンド | 説明 |
|---|---|---|
| `M-p` | `my/other-window-or-split` | 他ウィンドウへ移動。1 枚なら分割（幅 270 桁以上なら 3 分割） |
| `C-c C-r` | `my/window-resizer` | ウィンドウサイズ調整（`f` / `b` / `n` / `p`、他のキーで終了） |
| `S-<left>` / `S-<right>` / `S-<up>` / `S-<down>` | `windmove-left` / `windmove-right` / `windmove-up` / `windmove-down` | 矢印方向のウィンドウへ移動 |
| `<f8>` | `my/neotree-project-toggle` | ファイルツリーをプロジェクトルートで開閉 |
| `C-c i` | `imenu-list-smart-toggle` | 現在のバッファの関数・クラスなどの一覧を別ウィンドウで開閉する |
| `C-z n` / `C-z C-n` | `tab-next` | 次のタブ |
| `C-z p` / `C-z C-p` | `tab-previous` | 前のタブ |
| `C-z f` / `C-z C-f` | `tab-new` | タブを新規作成 |
| `C-z k` / `C-z C-k` | `tab-close` | タブを閉じる |
| `C-z 1`〜`C-z 9` | `tab-bar-select-tab` | 番号のタブへ移動 |

## 検索・移動・補完

| キー | コマンド | 説明 |
|---|---|---|
| `C-s` | `consult-line` | 現在のバッファの本文から、一致する行を探す（未保存の編集も対象） |
| `M-s l` | `my/consult-line-multi` | 同じプロジェクトで開いているバッファの本文をまとめて検索（1 文字から開始。`C-u` でプロジェクト外を含む全バッファ） |
| `C-.` | `consult-goto-line` | 指定行へ移動 |
| `C-x g` | `my/consult-ripgrep-or-grep` | プロジェクト内の保存済みファイルの本文を検索。開いていないファイルも対象（`rg` があれば `.gitignore` を尊重。無ければ `grep`） |
| `C-c g` | `grep` | `grep -nr` をそのまま実行（`.gitignore` で除外したファイルも検索する） |
| `C-x b` | `consult-buffer` | バッファ切り替え |
| `C-x f` | `consult-find` | ファイル名・パスを検索して開く。本文は検索しない |
| `C-x C-r` | `consult-recent-file` | 最近使ったファイル |
| `C-x C-y` | `consult-yank-pop` | コピー・切り取りの履歴（kill-ring）から選んで貼り付け |
| `C-x i` | `consult-imenu` | 現在のバッファの関数・クラスなどを名前で絞り込み、その場所へ移動 |
| `M-g o` | `consult-outline` | 見出しへジャンプ（C/C++ の ts モードでは関数定義） |
| `M-a` | `embark-act` | 補完候補やカーソル位置の対象に対して実行できる操作を選ぶ |
| `C-M-i` | `completion-at-point` | 手動で補完を起動 |
| `C-l` | `vertico-directory-delete-char` | ファイル名補完で直前が `/` のとき、ディレクトリを 1 階層戻る（それ以外は 1 文字削除） |
| `C-c y` | `consult-yasnippet` | コードや文章のひな型（スニペット）を一覧から選んで挿入 |

Corfu の候補が出ている間は `C-n` / `C-p` で候補を選び、`TAB` か `RET` で確定する。候補を選ぶ前の `TAB` と `RET` は
候補を閉じるだけで、改行には `RET` を 2 回押す。この間の `C-v` / `M-v` は候補一覧のページ送り（`C-s` は前のページ）、
`M-t` は説明の表示の切り替え、`C-M-v` / `M-<prior>` は説明を前後へスクロール。
インデント済みの行では `TAB` が補完も起動する（`tab-always-indent` が `complete`）。

Migemo はミニバッファの絞り込みと `C-s` で効く。ファイル名の補完と、外部コマンドへ正規表現を渡す `C-x g` / `C-c g` では効かない。

## C/C++ タグナビゲーション

C/C++ バッファで使う。バックエンドが eglot でも GNU Global でもキー操作は同じ。
2 段階の検索操作と GTAGS の作成は [C/C++ 開発環境のタグナビゲーション](cpp.md#タグナビゲーション) を参照。
`d` / `u` / `c` / `e` の違いは [4 つの検索の使い分け](cpp.md#4-つの検索の使い分け) を参照。

| キー | コマンド | 説明 |
|---|---|---|
| `C-t d` / `C-t C-d` | `my/gtags-find-definition` | 関数・変数・型などの定義場所を探す。関数の実装やクラスの構造を読む |
| `C-t u` / `C-t C-u` | `my/gtags-find-references` | 関数・変数・型などの名前を使っている場所を探す（参照）。メンバー変数の読み書きや、関数をコールバックとして渡す箇所を調べる |
| `C-t v` / `C-t C-v` | `my/gtags-find-symbol` | GTAGS に定義のないシンボルの出現箇所を検索。外部ライブラリの関数など、プロジェクト内に定義がない名前の利用例を探す |
| `C-t f` / `C-t C-f` | `my/gtags-find-file` | ファイル名で検索。`#include` に書かれたヘッダや、名前が分かっている実装ファイルを開く |
| `C-t p` / `C-t C-p` | `xref-go-back` | ジャンプ履歴を戻る。関数の実装を読み終えたら、調査を始めた呼び出し箇所へ戻る |
| `C-t n` / `C-t C-n` | `xref-go-forward` | ジャンプ履歴を進む。戻ったあと、直前に読んでいた実装をもう一度確認する |

### 読解補助（`C-t` 配下の追加キー）

eglot（clangd）が接続しているバッファでは、コードの解析結果を使う。
管理外では `c` は参照検索、`o` はファイル名などによるヘッダ/ソース切替、`s` は GNU Global のソース本文検索に切り替わる。
`e` / `t` / `l` は eglot 管理下でのみ使える。
キーごとの違いは [コードリーディング補助](cpp.md#コードリーディング補助) を参照。

| キー | コマンド | 説明 |
|---|---|---|
| `C-t c` / `C-t C-c` | `my/lsp-find-callers` | 対象の関数を呼び出す関数と、その呼び出し箇所を探す（関数の呼び出し元）。どの上位処理から呼ばれるかを調べる |
| `C-t e` / `C-t C-e` | `my/lsp-find-callees` | 対象の関数の中で呼び出している関数を一覧する（関数の呼び出し先、eglot 管理下のみ）。処理の委譲先から、次に読む関数を選ぶ |
| `C-t t` / `C-t C-t` | `my/lsp-find-type-hierarchy` | 対象のクラスが直接継承するクラス（基底）と、対象を直接継承するクラス（派生）を一覧（eglot 管理下のみ）。インターフェースの実装や共通処理を探す |
| `C-t o` / `C-t C-o` | `my/lsp-find-other-file` | ヘッダ/ソースの切り替え。クラスの宣言・コメントとメソッドの実装を見比べる |
| `C-t s` / `C-t C-s` | `my/lsp-find-workspace-symbol` | プロジェクト内の関数・クラスなどを名前の一部から探す。読む入口を見つける（非 LSP 時はソース本文を正規表現で検索） |
| `C-t l` / `C-t C-l` | `my/eglot-inlay-hints-toggle` | コードの横に引数名・推論された型を表示するヒント（inlay hints）を、現在のバッファで ON / OFF（eglot 管理下のみ）。`true` や数値の引数の意味、`auto` 変数の型を確認する |

### C/C++ バッファのその他のキー

| キー | コマンド | 説明 |
|---|---|---|
| `C-c c` | `compile` | コンパイル |
| `DEL` | `my/c-ts-hungry-delete-backward` | 直前の空白をまとめて削除（ts モードのみ） |

## Git

| キー | コマンド | 説明 |
|---|---|---|
| `C-x G` | `magit-status` | Magit を開く |
| `C-x M-g` | `magit-dispatch` | Magit のコマンドメニュー |
| `q`（magit-status） | `mu-magit-kill-buffers` | 現在のリポジトリの Magit バッファをすべて閉じ、ウィンドウ構成を戻す |
| `D` / `S`（magit-blame） | `difftastic-magit-show` | 構文差分（difftastic）で表示 |
| `D` / `S`（diff メニュー） | `difftastic-magit-diff` / `difftastic-magit-show` | 構文差分。Magit の diff メニューを初めて開いたときから利用できる |

## Markdown（markdown-mode）

| キー | コマンド | 説明 |
|---|---|---|
| `C-c C-c e` | `my-markdown-export` | mdh で HTML を出力。mdh 未導入時は従来の Pandoc へフォールバック |
| `C-u C-c C-c e` | `my-markdown-export` | 従来の Pandoc で HTML を出力 |
| `C-c C-v h` | `markdown-insert-header-dwim` | 見出しを挿入 |
| `C-c C-v l` | `markdown-insert-link` | リンクを挿入 |
| `C-c C-v c` | `markdown-insert-gfm-code-block` | コードブロックを挿入 |
| `C-c C-v d` | `markdown-insert-details` | 折りたたみ式の `<details>` を挿入 |
| `C-c C-v t` | `markdown-toc-generate-toc` | 目次を生成 |
| `C-c TAB` | `my-markdown-insert-tab` | 4 スペースを挿入。`C-c C-i` と同じキーのため、Markdown では `hippie-expand` より優先される |

mdh は未保存の変更を保存してから、Markdown と同じ場所へ同名の `.html` を出力する。
ファイル未指定のバッファは先に保存する。TRAMP のファイルは従来の変換を使用する。
mdh 自体が失敗した場合は `*mdh*` にログを表示する。必要なら `C-u C-c C-c e` で再実行する。
これらのキーは gfm-mode でも使用できる。

markdown-mode では `M-p` / `M-n` がリンク間の移動になり、ウィンドウ移動の `M-p` は使えない。

## Org（org-mode）

Org 標準のキーが優先されるため、`C-,`、`C-c C-r`、`C-c ;`、`C-c TAB`、`S-<矢印>` はこの設定の割り当てではなく Org の機能になる。

## スペルチェック

| キー | コマンド | 説明 |
|---|---|---|
| `C-c C-/` | `flyspell-correct-wrapper` | 修正候補を表示（flyspell が有効なバッファ） |

flyspell が有効になる範囲は次のとおり。

- prog-mode（Emacs Lisp を除く）: コメントと文字列だけ
- text 系のモード: Markdown だけ
- ファイルを開いた時点で 3000 文字を超えるバッファ: 無効

## 日本語入力

| キー | コマンド | 説明 |
|---|---|---|
| `<zenkaku-hankaku>`（半角/全角） | `toggle-input-method` | Mozc の ON / OFF を切り替え |
| `<henkan>`（変換） | `my/input-method-on` | Mozc を ON |
| `<muhenkan>`（無変換） | `my/input-method-off` | Mozc を OFF |

これらのキーは GUI でだけ Emacs に届く。端末（`emacs -nw`）では `M-x toggle-input-method` で Mozc を切り替えるか、
端末側の IME で入力する。標準の切り替えキー `C-\` は `hs-toggle-hiding` に割り当てたため使えない。

## GitHub Copilot

有効・無効の切り替えは README の [Copilot の有効と無効](../README.md#copilot-の有効と無効) を参照。

### インライン補完（`C-c j`）

`C-c j m` 以外は `copilot-mode` が ON のバッファで使える。

| キー | コマンド | 説明 |
|---|---|---|
| `C-c j m` | `copilot-mode` | Copilot の ON / OFF。バッファローカルなので押したバッファだけ切り替わる |
| `C-c j j` | `copilot-accept-completion` | 補完を確定 |
| `C-c j w` | `copilot-accept-completion-by-word` | 単語単位で確定 |
| `C-c j l` | `copilot-accept-completion-by-line` | 行単位で確定 |
| `C-c j n` / `C-c j p` | `copilot-next-completion` / `copilot-previous-completion` | 候補を切り替え |
| `C-c j c` | `copilot-complete` | 手動で補完を起動 |
| `C-c j s` | `copilot-panel-complete` | 複数候補をパネル表示 |
| `C-c j d` | `copilot-diagnose` | 接続状態を診断 |
| `C-c j e` | `copilot-select-completion-model` | 補完モデルを選択 |

### Chat（`C-c J`）

| キー | コマンド | 説明 |
|---|---|---|
| `C-c J J` / `C-c J h` | `copilot-chat-display` / `copilot-chat-hide` | チャットを開く / 隠す |
| `C-c J t` | `copilot-chat-transient` | メニューを開く |
| `C-c J e` | `copilot-chat-explain` | 選択範囲を説明 |
| `C-c J r` | `copilot-chat-review` | 選択範囲をレビュー |
| `C-c J f` | `copilot-chat-fix` | 選択範囲の修正案を出す |
| `C-c J o` | `copilot-chat-optimize` | 選択範囲の最適化案を出す |
| `C-c J T` | `copilot-chat-test` | 選択範囲のテストを生成 |
| `C-c J d` | `copilot-chat-doc` | 選択範囲のドキュメントを生成 |
| `C-c J E` | `copilot-chat-explain-defun` | カーソル位置の関数を説明（選択不要） |
| `C-c J R` | `copilot-chat-review-whole-buffer` | バッファ全体をレビュー |
| `C-c J p` / `C-c J P` | `copilot-chat-custom-prompt-selection` / `copilot-chat-custom-prompt-mini-buffer` | 任意の質問を送る（選択範囲を添える / 質問だけ） |
| `C-c J i` | `copilot-chat-ask-and-insert` | 回答をカーソル位置へ挿入 |
| `C-c J s` / `C-c J w` | `copilot-chat-send-to-buffer` / `copilot-chat-copy-code-at-point` | コードブロックを送る / コピー |
| `C-c J a` / `C-c J A` / `C-c J x` | `copilot-chat-add-current-buffer` / `copilot-chat-add-workspace` / `copilot-chat-del-current-buffer` | 現在のバッファをコンテキストへ追加 / ワークスペースを追加 / 現在のバッファを除去 |
| `C-c J l` | `copilot-chat-list` | コンテキスト一覧 |
| `C-c J g` | `copilot-chat-insert-commit-message` | コミットメッセージ生成 |
| `C-c J m` | `copilot-chat-set-model` | モデル切り替え |
| `C-c J c` / `C-c J q` | `copilot-chat-cancel` / `copilot-chat-reset` | 応答中断 / リセット |

回答は Org 形式・日本語で返る。

## その他

| キー | コマンド | 説明 |
|---|---|---|
| `<f6>` | `my/toggle-doom-theme` | doom-dracula と doom-nord-light を切り替える |
| `C-c l` | `rainbow-delimiters-using-stronger-colors` | 括弧の色をより強調する |
