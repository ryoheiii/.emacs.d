<!-- -*- gfm -*- -->
# キーバインド一覧

この設定で追加・変更したキーバインドをまとめる。Emacs 標準のままのものは載せない。
すべて端末（`emacs -nw`）で送出できるキーで構成している。

忘れたときは `C-c j` のようにプレフィックスまで押せば、which-key が候補を表示する。
未使用のキーを探すときは `M-x free-keys`。

## プレフィックスの割り当て

| プレフィックス | 用途 |
|---|---|
| `C-t` | C/C++ タグナビゲーション（**予約。変更しない**） |
| `C-q` | multiple-cursors（`repeat-mode` で連続実行できる） |
| `C-z` | tab-bar 操作 |
| `C-c j` | GitHub Copilot（インライン補完） |
| `C-c J` | GitHub Copilot Chat |
| `C-c C-v` | Markdown の挿入コマンド（markdown-mode 内） |

## 基本操作

| キー | コマンド | 説明 |
|---|---|---|
| `C-h` | `DEL` | バックスペースへ変換（`key-translation-map`。ヘルプは `M-?`） |
| `M-?` | `help-for-help` | ヘルプ |
| `C-m` | `smart-newline` | 改行（インデントと位置を自動調整） |
| `C-c a` | `align` | 文字列の揃え |
| `C-c M-a` | `align-regexp` | 正規表現での整列 |
| `C-c d` | `delete-indentation` | 行連結（インデント削除） |
| `C-c ;` | `comment-or-uncomment-region` | コメントアウト切り替え |
| `C-c C-i` | `hippie-expand` | 単純補完 |
| `C-c 0` | `my/copy-file-name` | ファイル名をクリップボードへ |
| `M-f` | `forward-symbol` | シンボル単位で前進 |
| `C-\` / `<f5>` | `hs-toggle-hiding` | コードの折りたたみ |
| `<f7>` | `toggle-truncate-lines` | 行の折り返し切り替え |
| `C-x u` | `vundo` | undo をツリー表示（`C-f`/`C-b` で前後、`C-n`/`C-p` で分岐） |

## ウィンドウ・タブ・ファイルツリー

| キー | コマンド | 説明 |
|---|---|---|
| `M-p` | `my/other-window-or-split` | 他ウィンドウへ移動。1 枚なら分割（幅 270 以上なら 3 分割） |
| `C-c C-r` | `my/window-resizer` | ウィンドウサイズ調整（`f`/`b`/`n`/`p`、他キーで終了） |
| `S-<arrow>` | `windmove` | 矢印方向のウィンドウへ移動 |
| `<f8>` | `my/neotree-project-toggle` | ファイルツリーをプロジェクトルートで開閉 |
| `C-c i` | `imenu-list-smart-toggle` | バッファ内シンボル一覧 |
| `C-z n` / `C-z C-n` | `tab-next` | 次のタブ |
| `C-z p` / `C-z C-p` | `tab-previous` | 前のタブ |
| `C-z f` / `C-z C-f` | `tab-new` | タブを新規作成 |
| `C-z k` / `C-z C-k` | `tab-close` | タブを閉じる |
| `C-z 1`〜`C-z 9` | `tab-bar-select-tab` | 番号のタブへ移動 |

## 検索・移動・補完

| キー | コマンド | 説明 |
|---|---|---|
| `C-s` | `consult-line` | 現在のバッファ内を検索 |
| `M-s l` | `my/consult-line-multi` | 全バッファ横断検索（1 文字から開始） |
| `C-.` | `consult-goto-line` | 指定行へ移動 |
| `C-x g` | `my/consult-ripgrep-or-grep` | プロジェクト検索（`rg` があれば ripgrep。`.gitignore` を尊重） |
| `C-c g` | `grep` | 生の `grep -nr`（ignore されたファイルも検索する） |
| `C-x b` | `consult-buffer` | バッファ切り替え |
| `C-x f` | `consult-find` | ファイル検索 |
| `C-x C-r` | `consult-recent-file` | 最近使ったファイル |
| `C-x C-y` | `consult-yank-pop` | kill-ring から貼り付け |
| `C-x i` | `consult-imenu` | バッファ内シンボルへジャンプ |
| `M-a` | `embark-act` | 補完候補・カーソル位置に対するアクション |
| `C-M-i` | `completion-at-point` | 手動で補完を起動 |
| `C-l` | `vertico-directory-delete-char` | ミニバッファでディレクトリを 1 階層戻る |
| `C-c y` | `consult-yasnippet` | スニペットを一覧から挿入 |

補完候補が出ている間（corfu）は `TAB` で確定、`C-n` / `C-p` で候補移動、`C-s` でスクロール。

> **`C-S` を使わない理由**: Emacs では `C-S` は `C-s` と同一のキーイベントであり
> （`(equal (kbd "C-s") (kbd "C-S"))` は `t`）、両方を `:bind` へ並べると後勝ちで
> 一方が到達不能になる。以前は `C-S` に割り当てた `my/consult-line-multi` が
> `C-s` を奪い、`consult-line` を呼ぶキーが存在しなかった（issue #10）。
> 横断検索は端末が送出できる `M-s l` へ置いている。
> この退行は `tests/my-test-keybindings.el` の
> `my-test-keybindings-consult-search-bindings` が固定する。

Migemo はミニバッファ補完の絞り込み（Orderless の matching style）で有効になる。
`C-x g` / `C-c g` は外部プロセスへ正規表現を渡すため Migemo は効かない。

## C/C++ タグナビゲーション

`C-t` プレフィックスは固定であり、バックエンド（eglot / GNU Global）が変わっても同じ操作で使える。
詳細は [cpp.md](cpp.md) を参照。

| キー | コマンド | 説明 |
|---|---|---|
| `C-t d` / `C-t C-d` | `my/gtags-find-definition` | 定義へジャンプ（`C-u` でシンボルを手入力） |
| `C-t u` / `C-t C-u` | `my/gtags-find-references` | 参照を検索 |
| `C-t v` / `C-t C-v` | `my/gtags-find-symbol` | シンボルの出現箇所を検索 |
| `C-t f` / `C-t C-f` | `my/gtags-find-file` | ファイル名で検索 |
| `C-t p` / `C-t C-p` | `xref-go-back` | ジャンプ履歴を戻る |
| `C-t n` / `C-t C-n` | `xref-go-forward` | ジャンプ履歴を進む |

C/C++ バッファでは `C-c c` が `compile` に割り当てられる。

GTAGS は初回だけ作成が必要である。`update-gtags` は `global -uv`（既存 DB の更新）
しか行わないため、GTAGS が無い状態では何も作られない。

| 状況 | 操作 |
|---|---|
| GTAGS 未作成 | C/C++ バッファで `M-x ggtags-create-tags`（またはシェルで `gtags`） |
| 作成済み・全体を更新 | C/C++ バッファで `M-x update-gtags` |
| 作成済み・保存時 | 自動更新（`ggtags-update-on-save`） |

## 複数カーソル（`C-q`）

`C-q` を押した後は `repeat-mode` により続けてキーだけで操作できる。

| キー | コマンド |
|---|---|
| `n` / `C-n` | `mc/mark-next-like-this` |
| `p` / `C-p` | `mc/mark-previous-like-this` |
| `a` / `C-a` / `*` | `mc/mark-all-like-this` |
| `d` / `C-d` | `mc/mark-all-like-this-dwim` |
| `m` / `C-m` | `mc/mark-more-like-this-extended` |
| `u` / `C-u` | `mc/unmark-next-like-this` |
| `U` | `mc/unmark-previous-like-this` |
| `s` / `C-s` | `mc/skip-to-next-like-this` |
| `S` | `mc/skip-to-previous-like-this` |
| `i` / `C-i` | `mc/insert-numbers` |
| `l` / `C-l` | `mc/insert-letters` |
| `o` / `C-o` | `mc/sort-regions` |
| `O` | `mc/reverse-regions` |

## 選択・シンボル操作

| キー | コマンド | 説明 |
|---|---|---|
| `C-,` | `er/expand-region` | 選択範囲を段階的に広げる |
| `C-M-p` / `C-M-n` | `move-text-up` / `move-text-down` | 行を上下へ移動 |
| `<f3>` | `symbol-overlay-put` | カーソル位置のシンボルをハイライト |
| `<f4>` | `symbol-overlay-remove-all` | ハイライトを全解除 |
| `C-x C-a` | `my-symbol-overlay-rename-visible` | 画面内のシンボルを置換 |
| `C-x a` | `my-symbol-overlay-rename-in-function` | 関数内のシンボルを置換 |
| `C-x C-g` | `symbol-overlay-rename` | バッファ全体のシンボルを置換 |

## Git

| キー | コマンド | 説明 |
|---|---|---|
| `C-x G` | `magit-status` | Magit を開く（`q` でバッファを片付けて閉じる） |
| `C-x M-g` | `magit-dispatch` | Magit のコマンドメニュー |
| `D` / `S` | `difftastic-magit-diff` / `difftastic-magit-show` | Magit の diff メニューから構文差分 |

## Markdown（markdown-mode 内）

| キー | コマンド | 説明 |
|---|---|---|
| `C-c C-v h` | `markdown-insert-header-dwim` | 見出しを挿入 |
| `C-c C-v l` | `markdown-insert-link` | リンクを挿入 |
| `C-c C-v c` | `markdown-insert-gfm-code-block` | コードブロックを挿入 |
| `C-c C-v d` | `markdown-insert-details` | 折り畳み `<details>` を挿入 |
| `C-c C-v t` | `markdown-toc-generate-toc` | 目次を生成 |
| `C-c TAB` | `my-markdown-insert-tab` | 4 スペースを挿入 |

## 日本語入力

| キー | コマンド | 説明 |
|---|---|---|
| `<zenkaku-hankaku>` | `toggle-input-method` | IME 切り替え |
| `<henkan>` | `my/input-method-on` | IME を ON |
| `<muhenkan>` | `my/input-method-off` | IME を OFF |

## スペルチェック

| キー | コマンド | 説明 |
|---|---|---|
| `C-c C-/` | `flyspell-correct-wrapper` | 修正候補を表示 |

flyspell は prog-mode ではコメント・文字列のみ（`flyspell-prog-mode`）、
text 系モードでは無効、3000 文字を超えるバッファでは自動的に無効になる。

## GitHub Copilot

Copilot が有効な環境でのみ有効になる。設定は [../README.md](../README.md) の
「環境ごとの切り替え」を参照。

### インライン補完（`C-c j`）

| キー | コマンド | 説明 |
|---|---|---|
| `C-c j m` | `copilot-mode` | Copilot の ON/OFF。キー自体はグローバルに置いてあり OFF 時も押せるが、`copilot-mode` はバッファローカルなので効果は押したバッファだけ |
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
| `C-c J f` | `copilot-chat-fix` | 修正案 |
| `C-c J o` | `copilot-chat-optimize` | 最適化案 |
| `C-c J T` | `copilot-chat-test` | テスト生成 |
| `C-c J d` | `copilot-chat-doc` | ドキュメント生成 |
| `C-c J E` | `copilot-chat-explain-defun` | カーソル位置の関数を説明（選択不要） |
| `C-c J R` | `copilot-chat-review-whole-buffer` | バッファ全体をレビュー |
| `C-c J p` / `C-c J P` | `copilot-chat-custom-prompt-selection` / `...-mini-buffer` | 自由入力（選択あり / ミニバッファ） |
| `C-c J i` | `copilot-chat-ask-and-insert` | 回答をカーソル位置へ挿入 |
| `C-c J s` / `C-c J w` | `copilot-chat-send-to-buffer` / `copilot-chat-copy-code-at-point` | コードブロックを送る / コピー |
| `C-c J a` / `C-c J A` / `C-c J x` | バッファ追加 / ワークスペース追加 / 除去 | コンテキスト管理 |
| `C-c J l` | `copilot-chat-list` | コンテキスト一覧 |
| `C-c J g` | `copilot-chat-insert-commit-message` | コミットメッセージ生成 |
| `C-c J m` | `copilot-chat-set-model` | モデル切り替え |
| `C-c J c` / `C-c J q` | `copilot-chat-cancel` / `copilot-chat-reset` | 応答中断 / リセット |

## その他

| キー | コマンド | 説明 |
|---|---|---|
| `<f6>` | `my/toggle-doom-theme` | doom-dracula ↔ doom-nord-light を切り替え |
| `C-c l` | `rainbow-delimiters-using-stronger-colors` | 括弧の色をより強調する |
