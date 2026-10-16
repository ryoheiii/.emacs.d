<!-- -*- gfm -*- -->
# C/C++ 開発環境

C/C++ 関連の機能は、上の段から順に使えるものを自動で選ぶ。必要なツールが無ければ 1 段下へフォールバックする。
どの段でも `C-t` のタグナビゲーションと補完 UI（Corfu）は同じように使える。
clangd、GNU Global、irony-server の導入は README の [外部ツール](../README.md#外部ツール) を参照。

## メジャーモードの選択

| 条件 | モード |
|---|---|
| tree-sitter 文法（`c` / `cpp`）が導入済み | `c-ts-mode` / `c++-ts-mode`（以下 ts モード） |
| 文法が無い、または `my/use-treesit-for-cc` が `nil` | `c-mode` / `c++-mode`（cc-mode + google-c-style） |

- `c` と `cpp` は独立に判定する。片方の文法だけを導入した場合は、その言語だけが ts モードで開く。
- `.h` は C++ として開く。
- `.log` / `.cfg`（この設定ではログ閲覧用に `c-mode` を割り当てている）と `.nut`（Squirrel）は、文法があっても cc-mode のまま開く。
- cc-mode と同様に、ts モードでも google-c-style 相当のインデント（offset 4）、`{` `}` `;` と `public:` などの後の自動改行、
  `DEL` の hungry delete（直前の空白をまとめて削除）が効く。cc-mode との差分は次の 2 点。
  - ブレース初期化リストの `{` は、次行へ送らず同じ行に置く（意図的な差分）。
  - `{` を閉じていない入力途中は、一部の行が cc-mode より 1 段浅くなることがある。

### 文法の導入と cc-mode への固定

```sh
./emacs-setup.sh --setup-treesit     # tree-sitter 対応の Emacs、git、C コンパイラが必要
```

Emacs からは `M-x my/treesit-install-c-grammars`（`C-u` 付きで導入済みでも再ビルド）。
導入先は `var/package/tree-sitter/`。導入後に再起動すると ts モードへ切り替わる。

cc-mode を使い続けるには `custom/custom.el` に `(setq my/use-treesit-for-cc nil)` を書く（次の起動から効く）。
再起動せずに切り替えるには `M-: (setq my/use-treesit-for-cc nil)` を評価し、開いているバッファで `M-x normal-mode` を実行する。

## 補完バックエンドの選択

| 段 | 条件 | 補完 |
|---|---|---|
| 1 | `clangd` があり、プロジェクトに `compile_commands.json` または `.clangd` がある | eglot（LSP） |
| 2 | 1 が不成立で、irony-server が導入済み（`M-x irony-install-server`） | irony |
| 3 | どちらも無い | cape（dabbrev / keyword / file）+ GNU Global |

- 段 1 の条件を満たすプロジェクトでは、`.c` `.cc` `.C` `.cpp` `.cxx` `.h` `.hh` `.hpp` `.hxx` を開いた時点で eglot が自動起動する。
  `compile_commands.json` は上位ディレクトリと `build/` 配下から探す。この設定では生成しないので、CMake などのビルド系で作る。
- 補完を確定しても `#include` は自動挿入しない。
- 診断（flymake）は既定で無効。有効にするには `loads/inits/99-private.el` で `eglot-stay-out-of` から `flymake` を外す。
  inlay hints も既定で無効（[inlay hints](#inlay-hints)）。
- `C-t d` などで移動した先のプロジェクト外のファイル（`/usr/include` など）も同じ clangd の管理下に入り、移動先でも LSP で検索できる。
- irony-server の導入先は `var/hist/irony/`。導入後に C/C++ バッファを開き直すと irony が有効になる。eglot 管理下のバッファでは irony を自動的に止める。

## タグナビゲーション

キーは [キーバインド一覧の C/C++ タグナビゲーション](keybindings.md#cc-タグナビゲーション) を参照。

### GTAGS の作成と更新

GNU Global を使うには GTAGS が必要で、初回だけ手で作る。`M-x update-gtags` は既存の GTAGS を更新するだけで新しく作らない。

| 状況 | 操作 |
|---|---|
| GTAGS 未作成 | C/C++ バッファで `M-x ggtags-create-tags`（またはシェルで `gtags`） |
| 作成済み・全体を更新 | C/C++ バッファで `M-x update-gtags` |
| 作成済み・保存時 | 自動更新 |

### 検索の操作

`C-t d` / `C-t u` / `C-t v` / `C-t f` は 2 段階で検索する。

1. ミニバッファで名前を補完する。カーソル位置のシンボル名・ファイル名が初期入力になり、GNU Global の候補を
   Orderless（途中一致、スペース区切りの複数語）で絞り込める。`C-n` / `C-p` で候補移動、`TAB` で入力欄へ挿入、`RET` で確定。
   候補に無い名前や正規表現を使うときは `M-RET`（端末では `ESC RET`）で入力そのものを確定する。
2. 検索結果から移動先を選ぶ。結果が 1 件でも自動では移動せず、`RET` で確定する。複数件のときはプレビューが付く。

定義・参照（`C-t d` / `C-t u`）は、eglot 管理下でカーソル位置の名前をそのまま確定したときだけ LSP で検索する。
名前を変えたとき、LSP で見つからないとき、eglot 管理外のバッファでは GNU Global で検索する。
`C-u C-t d` / `C-u C-t u` は常に GNU Global で検索する。`C-t v` / `C-t f` は常に GNU Global を使う。

## コードリーディング補助

### 呼び出し階層・型階層・ヘッダ/ソース切替・横断シンボル検索

eglot 管理下では clangd に問い合わせる。`C-t c` / `C-t e` / `C-t t` / `C-t s` の結果は `C-t d` と同じ選択 UI に出し、
1 件でも自動では移動しない。`C-t o` は対のファイルを直接開く。どのキーも移動前の位置へ `C-t p` で戻れる。

| キー | eglot 管理下 | eglot 管理外 |
|---|---|---|
| `C-t c` 呼び出し元 | 呼び出し箇所ごとに 1 件 | `C-t u` と同じ参照検索 |
| `C-t e` 呼び出し先 | 呼び出し先の定義へ移動（clangd 20 以降） | エラー |
| `C-t t` 型階層 | 直接の基底クラス `[基底]` と派生クラス `[派生]` を一覧 | エラー |
| `C-t o` ヘッダ/ソース | clangd が判定した対のファイル（`src/` と `include/` のような別ディレクトリの対も見つかる）。無ければ `ff-find-other-file` | `ff-find-other-file`（新規ファイルは作らない） |
| `C-t s` シンボル検索 | 名前のあいまい一致 | GTAGS があれば GNU Global でソースの全行を検索（コメント・文字列も対象）。無ければエラー |

- `C-u` を付けると、eglot 管理下でも `C-t c` は GNU Global での参照検索、`C-t o` は `ff-find-other-file` を使う。
- 階層は直接つながる 1 階層だけを表示する。さらに辿るときは移動先で同じキーをもう一度押す。
  `C-t c` の移動先は呼び出し箇所なので、呼び出し元の関数名へカーソルを移してから押す。

### 関数単位の折りたたみと一覧

ts モードでは `<f9>` でカーソル位置の関数を折りたたみ、`C-u <f9>` でバッファ全体を切り替える。
`M-g o` で関数定義の一覧からプレビュー付きでジャンプする。cc-mode では `<f9>` は `<f5>` と同じブロックの折りたたみになり、
`M-g o` の一覧は関数単位にならない。

### inlay hints

`C-t l` で引数名・推論型のヒントをバッファ単位に ON / OFF する。既定は OFF。eglot 管理下のみ。

### header-line のパス表示

C/C++ バッファの header-line に、プロジェクト内のパスとシンボルのパス（`namespace > Class > method`）を表示する。
パスは eglot 管理下で最も正確になる。表示は文字だけなので、端末でもそのまま使える。
